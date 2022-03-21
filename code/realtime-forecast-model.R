library(tidyverse)
library(lubridate)
library(fable)
library(covidHubUtils)
library(covidData)

source("code/hosp_arima_forecast_function.R")

data_start_date <- ymd("2020-07-24")

## roughly start of winter wave in MA, also give us a few weeks of data to fit the model on early
forecast_date <- ymd("2021-05-03") 

## load hosp truth data
hosps <- load_truth(
  truth_source = "HealthData",
  target_variable = "inc hosp",
  locations = "25",
  data_location = "covidData",
  as_of = forecast_date) %>% 
  filter(target_end_date >= data_start_date) %>%
  select(target_end_date, hosps = value) %>% 
  as_tsibble(index = target_end_date) %>% 
  tsibble::fill_gaps()

test_case <- read_csv("csv-data/MA-DPH-covid-alldata.csv") %>% 
  rename(target_end_date = test_date) %>%
  filter(issue_date == forecast_date,
         target_end_date <= forecast_date, 
         target_end_date >= data_start_date) %>% 
  select(target_end_date, test_case = new_positive) %>% 
  mutate(test_case_smooth = slider::slide_dbl(test_case, mean, .before = 6, .after=0, .complete=FALSE),
         test_case_smooth_.25 = fourth_rt(test_case_smooth)) %>%
  as_tsibble(index = target_end_date) %>% 
  tsibble::fill_gaps() 


all_data <- hosps %>%
  left_join(test_case) %>%
  filter(target_end_date < forecast_date) %>%
  add_lags("hosps", p=7, P=1) %>%
  add_lags("test_case", p=7, P=1) %>%
  add_lags("test_case_smooth", p=7, P=1) 

my_models <- all_data %>%
  model(arima_.25 = ARIMA(fourth_rt_transformation(hosps)),
        hosp_lag1 = TSLM(fourth_rt_transformation(hosps) ~ hosps_lag1),
        hosp_lag1_rt = TSLM(fourth_rt_transformation(hosps) ~ fourth_rt(hosps_lag1)),
        hosp_lag7 = TSLM(fourth_rt_transformation(hosps) ~ hosps_lag1 + hosps_lag2 + hosps_lag3 + hosps_lag4 + hosps_lag5 + hosps_lag6 + hosps_lag7),
        hosp_lag7_rt = TSLM(fourth_rt_transformation(hosps) ~ fourth_rt(hosps_lag1) + fourth_rt(hosps_lag2) + fourth_rt(hosps_lag3) + fourth_rt(hosps_lag4) + fourth_rt(hosps_lag5) + fourth_rt(hosps_lag6) + fourth_rt(hosps_lag7)),
        hosp_lag11_rt = TSLM(fourth_rt_transformation(hosps) ~ fourth_rt(hosps_lag1) + fourth_rt(hosps_lag7) + fourth_rt(hosps_lag8)),
        tcfs_lag1 = TSLM(fourth_rt_transformation(hosps) ~ test_case_smooth_lag1),
        tcfs_lagx = TSLM(fourth_rt_transformation(hosps) ~ test_case_smooth_lag1 + test_case_smooth_lag2 + test_case_smooth_lag3 + test_case_smooth_lag4 + test_case_smooth_lag5 + test_case_smooth_lag6 + test_case_smooth_lag7),
        tcf_lag1 = TSLM(fourth_rt_transformation(hosps) ~ test_case_lag1),
        tcf_lag7 = TSLM(fourth_rt_transformation(hosps) ~ test_case_lag1 + test_case_lag2 + test_case_lag3 + test_case_lag4 + test_case_lag5 + test_case_lag6 + test_case_lag7),
        tcfs_lag11 = TSLM(fourth_rt_transformation(hosps) ~ fourth_rt(hosps_lag1) + fourth_rt(hosps_lag7) + fourth_rt(hosps_lag8) +
                                                          fourth_rt(test_case_smooth_lag1) + fourth_rt(test_case_smooth_lag7) + fourth_rt(test_case_smooth_lag8))
  )

my_models %>% select("arima_.25") %>% report()
glance(my_models)
accuracy(my_models)

forecast(select(my_models, "arima_.25"), h=28) %>%
  autoplot(all_data)

forecast(select(my_models, "tcfs_lag11"), tail(all_data,1))
  

## to generate a TSLM forecast with only lagged hosp data
# - initialize hosp_sims, an empty a n_sim x h matrix, where n_sim = k * l
# - for h=1
#    - generate n_sim simulations and insert into hosp_sims[,1]
# - for h > 1
#    - draw k rows from hosp_sims[,h-1] 
#    - generate k versions of the data, including recomputing all lagged covariates
#    - for each of the k samples, forecast 1 step ahead for hosps, simulating l draws for each
#    - the resulting k*l samples are stored in hosp_sims[,h]  




## draft script for running forecasts in validation phase
## March 2022

## load packages, scripts
library(tidyverse)
library(lubridate)
library(fable)
library(covidHubUtils)

# TODO: 
# - update data_issue_date after downloading more recent data, thru Omicron wave
# - download Monday versions of data from Oct 2020 - Dec 2020 and add to main test data csv file

## define global parameters
hosp_data_start_date <- ymd("2020-08-01")
case_data_start_date <- ymd("2020-03-01")
first_forecast_date <- ymd("2020-10-05") ## roughly start of winter wave in MA
last_forecast_date <- ymd("2021-06-07") ## roughly start of summer trough in MA
data_issue_date <- as.Date("2022-02-02") ## could update with newer data

## load hosp truth data, both sets of case data
hosp_truth <- load_truth(
  truth_source = "HealthData",
  target_variable = "inc hosp",
  locations = "25") %>% 
  filter(target_end_date <= forecast_date, target_end_date >= hosp_data_start_date)

report_date_case_truth <- load_truth(
  truth_source = "JHU",
  target_variable = "inc case",
  temporal_resolution = "daily",
  locations = "25") %>% 
  filter(target_end_date <= forecast_date, target_end_date >= case_data_start_date, value >= 0)

test_date_case_truth <- read.csv("../csv-data/MA-DPH-csvdata-covid-2022-02-02.csv") %>%  #chose latest issue date from csv files
  mutate(issue_date = data_issue_date) %>% 
  mutate(Date = substr(Date, 1, 10)) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Date <= forecast_date, Date >= case_data_start_date)

ts_hosp <- hosp_truth %>%
  as_tsibble() %>% 
  select(target_end_date, hosp_val = value) %>% 
  tsibble::fill_gaps()

ts_report_date_case <- report_date_case_truth %>% 
  select(target_end_date, report_case_val = value) %>%  #change variable names
  as_tsibble() %>% 
  tsibble::fill_gaps()

ts_test_date_case <- test_date_case_truth %>% 
  select(target_end_date = Date, test_case_val = Positive.New) %>% 
  as_tsibble() %>% 
  tsibble::fill_gaps() 

## loop through forecast dates
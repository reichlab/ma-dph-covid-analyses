## draft script for running forecasts in validation phase
## March 2022

## load packages, scripts
library(tidyverse)
library(lubridate)
library(fable)
library(covidHubUtils)

# TODO: 
# - update data_issue_date after downloading more recent data, thru Omicron wave
# - download Monday versions of data from Oct 2020 - Dec 2020 and add to main test data csv file, change first_forecast_date
# - add dependency of covidData so we can load_truth(as_of=...)
# - fill in missing datapoints?

## define global parameters
hosp_data_start_date <- ymd("2020-08-01")
case_data_start_date <- ymd("2020-03-01")
first_forecast_date <- ymd("2021-01-04") #ymd("2020-10-05") ## roughly start of winter wave in MA
last_forecast_date <- ymd("2021-06-07") ## roughly start of summer trough in MA
data_analysis_date <- as.Date("2022-03-14") ## could update with newer data

validation_forecast_dates <- seq.Date(first_forecast_date, last_forecast_date, by = "1 week")

## load hosp truth data
hosp_final <- load_truth(
  truth_source = "HealthData",
  target_variable = "inc hosp",
  locations = "25",
  as_of = data_analysis_date) %>% 
  filter(
    ## only need data until last forecast date
    target_end_date <= last_forecast_date, 
    ## data should start ~1wk prior to the first date, to allow for weekly differencing
    target_end_date >= hosp_data_start_date-8) %>%
  select(target_end_date, hosps = value) %>% 
  as_tsibble(index = target_end_date) %>% 
  tsibble::fill_gaps()

## load ReportCaseFinal data
report_case_final <- load_truth(
  truth_source = "JHU",
  target_variable = "inc case",
  temporal_resolution = "daily",
  locations = "25",
  as_of = data_analysis_date) %>% 
  filter(
    target_end_date <= last_forecast_date, 
    target_end_date >= case_data_start_date, 
    ## note: filtering out values with <0 reported case values
    value >= 0) %>% 
  select(target_end_date, report_cases = value) %>%  #change variable names
  as_tsibble(index = target_end_date) %>% 
  tsibble::fill_gaps()


## load TestCaseFinal data
test_case_final <- read_csv("csv-data/MA-DPH-covid-alldata.csv") %>%
  filter(issue_date == data_analysis_date) %>%
  mutate(target_end_date = test_date) %>% 
  filter(
    target_end_date <= last_forecast_date, 
    target_end_date >= case_data_start_date) %>% 
  select(target_end_date, test_case_final = new_positive, issue_date) %>% 
  as_tsibble(index = target_end_date) %>% 
  tsibble::fill_gaps() 


## loop through forecast dates

## for a forecastMonday
for(forecast_date in validation_forecast_dates){
  
  ##  - extract as-of data for TestCases 
  test_case_realtime <- read_csv("csv-data/MA-DPH-covid-alldata.csv") %>%  #chose latest issue date from csv files
    filter(issue_date == forecast_date) %>% 
    mutate(target_end_date = ymd(substr(Date, 1, 10))) %>% 
    filter(
      target_end_date <= forecast_date, 
      target_end_date >= case_data_start_date) %>% 
    select(target_end_date, test_case_realtime = Positive.New) %>% 
    as_tsibble(index = target_end_date) %>% 
    tsibble::fill_gaps() 
  
  ##  - fit all models
  
}


#Script that reads in case data and forecast data, and created ARIMA model 


#Load Libraries
library(tidyverse)
library(covidHubUtils)
library(fable)



# define transformation and inverse of transformation
fourth_rt<- function(x) {x^(1/4)}
truncated_inv_fourth_rt <- function(x){
  pmax(0,x)^4
}
fourth_rt_transformation <- new_transformation(fourth_rt, truncated_inv_fourth_rt)


hosp_arima_forecast_function <- function(P, p, forecast_date, hosp_data_start_date, case_data_source, case_data_start_date, data_issue_date) {
  
  hosp_truth <- load_truth(
    truth_source = "HealthData",
    target_variable = "inc hosp",
    locations = "25") %>% 
    filter(target_end_date <= forecast_date, target_end_date >= hosp_data_start_date)
  
  ts_hosp <- hosp_truth %>%
    as_tsibble() %>% 
    select(target_end_date, hosp_val = value) %>% 
    tsibble::fill_gaps()
  
  if(case_data_source == "Report") {
    
    report_date_case_truth <- load_truth(
      truth_source = "JHU",
      target_variable = "inc case",
      temporal_resolution = "daily",
      locations = "25") %>% 
      filter(target_end_date <= forecast_date, target_end_date >= case_data_start_date, value >= 0) 
    
    ts_case <- report_date_case_truth %>% 
      select(target_end_date, report_case_val = value) %>%  #change variable names
      as_tsibble() %>% 
      tsibble::fill_gaps()
  
    
  }
  
  if(case_data_source == "Test") {
    test_date_case_truth <- read.csv("csv-data/MA-DPH-csvdata-covid-2022-02-02.csv") %>%  #Should be changed to just one file
      # mutate(issue_date = data_issue_date) %>% 
      # mutate(Date = substr(test_date, 1, 10)) %>% 
      # mutate(Date = as.Date(Date)) %>% 
      filter(test_date <= forecast_date, test_date >= case_data_start_date)
    
    ts_case <- test_date_case_truth %>% 
      select(target_end_date = test_date, test_case_val = new_positive) %>% 
      mutate(target_end_date = as.Date(target_end_date)) %>%
      as_tsibble() %>% 
      tsibble::fill_gaps() 
    
  }
  
  merge_all_ts <- ts_hosp %>% left_join(ts_case) %>%
    filter(target_end_date >= as.Date("2020-07-14"))
  
  
  if(case_data_source == "Report") {
    merge_all_ts$temp_col <- merge_all_ts$report_case_val
  }
  
  if(case_data_source == "Test") {
    merge_all_ts$temp_col <- merge_all_ts$test_case_val
  }
  
  
  model_cases_by_source <- merge_all_ts[c("target_end_date", "temp_col")] %>% 
    model(ARIMA(fourth_rt_transformation(temp_col)))
  
  report(model_cases_by_source)
  
  new_case_by_source_data <- model_cases_by_source %>% 
    forecast(h=28) 
  
  print(new_case_by_source_data %>%
          autoplot(merge_all_ts[c("target_end_date", "temp_col")]) +
          labs(y = paste("cases by",case_data_source, "date") , title = paste("ARIMA Forecast of cases by", case_data_source, "date")))
  
  all_cases_by_date <-  merge_all_ts[c("target_end_date", "temp_col")] %>%
    bind_rows(select(as_tsibble(new_case_by_source_data), target_end_date, temp_col = `.mean`))
  
  
  hosps_and_case_by_date <- ts_hosp %>%
    full_join(all_cases_by_date) %>%
    mutate(case_lag1 = lag(temp_col, 1),
           case_lag2 = lag(temp_col, 2),
           case_lag7 = lag(temp_col, 7),
           case_lag8 = lag(temp_col, 8),
           case_lag9 = lag(temp_col, 9),
           case_lag14 = lag(temp_col, 14),
           case_lag15 = lag(temp_col, 15),
           case_lag21 = lag(temp_col, 21),
           hosp_lag1 = lag(hosp_val, 1),
           hosp_lag2 = lag(hosp_val, 2),
           hosp_lag3 = lag(hosp_val, 3),
           hosp_lag7 = lag(hosp_val, 7),
           hosp_lag8 = lag(hosp_val, 8),
           hosp_lag9 = lag(hosp_val, 9),
           hosp_lag14 = lag(hosp_val, 14),
           hosp_lag15 = lag(hosp_val, 15),
           hosp_lag16 = lag(hosp_val, 16),
           hosp_lag17 = lag(hosp_val, 17),
           hosp_lag21 = lag(hosp_val, 21),
           hosp_lag22 = lag(hosp_val, 22),
           hosp_lag23 = lag(hosp_val, 23),
           hosp_lag24 = lag(hosp_val, 24)) %>%
    ## filter comes last to make sure we have lags early in the filtered dataset
    filter(target_end_date >= hosp_data_start_date)
  

  response_var <- "fourth_rt_transformation(hosp_val)"
  
  formula <- paste0(response_var, " ~ ",
                    paste0("hosp_lag", seq_len(p), collapse = " + "), " + ", paste0("case_lag", seq_len(p), collapse = " + "))
  seasonal_terms <- ""
  for (seasonal_lag in seq_len(P)) {
    seasonal_terms <- paste0(
      seasonal_terms, " + ",
      paste0("hosp_lag", seq(from = 0, length = p + 1) + seasonal_lag * 7, collapse = " + "), " + ",
      paste0("case_lag", seq(from = 0, length = p + 1) + seasonal_lag * 7, collapse = " + "))
  }
  formula <- paste0(formula, seasonal_terms)

  print(formula)
  
  #need to figure out why this is LM with arima errors.  
  hosps_and_case_model <- hosps_and_case_by_date %>%
    filter(target_end_date <= forecast_date) %>%
    model(ARIMA(as.formula(formula)))
  
  report(hosps_and_case_model)
  
hosp_forecast_df <- hosps_and_case_model %>% 
    forecast(new_data = filter(hosps_and_case_by_date, target_end_date > forecast_date), point_forecast = list(.mean = mean, .median = median)) 
  
hosp_forecast_df %>%
    autoplot(hosps_and_case_by_date) +
    labs(y = "Hospitalizations", title = paste("ARIMA Forecast of hospitalizations using cases by", case_data_source, "date"))

return(hosp_forecast_df)
}

forecast(h=28, point_forecast = list(.mean = mean, .median = median))


hosp_arima_forecast_function(P = 2,
                             p = 1, 
                             forecast_date <- as.Date("2021-06-01"),
                             hosp_data_start_date <- as.Date("2020-08-01"),
                             case_data_source = "Report",
                             case_data_start_date <- as.Date("2020-03-01"),
                             data_issue_date = as.Date("2022-02-02"))


hosp_arima_forecast_function(P = 2,
                             p = 1, 
                             forecast_date <- as.Date("2021-06-01"),
                             hosp_data_start_date <- as.Date("2020-08-01"),
                             case_data_source = "Test",
                             case_data_start_date <- as.Date("2020-03-01"),
                             data_issue_date = as.Date("2022-02-02"))


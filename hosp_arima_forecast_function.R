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

#add d and D to function
hosp_arima_forecast_function <- function(data, column_name, P, p, D, d) {
  
 
 data$case_col <- data$"column_name"
  
  model_cases_by_source <- data[c("target_end_date", "case_col")] %>% 
    model(ARIMA(fourth_rt_transformation(case_col)))
  
  report(model_cases_by_source)
  
  new_case_by_source_data <- model_cases_by_source %>% 
    forecast(h=28) 

  hosps_and_case_by_date <- data %>%
    mutate(case_lag1 = lag(case_col, 1),
           case_lag2 = lag(case_col, 2),
           case_lag7 = lag(case_col, 7),
           case_lag8 = lag(case_col, 8),
           case_lag9 = lag(case_col, 9),
           case_lag14 = lag(case_col, 14),
           case_lag15 = lag(case_col, 15),
           case_lag21 = lag(case_col, 21),
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
           hosp_lag24 = lag(hosp_val, 24)) 
  
  
  #function to transform hospitalizations 
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

  print(formula) #print formula to view lags 
  
  #need to figure out why this is LM with arima errors.  
  hosps_and_case_model <- hosps_and_case_by_date %>%
    filter(target_end_date <= forecast_date) %>%
    model(ARIMA(as.formula(formula)))
  
  report(hosps_and_case_model)
  
hosp_forecast_df <- hosps_and_case_model %>% 
    forecast(new_data = filter(hosps_and_case_by_date, target_end_date > forecast_date), point_forecast = list(.mean = mean, .median = median)) 
  
return(hosp_forecast_df)
}

forecast(h=28, point_forecast = list(.mean = mean, .median = median))


hosp_arima_forecast_function(P = 2,
                             p = 1, 
                             data = merge_all_ts, 
                             column_name = report_case_val)


hosp_arima_forecast_function(P = 2,
                             p = 1, 
                             forecast_date <- as.Date("2021-06-01"),
                             hosp_data_start_date <- as.Date("2020-08-01"),
                             case_data_source = "Test",
                             case_data_start_date <- as.Date("2020-03-01"),
                             data_issue_date = as.Date("2022-02-02"))


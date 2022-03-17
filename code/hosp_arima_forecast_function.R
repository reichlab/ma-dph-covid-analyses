#Script that reads in case data and forecast data, and created ARIMA model 

#Load Libraries
library(tidyverse)
library(covidHubUtils)
library(fable)

# define transformation and inverse of transformation
fourth_rt<- function(x) {x^(1/4)}
truncated_inv_fourth_rt <- function(x) {pmax(0,x)^4 }
fourth_rt_transformation <- new_transformation(fourth_rt, truncated_inv_fourth_rt)

#add d and D to function
arima_hosp_forecasts <- function(data, case_col, p, P, d, D) {
  
  data$cases <- pull(data, case_col)
  
  model_cases_by_source <- data[c("target_end_date", "cases")] %>% 
    model(ARIMA(fourth_rt_transformation(cases)))
  
  new_case_by_source_data <- model_cases_by_source %>% 
    forecast(h=28) %>%
    as_tsibble() %>%
    filter(target_end_date > forecast_date) %>%
    select(target_end_date, cases = .mean)

  hosps_and_case_by_date <- data
  
  # hosps_and_case_by_date <- data %>%
  #   mutate(case_lag1 = lag(cases, 1),
  #          case_lag2 = lag(cases, 2),
  #          case_lag7 = lag(cases, 7),
  #          case_lag8 = lag(cases, 8),
  #          case_lag9 = lag(cases, 9),
  #          case_lag14 = lag(cases, 14),
  #          case_lag15 = lag(cases, 15),
  #          case_lag21 = lag(cases, 21),
  #          hosp_lag1 = lag(hosps, 1),
  #          hosp_lag2 = lag(hosps, 2),
  #          hosp_lag3 = lag(hosps, 3),
  #          hosp_lag7 = lag(hosps, 7),
  #          hosp_lag8 = lag(hosps, 8),
  #          hosp_lag9 = lag(hosps, 9),
  #          hosp_lag14 = lag(hosps, 14),
  #          hosp_lag15 = lag(hosps, 15),
  #          hosp_lag16 = lag(hosps, 16),
  #          hosp_lag17 = lag(hosps, 17),
  #          hosp_lag21 = lag(hosps, 21),
  #          hosp_lag22 = lag(hosps, 22),
  #          hosp_lag23 = lag(hosps, 23),
  #          hosp_lag24 = lag(hosps, 24)) 
  # 
  
  ## function to transform hospitalizations 
  response_var <- "fourth_rt_transformation(hosps)"
   
  ## build RHS of formula and needed variables for values of p
  formula <- paste0(response_var, " ~ ",
                    paste0("hosp_lag", seq_len(p), collapse = " + "), 
                    " + ", 
                    paste0("case_lag", seq_len(p), collapse = " + "))
  
  ## add lag columns to data
  for(lag_i in seq_len(p)){
    hosps_and_case_by_date <- hosps_and_case_by_date %>%
      mutate("case_lag{lag_i}" := lag(cases, lag_i),
             "hosp_lag{lag_i}" := lag(hosps, lag_i))
  }
  
  ## add seasonal terms if P>0
  seasonal_terms <- ""
  for (seasonal_lag in seq_len(P)) {
    
    ## add terms to formula
    seasonal_terms <- paste0(
      seasonal_terms, " + ",
      paste0("hosp_lag", seq(from=0, to=p) + seasonal_lag * 7, collapse = " + "), " + ",
      paste0("case_lag", seq(from=0, to=p) + seasonal_lag * 7, collapse = " + "))
    
    # add lag columns to data
    for(lag_p in seq(from=0, to=p)){
      this_lag <- lag_p + seasonal_lag * 7
      hosps_and_case_by_date <- hosps_and_case_by_date %>%
        mutate("case_lag{this_lag}" := lag(cases, this_lag),
               "hosp_lag{this_lag}" := lag(hosps, this_lag))
    }
  }
  
  formula <- as.formula(paste0(formula, seasonal_terms))

  print(formula) #print formula to view lags 
  
  #need to figure out why this is LM with arima errors.  
  hosps_and_case_model <- hosps_and_case_by_date %>%
    model(ARIMA(formula))
  
  hosp_forecast_df <- hosps_and_case_model %>% 
    forecast(new_data = new_case_by_source_data, 
             point_forecast = list(.mean = mean, .median = median)) 
  
  return(list(forecasts=hosp_forecast_df, case_model=model_cases_by_source, hosp_model=hosps_and_case_model))
}


arima_hosp_forecasts(P = 2,
                     p = 1, 
                     data = , 
                     column_name = report_case_val)


hosp_arima_forecast_function(P = 2,
                             p = 1, 
                             forecast_date <- as.Date("2021-06-01"),
                             hosp_data_start_date <- as.Date("2020-08-01"),
                             case_data_source = "Test",
                             case_data_start_date <- as.Date("2020-03-01"),
                             data_issue_date = as.Date("2022-02-02"))


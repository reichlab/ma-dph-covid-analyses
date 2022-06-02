## figure showing as-of and final data + forecasts
library(tidyverse)
library(covidData)

theme_set(theme_bw())

forecast_date <- as.Date("2022-01-24")
final_date <- as.Date("2022-04-29")
state <- "ca"
state_code <- substr(covidcast::abbr_to_fips(state), 0,2)

## chose best test phase models for each data type with smooth=FALSE
testdate_forecast_model <- "test_final_smooth_case_False_SARIX_p_3_d_1_P_1_D_0"
rptdate_forecast_model <- "report_final_smooth_case_False_SARIX_p_3_d_1_P_1_D_0"
none_forecast_model <- "none_final_smooth_case_False_SARIX_p_4_d_1_P_1_D_0"

## compile as-of data
testdate_asof_filename <- file.path("csv-data", paste0("CA-DPH-testdate-covid-",forecast_date,".csv"))
rptdate_asof_filename <- file.path("csv-data", paste0("CA-DPH-reportdate-covid-",forecast_date,".csv"))

testdate_asof_data <- read_csv(testdate_asof_filename) |> 
  select(-issue_date) |> 
  rename(date = test_date, test_date_cases = new_positive)

rptdate_asof_data <- read_csv(rptdate_asof_filename) |> 
  select(-issue_date) |> 
  rename(date = report_date, rpt_date_cases = new_positive) 

as_of_data <- left_join(testdate_asof_data, rptdate_asof_data) |> 
  pivot_longer(cols = -date, names_to = "data_type")

as_of_hosp_data <- load_data(as_of = forecast_date, measure="hospitalizations", 
                             temporal_resolution = "daily",
                             location_code=state_code, source="healthdata")

## compile final data
testdate_final_filename <- file.path("csv-data", paste0("CA-DPH-testdate-covid-",final_date,".csv"))
rptdate_final_filename <- file.path("csv-data", paste0("CA-DPH-reportdate-covid-",final_date,".csv"))

testdate_final_data <- read_csv(testdate_final_filename) |> 
  select(-issue_date) |> 
  rename(date = test_date, test_date_cases = new_positive)

testdate_final_smooth <- testdate_final_data |> 
  mutate(test_date_cases = (lag(test_date_cases, 0) + 
                        lag(test_date_cases, 1) + 
                        lag(test_date_cases, 2) + 
                        lag(test_date_cases, 3) + 
                        lag(test_date_cases, 4) + 
                        lag(test_date_cases, 5) +
                        lag(test_date_cases, 6)) / 7 )

rptdate_final_data <- read_csv(rptdate_final_filename) |> 
  select(-issue_date) |> 
  rename(date = report_date, rpt_date_cases = new_positive) 

rptdate_final_smooth <- rptdate_final_data |> 
  mutate(rpt_date_cases = (lag(rpt_date_cases, 0) + 
                        lag(rpt_date_cases, 1) + 
                        lag(rpt_date_cases, 2) + 
                        lag(rpt_date_cases, 3) + 
                        lag(rpt_date_cases, 4) + 
                        lag(rpt_date_cases, 5) +
                        lag(rpt_date_cases, 6)) / 7 ) 

final_data <- left_join(testdate_final_data, rptdate_final_data) |> 
  pivot_longer(cols = -date, names_to = "data_type")

final_data_smooth <- left_join(testdate_final_smooth, rptdate_final_smooth) |> 
  pivot_longer(cols = -date, names_to = "data_type")

final_hosp_data <- load_data(as_of = final_date, measure="hospitalizations", 
                             temporal_resolution = "daily",
                             location_code=state_code, source="healthdata")


## compile forecasts
testdate_fcast_filename <- file.path("forecasts", 
                               state, 
                               testdate_forecast_model,
                               paste0(forecast_date, "-", testdate_forecast_model, ".csv"))
rptdate_fcast_filename <- file.path("forecasts", 
                                     state, 
                                     rptdate_forecast_model,
                                     paste0(forecast_date, "-", rptdate_forecast_model, ".csv"))
none_fcast_filename <- file.path("forecasts", 
                                     state, 
                                     none_forecast_model,
                                     paste0(forecast_date, "-", none_forecast_model, ".csv"))

testdate_forecast_data <- read_csv(testdate_fcast_filename) |> 
  mutate(target_variable = substr(target, start = nchar(target)-7, stop=nchar(target)),
         data_type = "test_date_cases") |> 
  pivot_wider(names_from = quantile, values_from = value, names_prefix = "q")
rptdate_forecast_data <- read_csv(rptdate_fcast_filename) |> 
  mutate(target_variable = substr(target, start = nchar(target)-7, stop=nchar(target)),
         data_type = "rpt_date_cases") |> 
  pivot_wider(names_from = quantile, values_from = value, names_prefix = "q")
none_forecast_data <- read_csv(none_fcast_filename) |> 
  mutate(target_variable = substr(target, start = nchar(target)-7, stop=nchar(target)),
         data_type = "none") |> 
  pivot_wider(names_from = quantile, values_from = value, names_prefix = "q")

## plot cases
case_plot <- 
  ggplot(mapping = aes(x=date, color=data_type)) +
  ## as of data lines
  geom_point(data = as_of_data, aes(y=value), alpha=.5) +
  ## final data line until forecast date
  geom_line(data = filter(final_data, date<=forecast_date), aes(y=value)) +
  ## final smoothed data line until forecast date
  geom_line(data = filter(final_data_smooth, date<=forecast_date), aes(y=value), size=1.5, alpha=.6) +
  ## final data line after forecast date
  geom_line(data = filter(final_data, data_type %in% c("test_date_cases", "rpt_date_cases")), aes(y=value), alpha=0.5) +
  ## forecast ribbons
  geom_ribbon(data = filter(testdate_forecast_data, target_variable == "inc case"),
            aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill=data_type), alpha=.3, size=0) +
  geom_ribbon(data = filter(rptdate_forecast_data, target_variable == "inc case"),
            aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill=data_type), alpha=.3, size=0) +
  ## forecast lines
  # geom_line(data = filter(testdate_forecast_data, target_variable == "inc case"),
  #                         aes(x=target_end_date, y=q0.5), alpha=.7, linetype=2) +
  # geom_line(data = filter(rptdate_forecast_data, target_variable == "inc case"),
  #           aes(x=target_end_date, y=q0.5),alpha=.7, linetype=2) +
  scale_x_date(NULL, 
               limits = c(forecast_date - 30, forecast_date+30),
               date_breaks = "1 month", 
               date_labels = "%b '%y",
               expand = expansion(add=1)) +
  theme(axis.ticks.length.x = unit(0.5, "cm"), 
        axis.text.x = element_text(vjust = 7, hjust = -0.2),
        legend.position = c(0.05,0.9), legend.justification = c(0,1)) +
  geom_vline(xintercept = forecast_date, linetype=2, col="grey") + 
  ylab("incident cases") +
  ggtitle(paste("case data and forecasts:", forecast_date))

## plot hospitalizations
hosp_plot <- ggplot(mapping = aes(x=date)) +
  #geom_line(data = as_of_hosp_data, aes(y=inc)) +
  geom_line(data = filter(final_hosp_data, date <= forecast_date), aes(y=inc)) +
  geom_line(data = final_hosp_data, aes(y=inc), alpha=.2) +
  geom_ribbon(data = filter(testdate_forecast_data, target_variable == "inc hosp"),
              aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill=data_type), alpha=.3, size=0) +
  geom_ribbon(data = filter(rptdate_forecast_data, target_variable == "inc hosp"),
              aes(x=target_end_date, ymin=q0.1, ymax=q0.9, fill=data_type), alpha=.3, size=0) +
  geom_line(data = filter(testdate_forecast_data, target_variable == "inc hosp"),
            aes(x=target_end_date, y=q0.5, color=data_type), size=2, alpha=.7) +
  geom_line(data = filter(rptdate_forecast_data, target_variable == "inc hosp"),
            aes(x=target_end_date, y=q0.5, color=data_type), size=2, alpha=.7) +
  scale_x_date(NULL, 
               limits = c(forecast_date - 30, forecast_date+30),
               date_breaks = "1 month", 
               date_labels = "%b '%y",
               expand = expansion(add=1)) +
  theme(axis.ticks.length.x = unit(0.5, "cm"), 
        axis.text.x = element_text(vjust = 7, hjust = -0.2), 
        legend.position = "none") +
  geom_vline(xintercept = forecast_date, linetype=2, col="grey") + 
  ylab("incident hospitalizations") +
  ggtitle(paste("hosp data and forecasts:", forecast_date))

cowplot::plot_grid(case_plot, hosp_plot, nrow=2, align="v")

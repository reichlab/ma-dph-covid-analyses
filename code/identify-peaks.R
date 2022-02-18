## plot all versions of the data

library(tidyverse)
library(slider)
library(rlang)

theme_set(theme_bw())

alldata <- read_csv("csv-data/MA-DPH-covid-alldata.csv") %>%
  mutate(test_date = as.Date(test_date))

most_recent_data <- alldata %>%
  filter(issue_date == max(alldata$issue_date))


#' Identify local maxima in time series data frame
#'
#' @details A given time t is identified as a local maximum if the value of the
#'   response variable at time t is the maximum value of that variable that was
#'   observed within a window of specified size centered at that time
#'
#' @param data a data frame containing at minimum a variable over which to
#'   compute local maxima and a variable with a time index
#' @param response_var the unquoted name of the response variable in `data`
#' @param time_var the unquoted name of the time variable in `data`
#' @param window the number of time points in the rollowing window used for
#'   identifying local maxima
#'
#' @return a data frame with the rows from `data` that were identified as
#'   local maxima
get_local_maxima <- function(data,
                             response_var,
                             time_var,
                             window = 11 * 7 + 1) {
  response_var <- enquo(response_var)
  time_var <- enquo(time_var)
  local_maxima <- data %>%
    dplyr::mutate(
      rolling_max = slider::slide_index_max(!!response_var,
                                            !!time_var,
                                            before = (window - 1) / 2,
                                            after = (window - 1) / 2)
    ) %>%
    dplyr::filter(
      !is.na(rolling_max),
      !!response_var == rolling_max
    )

  return(local_maxima)
}

local_max <- get_local_maxima(most_recent_data,
                              response_var = confirmed_case_7d_avg,
                              time_var = test_date)

## plot most recent data with identified peaks
most_recent_data %>%
  ggplot(aes(x=test_date)) +
  geom_line(aes(y=confirmed_case_7d_avg)) +
  #geom_bar(aes(y=new_positive), stat="identity", alpha=.5)
  geom_point(aes(y=new_positive), alpha=.2) +
  geom_vline(
    mapping = aes(xintercept = test_date),
    data = local_max,
    linetype = 2)


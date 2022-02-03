## plot all versions of the data

library(tidyverse)

theme_set(theme_bw())

alldata <- read_csv("csv-data/MA-DPH-covid-alldata.csv") %>%
  mutate(test_date = as.Date(test_date))

most_recent_data <- alldata %>%
  filter(issue_date == max(alldata$issue_date))


## plot most recent data
most_recent_data %>%
  ggplot(aes(x=test_date)) +
  geom_line(aes(y=confirmed_case_7d_avg)) +
  #geom_bar(aes(y=new_positive), stat="identity", alpha=.5)
  geom_point(aes(y=new_positive), alpha=.2)


## plot data as of 2022-01-07 (Friday)

alldata %>%
  filter(issue_date == "2022-01-07") %>%
  ggplot(aes(x=test_date)) +
  geom_line(aes(y=confirmed_case_7d_avg)) +
  geom_point(aes(y=new_positive), alpha=.2) +
  scale_x_date(limits=c(as.Date("2021-11-15"), as.Date("2022-01-07")))



## compute some lag statistics
date_with_lags <- alldata %>%
  filter(test_date >= "2021-01-01") %>%
  mutate(lag = as.numeric(issue_date - test_date))%>%
  arrange(issue_date) %>%
  group_by(test_date) %>%
  mutate(diff_from_yesterday = new_positive - lag(new_positive)) %>%
  ungroup() %>%
  left_join(select(most_recent_data, test_date, new_positive), by="test_date") %>%
  rename(new_cases_on_issue = new_positive.x, 
         new_cases_most_recent = new_positive.y) %>%
  mutate(pct_diff_of_most_recent = diff_from_yesterday/new_cases_most_recent,
         pct_of_most_recent = new_cases_on_issue/new_cases_most_recent,
         test_day_of_week = lubridate::wday(test_date, label=TRUE))

ggplot(date_with_lags, aes(x=lag, y=pct_diff_of_most_recent, group=test_date)) +
  geom_line() +
  coord_cartesian(xlim=c(0, 100)) +
  scale_x_continuous(breaks=seq(0, 100, by=7))

ggplot(date_with_lags, aes(x=lag, y=pct_of_most_recent, group=test_date)) +
  geom_line(alpha=0.2) +
  coord_cartesian(xlim=c(0, 49)) +
  scale_x_continuous(breaks=seq(0, 100, by=7))

ggplot(date_with_lags, aes(x=factor(lag), y=pct_of_most_recent)) +
  geom_boxplot(alpha=0.2) +
  coord_cartesian(xlim=c(0, 21)) 

date_with_lags %>%
  filter(lag <= 14) %>% 
  ggplot(aes(x=factor(lag), y=pct_of_most_recent)) +
  geom_boxplot(alpha=0.2) +
  facet_grid(.~test_day_of_week) +
  scale_y_continuous(breaks=c(0, .25, .5, .6, .7, .8, .9, 1)) + 
  ylab("fraction of final count reported") +
  xlab("days since test date of cases")


## plot all versions of data

alldata %>%
  ggplot(aes(x=test_date, y=confirmed_case_7d_avg, color=issue_date)) +
  geom_line() +
  scale_y_sqrt()

alldata %>%
  ggplot(aes(x=lag, y=test_date, fill=new_positive)) +
  geom_tile() 

## look at correlations with hospitalizations

library(tidyverse)
library(covidcast)
library(cowplot)
library(ggExtra)
library(forecast)

theme_set(theme_bw())

start_day <- "2020-04-01"
end_day <- "2022-02-01"

case_dat <- read_csv("csv-data/MA-DPH-covid-alldata.csv") %>%
  mutate(test_date = as.Date(test_date)) %>%
  filter(issue_date == max(alldata$issue_date),
         test_date >= start_day,
         test_date <= end_day) %>%
  rename(time_value = test_date,
         value = new_positive) %>%
  mutate(target = "cases by test date", geo_value = "ma") %>% 
  select(target, time_value, geo_value, value)
  
hosp_dat <- covidcast_signal(data_source = "hhs",
                             signal = "confirmed_admissions_covid_1d_7dav", 
                             geo_type = "state", 
                             geo_values = "ma",
                             start_day = start_day, end_day = end_day) %>%
  select(-starts_with("missing"), - sample_size, -stderr) %>%
  mutate(target = "hosps") %>%
  filter(time_value>as.Date("2020-09-01"))


all_dat <- bind_rows(case_dat, hosp_dat) %>%
  mutate(target = factor(target, levels=c("cases by test date", "hosps"))) %>%
  group_by(geo_value, target) %>%
  mutate(scaled_max_value = value/max(value, na.rm=TRUE),
         pct_change_7d = (value - lag(value,7))/lag(value,7),
         pct_change_1d = (value - lag(value))/lag(value),
         rel_inc_7d = (value)/lag(value,7),
         rel_inc_1d = (value)/lag(value))

plot_ccf_function <- function(loc){
  tmp_dat <- filter(all_dat, geo_value == loc)
  p_data <- ggplot(tmp_dat, aes(x=time_value, y=scaled_max_value, color=target)) +
    ggtitle(loc)+
    ylab("scaled inc (daily)")+
    scale_x_date(name=NULL, date_breaks = "3 months", date_minor_breaks = "1 month", date_labels = "%b '%y") +
    theme(legend.position = c(0,1), legend.justification = c(0,1), legend.background=element_rect(fill = alpha("white", 0))) +
    geom_line()
  
  p_growth <- ggplot(tmp_dat, aes(x=time_value, y=rel_inc_7d, color=target)) +
    geom_smooth(span=.15, se=FALSE) +
    geom_point(alpha=.1) +
    geom_hline(yintercept=1, linetype=2, alpha=.7) +
    scale_y_log10(name="rel 1 wk change")+
    scale_x_date(name=NULL, date_breaks = "3 months", date_minor_breaks = "1 month", date_labels = "%b '%y") +
    theme(legend.position = "none") +
    coord_cartesian(ylim=c(0.5, 2.5))
  p1 <- plot_grid(p_data, p_growth, ncol=1, align="v")
  
  cases <- filter(tmp_dat, target=="cases by test date", time_value>as.Date("2020-09-01"))%>% pull(value)
  hosps <- filter(tmp_dat, target=="hosps", time_value>as.Date("2020-09-01"))%>% pull(value)

  p_ch_ccf <- ggCcf(cases, hosps) + 
    ggtitle(NULL) + 
    xlab(NULL) + 
    ylim(-.1,1) + 
    annotate("text", label = "cases & hosps", x=25, y=1, hjust=1)
  
  tmp_ccf <- ggCcf(cases, hosps, lag.max = 35, plot=FALSE)
  message(paste("max correlation =",  tmp_ccf$acf[which.max(tmp_ccf$acf)]))
  message(paste("lag at the max =", tmp_ccf$lag[which.max(tmp_ccf$acf)]))
  
  
  plot_grid(p1, p_ch_ccf, ncol=1)
  
}

plot_ccf_function("ma")

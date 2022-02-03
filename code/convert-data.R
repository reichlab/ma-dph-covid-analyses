## reformatting Excel data

library(tidyverse)

## convert files to csv
files_to_convert <- list.files(path = "raw-data/", pattern="*.xlsx", full.names = TRUE)

master_tbl <- tibble(
  issue_date = NULL,
  test_date = NULL, 
  total_positive = NULL,
  new_positive = NULL,
  confirmed_case_7d_avg = NULL)

for(i in 1:length(files_to_convert)){
  this_issue_date <- as.Date(substr(files_to_convert[i], start = 32, stop=41))
  message(paste("converting", this_issue_date))
  tmp <- readxl::read_excel(files_to_convert[i], sheet = "CasesByDate (Test Date)") %>%
    transmute(test_date = Date,
              total_positive = `Positive Total`,
              new_positive = `Positive New`,
              confirmed_case_7d_avg = `7-day confirmed case average`) %>%
    mutate(issue_date = this_issue_date)
  
  master_tbl <- bind_rows(master_tbl, tmp)
  ## change directory and file extension
  newfile <- gsub("raw", "csv", gsub("xlsx", "csv", files_to_convert[i]))
  
  ## write new csv file if it doesn't exist
  if(!file.exists(newfile))
    readr::write_csv(tmp, file = newfile)
}

write_csv(master_tbl, file="csv-data/MA-DPH-covid-alldata.csv")
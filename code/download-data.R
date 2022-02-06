## scrape and save xls files from DPH
## archive of data on this page: https://www.mass.gov/info-details/archive-of-covid-19-cases-in-massachusetts
## urls take the form: https://www.mass.gov/doc/covid-19-raw-data-february-2-2022/download

library(lubridate)
library(readxl)

start_date <- "2020-04-29" ## first date DPH released raw data

## download all files
dates_to_download <- seq.Date(as.Date(start_date), Sys.Date(), by="1 day") 

for(i in 1:length(dates_to_download)){
  date_to_download <- dates_to_download[i]
  url <- paste0("https://www.mass.gov/doc/covid-19-raw-data-", 
                tolower(month(date_to_download, label=TRUE, abbr=FALSE)),
                "-", 
                mday(date_to_download),
                "-",
                year(date_to_download),
                "/download")
  dest_file <- paste0("raw-data/MA-DPH-rawdata-covid-", ymd(date_to_download), 
                      ifelse(date_to_download <= as.Date("2020-12-31"),
                             ".zip", ".xlsx"))
  if(!file.exists(dest_file))
    try(download.file(url, destfile=dest_file))
}



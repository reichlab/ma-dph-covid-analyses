# Analyses of COVID-19 case data by report and test date


## Massachusetts data
Analyses of Massachusetts DPH COVID-19 data use original data from this site: https://www.mass.gov/info-details/archive-of-covid-19-cases-in-massachusetts

These data represent case numbers aggregated by the date of a test for COVID-19. We store individual files that were made available on the DPH website on a particular day, starting in January 2021. (Data files from 2020 were also available but not as easily scrapable.) The raw Excel-format files available from the archive are stored in the [raw-data](raw-data/) folder. (This folder is not pushed to GitHub to keep the repository to a manageable size.) CSV files of processed data are stored in [csv-data](csv-data/). These files have names that correspond to the date on which they were available on the MA DPH website, e.g. [MA-DPH-csvdata-covid-2021-01-04.csv](csv-data/MA-DPH-csvdata-covid-2021-01-04.csv). The CSV files have the following columns:

 - `test_date`: the date on which the test was performed, in `YYYY-MM-DD` format
 - `total_positive`: the cumulative number of positive tests by `test_date`
 - `new_positive`: the number of new positive tests reported on `test_date`
 - `confirmed_case_7d_avg`: the 7-day average number of new cases. This field is is the original raw-data file, provided and computed by MA DPH.
 - `issue_date`: the date of the release of the file that contained this row of data.
 
All versions of CSV files are aggregated into a single file, which is available in [MA-DPH-covid-alldata.csv](csv-data/MA-DPH-covid-alldata.csv)

The Massachusetts raw data can be downloaded using the [download-data.R](code/download-data.R) script and converted to CSV format using the [convert-data.R](code/convert-data.R) script.
 
## California data
Analyses of California DPH COVID-19 data use original data from this repo: https://github.com/cagov/covid-static-data/tree/main/data/dashboard/confirmed-cases

Data on COVID-19 cases by report and test date in California are downloaded using the [download-data-ca.R](code/download-data-ca.R) script. These files are stored in the [csv-data](csv-data/) folder using the name convention `CA-DPH-testdate-covid-2022-04-19.csv` and `CA-DPH-reportdate-covid-2022-04-19.csv` where the date is retrieved from the `published_date` field of the metadata of the downloaded data. Only specific versions are currently available in this repo, as we have not surfed the GitHub history to download specific versions.

The CA data files have the following columns (the files have either `test_date` or `report_date` depending on the type of data being reported):
 - `test_date`: the date on which the test was performed, in `YYYY-MM-DD` format
 - `report_date`: the date on which the case was reported, in `YYYY-MM-DD` format
 - `new_positive`: the number of new positive tests reported on `test_date`
 - `issue_date`: the date of the release of the file that contained this row of data.

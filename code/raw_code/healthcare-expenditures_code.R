library(here)
library(readr)

## read coverage data into R
coverage <- read_csv('data/raw_data/healthcare-coverage.csv', 
                     skip = 2)
coverage <- read_csv('data/raw_data/healthcare-coverage.csv', 
                     skip = 2,
                     n_max  = which(coverage$Location == "Notes")-1)

## read spending data into R
spending <- read_csv('data/raw_data/healthcare-spending.csv', 
                     skip = 2)
#got some parsing errors...
spending <- read_csv('data/raw_data/healthcare-spending.csv', 
                     skip = 2, 
                     n_max  = which(spending$Location == "Notes")-1)

## Save data to raw_data
## here() starts at /home/rstudio/health-expenditures-project
save(coverage, spending, file = here::here("data", "raw_data", "case_study_1.rda"))


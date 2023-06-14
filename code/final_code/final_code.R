library(here)
library(datasets)
library(tidyverse)
# library(tibble)
# library(tidyr)
# library(dplyr)

# load raw data
load(here::here("data","raw_data", "case_study_1.rda"))
#This loads all the data objects that we previously saved in our raw_data directory.

# load states data
data(state)
state.name
# add Washington DC to dataset and create tibble 
state.abb <- c(state.abb, "DC")
state.region <- as.factor(c(as.character(state.region), "South"))
state.name <- c(state.name, "District of Columbia")
state_data <- tibble(Location = state.name,
                     abb = state.abb,
                     region = state.region)

# explore data
coverage
names(coverage)
spending
names(spending)
state_data

# tidy coverage data
## reshape data to long
coverage <- coverage %>%
  mutate(across(starts_with("20"), 
                as.integer)) %>%  ## convert all year-based columns to integer
  pivot_longer(-Location,         ## Use all columns BUT 'Location'
               names_to = "year_type",
               values_to = "tot_coverage")

## separate year_type
coverage <- coverage %>% 
  separate(col = year_type, 
           into = c("year", "type"),
           sep = "__", convert = TRUE)

## add state-level abbreviations and region
coverage <- coverage %>%
  left_join(state_data, by = "Location")


# tidy spending data
## reshape data to long
spending <- spending %>%
  pivot_longer(-Location,         ## Use all columns BUT 'Location'
               names_to = "year",
               values_to = "tot_spending")
  
## separate "year" and delete "Total Health Spending"
spending <- spending %>% 
  separate(col = year, 
           into = c("year", "name"),
           sep = "__", convert = TRUE) %>% 
  select(-name)


# Join the Data
## inner join datasets
hc <- inner_join(coverage, spending, 
                 by = c("Location", "year"))

## filter country-level summary
hc <- hc %>% 
  filter(Location != "United States")

# add additonal better variables
## explore health care types
table(hc$type)

## separate "Total" since it is the state's total population
pop <- hc %>% 
  filter(type == "Total") %>% 
  select(Location, year, tot_coverage)

## add population level information
hc <- hc %>% 
  filter(type != "Total") %>% 
  left_join(pop, by = c("Location", "year")) %>% 
  rename(tot_coverage = tot_coverage.x,
         tot_pop = tot_coverage.y)
## add proportion covered
hc <- hc %>% 
  mutate(prop_coverage = tot_coverage / tot_pop)
  
## get spending capita in dollars
hc <- hc %>% 
  mutate(spending_capita = (tot_spending*1e6) / tot_pop)
  
# Save tidy data
save(hc, file = here::here("data", "tidy_data", "case_study_1_tidy.rda"))
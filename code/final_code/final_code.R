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


# load tidy data ----------------------------------------------------------


# load data
load(here::here("data", "tidy_data", "case_study_1_tidy.rda"))
hc


# Visualize data set ------------------------------------------------------


# install.packages("visdat")
library(visdat)

# visualize data
vis_dat(hc)

# visualize missiing data
vis_miss(hc)

hc |> 
  filter(is.na(tot_coverage))


# Summarize data set ------------------------------------------------------


library(skimr)

# get summary of the data
skim(hc)

# group by year
hc %>% 
  group_by(year) %>%
  skim()


# Q1: Relationship between coverage and spending? -------------------------

# coverage vs spending
hc %>%
  filter(type == "Employer", 
         year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage)) +
  geom_point() + 
  labs(x = "spending per capita",
       y = "coverage proportion")

# generate scatterplot with fit line
hc %>%
  filter(type == "Employer", 
         year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage)) + 
  geom_point() + 
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red")

# add state abbreviation labels
hc %>%
  filter(type == "Employer", 
         year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage)) + 
  geom_point() + 
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150)

# color by region
hc %>%
  filter(type == "Employer", 
         year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + ######
  geom_point() + 
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150, 
            show.legend = FALSE) ######

# create facets by year
hc %>%
  filter(type == "Employer") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  facet_wrap(~year) + #######
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150, 
            show.legend = FALSE)

# We see that the overall trend holds, but there has been some movement.
# For example, we see at a glance that DC has a higher proportion of its
# population covered in 2014 relative to 2013, while MA saw a drop in coverage.
# UT appears to be an outlier in both years having low spending but a high
# proportion of individuals covered.

# visualize 2013 data by type
hc %>%
  filter(year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  facet_wrap(~type) +
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150, 
            show.legend = FALSE)

# visualize 2014 data by type
hc %>%
  filter(year == "2014") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  facet_wrap(~type) +
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150, 
            show.legend = FALSE)

# From these data, we see that Employer health care coverage is the most popular
# way in which individuals receive their health insurance across all states. 
# We also see a flat or positive relationship for all other types of insurance, 
# except for “Uninsured”. We see that the more money spent per capita the fewer
# individuals the state has without insurance, as one might expect.


# save plots --------------------------------------------------------------

pdf(here::here("figures", "exploratory", "2013and2014_spending_and_coverage.pdf"))

hc %>%
  filter(type == "Employer") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  facet_wrap(~year) +
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150, 
            show.legend = FALSE)

dev.off()

pdf(here::here("figures", "exploratory", "2013_coverage_type.pdf"))
hc %>%
  filter(year == "2013") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  facet_wrap(~type) +
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150, 
            show.legend = FALSE)

dev.off()

pdf(here::here("figures", "exploratory", "2014_coverage_type.pdf"))

hc %>%
  filter(year == "2014") %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  facet_wrap(~type) +
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  geom_text(aes(label=abb), 
            nudge_x = 150, 
            show.legend = FALSE)

dev.off()


# Q2: Spending Across Geographic Regions? ---------------------------------

# generate boxplot
hc %>% 
  ggplot(aes(x = region, 
             y = spending_capita)) + 
  geom_boxplot() +
  labs(y = "spending per capita")

# add data points to boxplot
hc %>% 
  filter(type == "Employer") %>%
  ggplot(aes(x = region, 
             y = spending_capita)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.2) +
  labs(y = "spending per capita")

# Q3: Coverage and Spending Change Over Time? -----------------------------

# color by region
hc %>% 
  ggplot(aes(x = spending_capita, 
             y = prop_coverage,
             color = region)) + 
  geom_point() + 
  labs(x = "spending per capita",
       y = "coverage proportion") +
  geom_smooth(method = "lm", col = "red") + 
  facet_grid(year~type)

# Visually, we can start to get a sense that a few things changed from 
# 2013 to 2014. For example, as we saw previously, individual states changed
# from one year to the next, but overall patterns seem to hold pretty steady
# between these two years.




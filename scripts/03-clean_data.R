#### Preamble ####
# Purpose: Cleans the raw election data
# Author: Xuanle Zhou, Yongqi Liu, Yuxuan Wei
# Date: 14 October 2024
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Get the raw data from the US election pollsters
# Any other information needed? NA

#### Workspace setup ####
library(tidyverse)
library(dplyr)
raw_data <- read_csv("data/01-raw_data/US_voting.csv")
colnames(raw_data)

#### Clean the poll data
# Filter for likely voters (lv)
#likely_voters_data <- raw_data %>% filter(population == "lv")

# Now we decide to use pollster with grade >2.5 as 'high quality' pollster
cleaned_data <- raw_data %>% filter(numeric_grade > 2.5)
cleaned_data$mdy <- mdy(cleaned_data$end_date)
cleaned_data <- cleaned_data %>%
  filter(candidate_name== "Donald Trump")

# Clean raw dataset, remove columns with many missing values and keep the columns we want to make analysis on
cleaned_data <- cleaned_data %>% dplyr::select(pollster_rating_name, methodology, numeric_grade, start_date,
                                               end_date, sample_size, population, state, 
                                               candidate_name, pct)

#filter(!is.na(population) & !is.na(state) & !is.na(candidate_name) & !is.na(pct) & !is.na(numeric_grade& )

# Change name for different types of voters in population column
cleaned_data <- cleaned_data %>%
  mutate(
    population_group = case_when(
      population == "lv" ~ "likely voters",
      population == "rv" ~ "registered voters",
      population == "a"  ~ "adults"))

# Delete population column 
cleaned_data <- cleaned_data %>% select(-population)

# Delete rows where end dates are earlier than start dates
cleaned_data <- cleaned_data %>%
  filter(end_date >= start_date)

# convert the format of the start date and end date
cleaned_data <- cleaned_data %>%
  mutate(
    start_date = mdy(start_date),  # or dmy(), ymd() depending on the format
    end_date = mdy(end_date)       # same as above
  )

# Revise the state name
cleaned_data <- cleaned_data %>%
  mutate(state = ifelse(is.na(state), "National",
                        ifelse(state == "Nebraska CD-2", "Nebraska",
                               ifelse(state %in% c("Maine CD-1", "Maine CD-2"), "Maine", state))))
# revise the name of column 
cleaned_data <- cleaned_data %>%
  rename(percent = pct)

# Delete NA
cleaned_data <- na.omit(cleaned_data)

# regroup the methodology
cleaned_data <- cleaned_data %>%
  mutate(methodology = ifelse(grepl("/", methodology), "Mixed", methodology))

# View the cleaned dataset
head(cleaned_data)

#### Save data ####
# Save the cleaned dataset to a new CSV file
write_csv(cleaned_data, "data/02-analysis_data/cleaned_US_voting.csv")

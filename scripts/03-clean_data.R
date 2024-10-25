#### Preamble ####
# Purpose: Cleans the raw election data
# Author: Xuanle Zhou, Yongqi Liu, Yuxuan Wei
# Date: 14 October 2024
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Get the raw data from the US election pollsters
# Any other information needed? NA

#### Workspace setup ####
#install.packages("arrow")
library(tidyverse)
library(dplyr)
library(arrow)

# Read the raw data
raw_data <- read_csv("data/01-raw_data/US_voting.csv")
colnames(raw_data)

#### Clean the poll data
# Now we decide to use pollster with grade >2.5 as 'high quality' pollster
cleaned_data <- raw_data %>% filter(numeric_grade > 2.5)
cleaned_data$mdy <- mdy(cleaned_data$end_date)

# Define the cutoff date (July 21, 2024)
cutoff_date <- as.Date("2023-10-01")
cutoff_date_harris <- as.Date("2024-07-21")

# Fitler trump and Harris
cleaned_data <- cleaned_data %>% filter(mdy>cutoff_date) %>%
  filter((candidate_name == "Donald Trump") | 
           (candidate_name == "Kamala Harris" & mdy > cutoff_date_harris))

# Clean raw dataset, remove columns with many missing values and keep the columns we want to make analysis on
cleaned_data <- cleaned_data %>% dplyr::select(pollster_rating_name, methodology, numeric_grade, start_date,
                                               end_date, sample_size, population, state, 
                                           candidate_name, pct, transparency_score) 

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

# Convert the format of the start date and end date
cleaned_data <- cleaned_data %>%
  mutate(
    start_date = mdy(start_date), 
    end_date = mdy(end_date))

# Revise the state name
cleaned_data <- cleaned_data %>%
  mutate(state = ifelse(is.na(state), "National",
                        ifelse(state == "Nebraska CD-2", "Nebraska",
                               ifelse(state %in% c("Maine CD-1", "Maine CD-2"), "Maine", state))))
# Revise the name of column 
cleaned_data <- cleaned_data %>%
  rename(percent = pct)

# Create the counting down column to the final election date
final_election <- as.Date("11/5/24", format="%m/%d/%y")

# Create the column
cleaned_data <- cleaned_data %>% 
  mutate(days_until_election = as.numeric(difftime(final_election, end_date, units = "days")))

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

write_parquet(cleaned_data, "data/02-analysis_data/cleaned_US_voting.parquet")
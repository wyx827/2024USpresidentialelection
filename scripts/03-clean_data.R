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

# Sort pollsters based on numeric_grade and pollscore
#reliable_pollsters <- likely_voters_data %>%
  #filter(!is.na(numeric_grade) & !is.na(pollscore)) %>%
  #arrange(desc(numeric_grade), desc(pollscore))

# Rank pollsters by transparency_score as well
#Higher transparency scores indicate more detailed methodology information
#reliable_pollsters <- reliable_pollsters %>%
#  filter(!is.na(transparency_score)) %>%
 # arrange(desc(transparency_score))

# Consider methodology
# Extract the columns that provide relevant information
#final_pollster_ranking <- reliable_pollsters %>%
 # dplyr::select(pollster, numeric_grade, pollscore, transparency_score, methodology)



# Now we decide to use pollster with grade >2.5 as 'high quality' pollster
cleaned_data <- raw_data %>% filter(numeric_grade > 2.5)
# Clean raw dataset, remove columns with many missing values and keep the columns we want to make analysis on
cleaned_data <- cleaned_data %>% dplyr::select(pollster, methodology, numeric_grade, start_date,
                                               end_date, sample_size, population, state, 
                                               candidate_name, pct)

#filter(!is.na(population) & !is.na(state) & !is.na(candidate_name) & !is.na(pct) & !is.na(numeric_grade& )

# Change name for different types of voters in population column
cleaned_data <- cleaned_data %>%
  mutate(
    population_group = case_when(
      population == "lv" ~ "likely_voters",
      population == "rv" ~ "registered_voters",
      population == "a"  ~ "adults"))

# View the cleaned dataset
head(cleaned_data)

#### Save data ####
# Save the cleaned dataset to a new CSV file
write_csv(cleaned_data, "data/02-analysis_data/cleaned_US_voting.csv")

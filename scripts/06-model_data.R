#### Preamble ####
# Purpose: Model for the Trump's election
# Author: Xuanle Zhou, Yongqi Liu, Yuxuan Wei
# Date: October 22, 2024
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need the US election poll data
# Any other information needed? NA


#### Workspace setup ####
#install.packages("caret")
#install.packages("rsample")
library(tidyverse)
library(dplyr)
library(rstanarm)
library(ggplot2)
library(caret)
library(modelsummary)
library(arrow)
library(rsample)


#### Read data ####
analysis_data <- read_parquet("data/02-analysis_data/cleaned_US_voting.parquet")

analysis_data <- analysis_data |>
  filter(candidate_name == "Donald Trump") 

### Model data ####
set.seed(853)

# Perform a stratified split based on the 'state' variable to ensure all levels are present in both sets
split <- initial_split(data=analysis_data, prop = 0.8, strata = state)

# Create training and testing sets
analysis_data_train <- training(split)
analysis_data_test <- testing(split)


# Build up model
regression_model <- lm(percent ~ numeric_grade + sample_size + state + transparency_score + days_until_election, 
                      data = analysis_data_train)

#### Save model ####
write_parquet(analysis_data_test, sink = "data/02-analysis_data/test_data.parquet")
write_parquet(analysis_data_train, sink = "data/02-analysis_data/train_data.parquet")
saveRDS(regression_model, file = "models/regression_model.rds")


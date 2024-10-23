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
library(bayesplot)
library(ggplot2)
library(caret)
library(modelsummary)
library(arrow)
library(rsample)
library(splines)


#### Read data ####
analysis_data <- read_parquet("data/02-analysis_data/cleaned_US_voting.parquet")

analysis_data <- analysis_data |>
  filter(candidate_name == "Donald Trump") 

# Build up model
regression_model <- lm(percent ~ numeric_grade + sample_size + state + transparency_score + end_date, 
                      data = analysis_data)

#### Save model ####
# Save the model 
saveRDS(regression_model, file = "models/regression_model.rds")




### Model data ####
#Build up Logistic Regression Model
# For reproducibility
set.seed(853)

# Group by more detailed factors
analysis_data <- analysis_data %>%
  group_by(state, pollster_rating_name, methodology, sample_size, population_group) %>%
  summarise(
    Trump_percent = mean(percent[candidate_name == "Donald Trump"], na.rm = TRUE),
    Harris_percent = mean(percent[candidate_name == "Kamala Harris"], na.rm = TRUE)
  )

# Create a binary outcome for prediction: 1 if Trump is predicted to win, 0 if Harris
analysis_data <- analysis_data %>%
  mutate(trump_win = ifelse(Trump_percent > Harris_percent, 1, 0))

analysis_data <- na.omit(analysis_data)
# Split the data into training (70%) and testing (30%) sets
split <- initial_split(analysis_data, prop = 0.7)
analysis_train_data <- training(split)
analysis_test_data <- testing(split)


# Build the logistic regression model using rstanarm using train data
logistic_model <- stan_glm(
  trump_win ~ state + population_group + numeric_grade + sample_size + methodology,
  data = analysis_data_train,
  family = binomial(link = "logit"),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  seed = 853,
  iter = 4000)


modelsummary(logistic_model)



#### Save model ####
saveRDS(logistic_model, file = "models/logistic_model.rds")
write_parquet(analysis_data_test, sink = "data/02-analysis_data/test_data.parquet")


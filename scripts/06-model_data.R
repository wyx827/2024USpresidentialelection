#### Preamble ####
# Purpose: Model for the Trump's election
# Author: Xuanle Zhou, Yongqi Liu, Yuxuan Wei
# Date: October 22, 2024
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need the US election poll data
# Any other information needed? NA


#### Workspace setup ####
#install.packages("splines")
library(tidyverse)
library(dplyr)
library(splines)
library(rstanarm)
library(bayesplot)
library(rstanarm)
library(ggplot2)
#install.packages("caret")
library(caret)
library(modelsummary)


#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/cleaned_US_voting.csv")

### Model data ####
# 1. Bayesian Model
# Change 'end_date' and 'state' to factor variables
analysis_data <- analysis_data |>
  mutate( end_date = as.numeric(end_date), state = factor(state))

# Bayesian Regression Model (Without State Effect):
bayesian_model <- stan_glm(
  percent ~ ns(end_date, df = 9) + pollster_rating_name, 
  data = analysis_data,
  family = gaussian(),
  prior = normal(0, 2.5),  # Weakened prior to allow more spread
  prior_intercept = normal(50, 20),  # Slightly wider prior for intercept
  seed = 1234,
  iter = 3000,  # Increase iterations for better convergence
  chains = 4,
  refresh = 0)

# Bayesian Mixed-Effects Model (With Random Effects for State and Pollster):
bayesian_model_state <- stan_glmer(
  percent ~ ns(end_date, df = 9) + (1 | pollster_rating_name) + (1 | state),
  data = analysis_data,
  family = gaussian(),
  prior = normal(0, 5),  # Moderate prior for the coefficients
  prior_intercept = normal(50, 20),  # Adjusted prior intercept
  seed = 1234,
  adapt_delta = 0.99,  # Increased adapt_delta to improve convergence
  iter = 3000,  # Increased iterations for better convergence
  chains = 4,
  refresh = 0)

# Summarize the model
summary(bayesian_model)
summary(bayesian_model_state)

# Posterior predictive checks
pp_check(bayesian_model)
pp_check(bayesian_model_state)

pp_check(sim_run_data_second_model_rstanarm) +
  theme_classic() +
  theme(legend.position = "bottom")

posterior_vs_prior(sim_run_data_second_model_rstanarm) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  coord_flip()

plot(
  sim_run_data_second_model_rstanarm,
  "areas"
)



# Convert pollster_rating_name to a factor with appropriate levels
analysis_data$pollster_rating_name <- factor(analysis_data$pollster_rating_name)

# Assuming "Emerson College"
new_data <- data.frame(
  end_date_num = seq(
    min(analysis_data$end_date_num, na.rm = TRUE),
    max(analysis_data$end_date_num, na.rm = TRUE),
    length.out = 100
  ),
  pollster_rating_name = factor("Emerson College", levels = levels(analysis_data$pollster_rating_name))
)

posterior_preds <- posterior_predict(spline_model_random, newdata = new_data)

# Summarize predictions
pred_summary <- new_data %>%
  mutate(
    pred_mean = colMeans(posterior_preds),
    pred_lower = apply(posterior_preds, 2, quantile, probs = 0.025),
    pred_upper = apply(posterior_preds, 2, quantile, probs = 0.975)
  )

# Plot the result
ggplot(analysis_data, aes(x = end_date_num, y = percent, color = pollster_rating_name)) +
  geom_point() +
  geom_line(data = pred_summary, aes(x = end_date_num, y = pred_mean), color = "blue") +
  geom_ribbon(
    data = pred_summary,
    aes(x = end_date_num, ymin = pred_lower, ymax = pred_upper),
    fill = "blue", alpha = 0.2, inherit.aes = FALSE
  ) +
  labs(x = "Days since earliest poll", y = "Trump Percentage", title = "Trump Polling Percentage over Time with Spline Fit") +
  theme_minimal()







#2. Logistic Regression Model
# Read the data
analysis_data <- read_csv("data/02-analysis_data/cleaned_US_voting.csv")

# Calculate Trump and Harris percentages for each state using summarise and case_when
analysis_data <- analysis_data %>%
  group_by(state) %>%
  mutate(
    Trump_percent = mean(case_when(candidate_name == "Donald Trump" ~ percent), na.rm = TRUE),
    Harris_percent = mean(case_when(candidate_name == "Kamala Harris" ~ percent), na.rm = TRUE))

# Create a binary outcome for prediction: 1 if Trump is predicted to win, 0 if Harris
analysis_data <- analysis_data %>%
  mutate(trump_win = ifelse(Trump_percent > Harris_percent, 1, 0))

# Summarize and clean the data to avoid duplicates
set.seed(853)

analysis_data_reduced <- 
  analysis_data |> 
  slice_sample(n = 1000)

logistic_model <- stan_glm(
  trump_win ~ pollster_rating_name + state + population_group + numeric_grade + sample_size + methodology,
  data = analysis_data_reduced,
  family = binomial(link = "logit"),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  seed = 853
)

modelsummary(logistic_model)


#### Save model ####
saveRDS(logistic_model, file = "models/logistic_model.rds")



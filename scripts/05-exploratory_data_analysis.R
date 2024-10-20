#### Preamble ####
# Purpose: Do exploratory data analyis on the cleaned election data
# Author: Xuanle Zhou, Yongqi Liu, Yuxuan Wei
# Date: 14 October 2024
# Contact: cassieliu.liu@mail.utoronto.ca, shaw.wei@mail.utoronto.ca
# License: MIT
# Pre-requisites: Get the cleaned dataSET of the US election 
# Any other information needed? NA

#### Workspace setup ####
library(tidyverse)
#install.packages("rstanarm")
library(rstanarm)
library(dplyr)

#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/cleaned_US_voting.csv")
colnames(analysis_data)

#### Summary the numeric variables ####
summary(cleaned_data)
summary(analysis_data$sample_size)
summary(analysis_data$pct)
summary(analysis_data$numeric_grade)

# Histogram of support percentages (pct) for Trump
ggplot(cleaned_data, aes(x = percent)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Support (pct) for Donald Trump", x = "Support Percentage (pct)", y = "Count") +
  theme_minimal()

# Histogram of sample size to see its distribution
ggplot(cleaned_data, aes(x = sample_size)) +
  geom_histogram(binwidth = 100, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Sample Size", x = "Sample Size", y = "Count") +
  theme_minimal()

# Line plot of candidate support over time
ggplot(candidates, aes(x = as.Date(start_date, format = "%m/%d/%y"), y = pct, color = candidate_name)) +
  geom_line() +
  labs(title = "Candidate Support Over Time", x = "Date", y = "Support Percentage (pct)") +
  theme_minimal()

# Boxplot to compare support percentages across candidates
ggplot(candidates, aes(x = candidate_name, y = pct, fill = candidate_name)) +
  geom_boxplot() +
  labs(title = "Comparison of Support Across Candidates", x = "Candidate", y = "Support Percentage (pct)", fill="Candidate Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# View the updated dataset
head(cleaned_data)

# Bar plot of candidate support by population type
ggplot(cleaned_data, aes(x = candidate_name, y = percent, fill = population_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Candidate Support by Population Type", x = "Candidate", y = "Support Percentage (pct)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

candidates |>
  filter(pct > 1) |>
  ggplot(aes(x = sample_size, y = pct, 
             color = candidate_name)) +
  geom_point(size = 1, alpha = 0.2) +
  theme_classic() +
  labs(
    x = "Sample Size",
    y = "Percentage",
    color = "Candidate Name"
  ) +
  scale_color_brewer(palette = "Set1")


#### Model data #####

## Starter models ##
# Model 1: pct as a function of end_date for Trump
model_trump_date <- lm(percent ~ end_date, data = cleaned_data)

# Model 2: pct as a function of end_date and pollster for Trump
model_trump_date_pollster <- lm(percent ~ end_date + pollster_rating_name, data = cleaned_data)

# Augment the data with model predictions
just_trump_high_quality <- cleaned_data |>
  mutate(
    fitted_trump_date = predict(model_trump_date),
    fitted_trump_date_pollster = predict(model_trump_date_pollster)
  )

# Model 1: Plot for Trump based on end_date
ggplot(just_trump_high_quality, aes(x = end_date)) +
  geom_point(aes(y = percent), color = "black", size = 1) +
  geom_line(aes(y = fitted_trump_date), color = "blue", linetype = "dotted") +
  theme_classic() +
  labs(y = "Trump Percent", x = "Date", title = "Linear Model 1:Trump Poll Percent vs. Date")

# Model 2: Plot for Trump based on end_date and pollster
ggplot(just_trump_high_quality, aes(x = end_date)) +
  geom_point(aes(y = percent), color = "black", size = 0.5) +
  geom_line(aes(y = fitted_trump_date_pollster), color = "blue", linetype = "dotted") +
  facet_wrap(vars(pollster_rating_name)) +
  theme_classic() +
  labs(y = "Trump Percent", x = "Date", title = "Linear Model 2: Trump Poll Percent vs. Date by Pollster")

## Bayesian Model ##
just_trump_high_quality2 <- cleaned_data |>
  mutate(
    pollster = factor(pollster_rating_name),    # Convert pollster to a factor
    state = factor(state),          # Convert state to a factor
    num_trump = round((percent / 100) * sample_size, 0)
  )

# Model formula
model_formula_1 <- cbind(num_trump, sample_size - num_trump) ~ (1 | pollster)

# Specify priors
priors <- normal(0, 2.5, autoscale = TRUE)

# Fit Bayesian model using stan_glmer
bayesian_model_1 <- stan_glmer(
  formula = model_formula_1,
  data = just_trump_high_quality2,
  family = binomial(link = "logit"),  # Use binomial family for logistic regression
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)

# Posterior predictive checks
pp_check(bayesian_model_1)

# Summarize the model
summary(bayesian_model_1)

# Model formula with state as an additional random effect
model_formula_2 <- cbind(num_trump, sample_size - num_trump) ~ (1 | pollster) + (1 | state)

# Fit Bayesian model using stan_glmer
bayesian_model_2 <- stan_glmer(
  formula = model_formula_2,
  data = just_trump_high_quality2,
  family = binomial(link = "logit"),  # Use binomial family for logistic regression
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)

# Posterior predictive checks
pp_check(bayesian_model_2)

# Summarize the model
summary(bayesian_model_2)

# Plot random effects for pollster
plot(bayesian_model_2, pars = "(Intercept)", prob = 0.95) + ggtitle("Bayesian Model 1: Pollster Effect") + theme(plot.title = element_text(hjust = 0.5))

# Plot random effects for pollster and state
plot(bayesian_model_2, pars = "(Intercept)", prob = 0.95) + ggtitle("Bayesian Model 2: Pollster Effect and State Effects") + theme(plot.title = element_text(hjust = 0.5))



#### Save model ####
#saveRDS(
#  just_trump_high_quality,
#  file = "models/first_model_1.rds")



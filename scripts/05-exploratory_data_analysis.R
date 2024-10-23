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
library(ggplot2)
library(modelsummary)

#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/cleaned_US_voting.csv")
colnames(analysis_data)

#### Summary the numeric variables ####
summary(analysis_data)
summary(analysis_data$sample_size)
summary(analysis_data$percent)
summary(analysis_data$numeric_grade)

### See the distribution for some useful variables ###
# Histogram of support percentages for Trump
ggplot(analysis_data, aes(x = percent)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Support (percent) for Donald Trump", x = "Support Percentage (percent)", y = "Count") +
  theme_minimal()

# Histogram of sample size to see its distribution
ggplot(analysis_data, aes(x = sample_size)) +
  geom_histogram(binwidth = 100, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Sample Size", x = "Sample Size", y = "Count") +
  theme_minimal()

# Line plot of Trump and Harris's support over time by end data
ggplot(analysis_data, aes(x = as.Date(end_date, format = "%y/%m/%d"), y = percent, color = candidate_name)) +
  geom_line() +
  labs(title = "Candidate Support Over Time", x = "Date", y = "Support Percentage (percent)") +
  theme_minimal()

# Bar plot of Trump support by population type
ggplot(analysis_data, aes(x = candidate_name, y = percent, fill = population_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Candidate Support by Population Type", x = "Candidate", y = "Support Percentage (percent)",
       fill = "Population Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Relationship between Variables ###
# Scatter plot of relationship between gained percent and pollster's grade
analysis_data |>
  ggplot(aes(x = percent, y = numeric_grade)) +
  geom_point() +  
  facet_wrap(vars(pollster_rating_name)) + 
  labs(y = "Numeric Grade", x = "Percent") +
  theme_minimal()

# Scatterplot about size and percentage by methodology
analysis_data |>
  filter(percent > 1) |>
  ggplot(aes(x = sample_size, y = percent, 
             color = candidate_name)) + facet_wrap(vars(methodology)) + theme_minimal() +
  geom_point(size = 1, alpha = 0.2) +
  theme_classic() +
  labs(
    x = "Sample Size",
    y = "Percentage", color = "Candidate Name") + scale_color_brewer(palette = "Set1")

# Stacked Bar Chart of Percent by Numeric Grade and Population Group
analysis_data |>
  filter(numeric_grade > 2.5) |>
  ggplot(aes(x = numeric_grade, y = percent, fill = population_group)) +
  geom_bar(stat = "identity") + 
  theme_classic() +
  labs(
    x = "Numeric Grade",
    y = "Percent",
    title = "Stacked Bar Chart of Percent by Numeric Grade and Population Group",
    fill = "Population Group")


#### Model Data and Construct Some Basic Starter Models #####
colnames(analysis_data)

## Starter models ##
#1. Logistic regression model: regarding to the state, grade and methodology influence on the gained percent
logistic_reg <- glm(percent/100 ~
        state + numeric_grade + methodology, data = analysis_data, family = quasibinomial)

# Summary the logistic regression model
modelsummary(logistic_reg)

#2. Bayesian Model Initial Build up
bayesian_model <- analysis_data |>
  mutate(population = factor(population_group),   
    state = factor(state),         
    num_trump = round((percent / 100) * sample_size, 0))

# Model formula
model_formula_1 <- cbind(num_trump, sample_size - num_trump) ~ (1 | population)

# Specify priors
priors <- normal(0, 2.5, autoscale = TRUE)

# Fit Bayesian model using stan_glmer
bayesian_model_1 <- stan_glmer(
  formula = model_formula_1,
  data = bayesian_model,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95)

# Posterior predictive checks
pp_check(bayesian_model_1)

# Summarize the model
summary(bayesian_model_1)

# Model formula with state as an additional random effect
model_formula_2 <- cbind(num_trump, sample_size - num_trump) ~ (1 | population) + (1 | state)

# Specify priors
priors <- normal(0, 2.5, autoscale = TRUE)

# Fit Bayesian model using stan_glmer
bayesian_model_2 <- stan_glmer(
  formula = model_formula_2,
  data = bayesian_model2,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95)

# Posterior predictive checks
pp_check(bayesian_model_2)

# Summarize the model
summary(bayesian_model_2)

# Plot random effects for pollster
plot(bayesian_model_1, pars = "(Intercept)", prob = 0.95) + 
  ggtitle("Bayesian Model 1: Population Group Effect") + theme(plot.title = element_text(hjust = 0.5))

# Plot random effects for pollster and state
plot(bayesian_model_2, pars = "(Intercept)", prob = 0.95) + ggtitle("Bayesian Model 2: Popultion Effect and State Effects") 
+ theme(plot.title = element_text(hjust = 0.5))


#### Save model ####
#saveRDS(logstic_reg, file = "models/logistic_reg.rds")
#saveRDS(bayesian_model_1, file = "models/bayesian_model_1.rds")
#saveRDS(bayesian_model_2, file = "models/bayesian_model_2.rds")

#### Completion Message ####
print("EDA has completed.")









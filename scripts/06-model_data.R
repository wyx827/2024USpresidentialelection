#### Preamble ####
# Purpose: Model for the Trump's election
# Author: Xuanle Zhou, Yongqi Liu, Yuxuan Wei
# Date: October 19 2024
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

#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/cleaned_US_voting.csv")

### Model data ####
# Convert end_date to a numeric
analysis_data <- analysis_data %>%
  mutate(end_date_num = as.numeric(end_date - min(end_date)))

# Fit Bayesian model with spline and pollster as fixed effect
spline_model <- stan_glm(
  percent ~ ns(end_date_num, df = 9) + pollster_rating_name, 
  data = analysis_data,
  family = gaussian(),
  prior = normal(0, 5),
  prior_intercept = normal(50, 10),
  seed = 1234,
  iter = 2000,
  chains = 4,
  refresh = 0)

# Adding random effects for pollster
spline_model_random <- stan_glmer(
  percent ~ ns(end_date_num, df = 9) + (1 | pollster_rating_name)+ (1 | state),
  data = analysis_data,
  family = gaussian(),
  prior = normal(0, 5),
  prior_intercept = normal(50, 10),
  seed = 1234,
  iter = 2000,
  chains = 4,
  refresh = 0)

# Summarize the model
summary(spline_model)
summary(spline_model_random)

# Posterior predictive checks
pp_check(spline_model)
pp_check(spline_model_random)

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

#---------------------------------------logistic regression----------------
# Convert end_date to a numeric counter and create a binary outcome for winning
analysis_data <- analysis_data %>%
  mutate(
    end_date_num = as.numeric(end_date - min(end_date, na.rm = TRUE)),  # Convert end_date to a numeric counter
    win_prob = ifelse(percent > 50, 1, 0)  # Create a binary outcome for winning
  )

# Make pollster_rating_name a factor
analysis_data$pollster_rating_name <- factor(analysis_data$pollster_rating_name)

# Check the structure to confirm
str(analysis_data)

# Fit a Logistic Regression Model
logistic_model <- stan_glm(
  win_prob ~ ns(end_date_num, df = 9) + pollster_rating_name,
  data = analysis_data,
  family = binomial(link = "logit"),
  prior = normal(0, 2.5),  # Use a reasonable prior
  prior_intercept = normal(0, 2.5),
  seed = 1234,
  iter = 2000,
  chains = 4,
  refresh = 0
)

# Summarize the Model
summary(logistic_model)
#——————-unfinished...











#### Save model ####
saveRDS(
  spline_model_random,
  file = "models/trump_model.rds"
)



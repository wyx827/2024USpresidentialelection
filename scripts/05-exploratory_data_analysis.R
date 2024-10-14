#### Preamble ####
# Purpose: Do exploratory data analyis on the cleaned election data
# Author: Xuanle Zhou, Yongqi Liu, Yuxuan Wei
# Date: 14 October 2024
# Contact: cassieliu.liu@mail.utoronto.ca
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

# Histogram of support percentages (pct) for all candidates
ggplot(cleaned_data, aes(x = pct)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Support (pct) for Candidates", x = "Support Percentage (pct)", y = "Count") +
  theme_minimal()

# Histogram of sample size to see its distribution
ggplot(cleaned_data, aes(x = sample_size)) +
  geom_histogram(binwidth = 50, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Sample Size", x = "Sample Size", y = "Count") +
  theme_minimal()

# Correct way to filter for multiple candidates
candidates <- cleaned_data %>%
  filter(candidate_name %in% c("Kamala Harris", "Donald Trump", "Jill Stein", 
                               "Chase Oliver", "Cornel West"))
# Display the filtered data
print(candidates)

# Line plot of candidate support over time
ggplot(candidates, aes(x = as.Date(start_date, format = "%m/%d/%y"), y = pct, color = candidate_name)) +
  geom_line() +
  labs(title = "Candidate Support Over Time", x = "Date", y = "Support Percentage (pct)") +
  theme_minimal()

# Filter out the candidates with little voter support
filtered_data <- cleaned_data %>%
  filter(!candidate_name %in% c("Chase Oliver", "Claudia De La Cruz"))

# Boxplot to compare support percentages across candidates
ggplot(filtered_data, aes(x = candidate_name, y = pct, fill = candidate_name)) +
  geom_boxplot() +
  labs(title = "Comparison of Support Across Candidates", x = "Candidate", y = "Support Percentage (pct)", fill="Candidate Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# View the updated dataset
head(cleaned_data)

# Bar plot of candidate support by population type
ggplot(cleaned_data, aes(x = candidate_name, y = pct, fill = population_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Candidate Support by Population Type", x = "Candidate", y = "Support Percentage (pct)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# The output shows there are 2 candidates with nearly no votes, so remove them.

# Re-create the bar plot without those two candidates
ggplot(filtered_data, aes(x = candidate_name, y = pct, fill = population_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Candidate Support by Population Type", x = "Candidate", y = "Support Percentage (pct)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

filtered_data |>
  filter(pct > 1) |>
  ggplot(aes(x = sample_size, y = pct, 
             color = candidate_name)) +
  geom_point(size = 2, alpha = 0.5) +
  theme_classic() +
  labs(
    x = "Sample Size",
    y = "Percentage",
    color = "Candidate Name"
  ) +
  scale_color_brewer(palette = "Set1")


### Model data ####
#[\\\\\\\\\\\看一下是否需要在这里初步搭一个大概的model\\\\\\\\\\\\]
first_model <-
  stan_glm(
    formula = flying_time ~ length + width,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
    seed = 853)


#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)



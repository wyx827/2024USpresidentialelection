#### Preamble ####
# Purpose: Downloads and saves the data from https://projects.fivethirtyeight.com/
# Author: Xuanle Zhou, Yongqi Liu, Yuxuan Wei
# Date: 13 October 2024
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: NA
# Any other information needed? NA

#### Workspace setup ####
library(tidyverse)

#### Download data ####
raw_elections_data <-
  read_csv(
    file = 
      "https://projects.fivethirtyeight.com/polls/data/president_polls.csv",
    show_col_types = FALSE,
    skip = 0)

#### Save data ####
write_csv(x = raw_elections_data, file = "data/01-raw_data/US_voting.csv")
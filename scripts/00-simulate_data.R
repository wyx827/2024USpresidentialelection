#### Preamble ####
# Purpose: Simulates a dataset of 2024 US election, to explore 
  #what plausible values might appear in the dataset.
# Author: Xuanle Zhou, Yongqi Liu, Yuxuan Wei
# Date: 18 October 2024
# Contact: isabella.zhou@mail.utoronto.ca
# License: MIT
# Pre-requisites: Construct the overview of how the dataset should look like.
# Any other information needed? Make sure you are in the `2024USpresidentialelection` rproj


#### Workspace setup ####
library(tidyverse)
set.seed(853)

#### Simulate data #### 

#determine the number of observations
num_obs <- 100000

#pollster names
pollster <- c(
  "The New York Times/Siena College", 
  "ABC News/The Washington Post", 
  "Marquette University Law School", "YouGov",
  "Monmouth University Polling Institute", 
  "Marist College", "Suffolk University","Data Orbital", 
  "University of Massachusetts Lowell Center for Public Opinion", 
  "Emerson College", 
  "Muhlenberg College Institute of Public Opinion", 
  "Selzer & Co.", 
  "University of North Florida Public Opinion Research Lab", 
  "CNN","SurveyUSA", "Beacon Research/Shaw & Co. Research", 
  "Quinnipiac University", "MassINC Polling Group", "Ipsos", 
  "Christopher Newport University Wason Center for Civic Leadership", 
  "Siena College","AtlasIntel", "Echelon Insights", 
  "The Washington Post/George Mason University Schar School of Policy and Government", 
  "East Carolina University Center for Survey Research", 
  "Data for Progress", 
  "Hart Research Associates/Public Opinion Strategies",
  "University of New Hampshire Survey Center", 
  "Stockton University William J. Hughes Center for Public Policy", 
  "Remington Research Group", 
  "Mason-Dixon Polling & Strategy",
  "Roanoke College Institute for Policy and Opinion Research", 
  "Fairleigh Dickinson University", 
  "University of Arkansas Department of Political Science", 
  "Lake Research Partners/The Tarrance Group",
  "Public Policy Institute of California", 
  "Michigan State University Institute for Public Policy and Social Research",
  "Elon University", 
  "Southern Illinois University Paul Simon Public Policy Institute", 
  "Pew Research Center",
  "University of Illinois Springfield Survey Research Office", 
  "Western New England University Polling Institute", 
  "High Point University Survey Research Center", "Gallup", 
  "Abt Associates", "The Winston Group", "KFF", 
  "Winthrop University Center for Public Opinion & Policy Research", 
  "The Washington Post/University of Maryland Center for Democracy and Civic Engagement", 
  "University of California Berkeley Institute of Governmental Studies"
)

#methodology
methodology <- c(
  "App Panel", "Live Phone", "Online Ad", "Email", "Probability Panel", 
  "Text", "Text-to-Web", "Mixed", "Online Panel", "IVR" 
)

#grade of pollster
grade <- c(
  `The New York Times/Siena College` = 3.0, 
  `ABC News/The Washington Post` = 3.0, 
  `Marquette University Law School` = 3.0, 
  `YouGov` = 3.0, 
  `Monmouth University Polling Institute` = 2.9, 
  `Marist College` = 2.9, 
  `Suffolk University` = 2.9, 
  `Data Orbital` = 2.9, 
  `University of Massachusetts Lowell Center for Public Opinion` = 2.9, 
  `Emerson College` = 2.9, 
  `Muhlenberg College Institute of Public Opinion` = 2.8, 
  `Selzer & Co.` = 2.8, 
  `University of North Florida Public Opinion Research Lab` = 2.8, 
  `CNN` = 2.8, 
  `SurveyUSA` = 2.8, 
  `Beacon Research/Shaw & Co. Research` = 2.8, 
  `Quinnipiac University` = 2.8, 
  `MassINC Polling Group` = 2.8, 
  `Ipsos` = 2.8, 
  `Christopher Newport University Wason Center for Civic Leadership` = 2.8,
  `Siena College` = 2.7, 
  `AtlasIntel` = 2.7, 
  `Echelon Insights` = 2.7, 
  `The Washington Post/George Mason University Schar School of Policy and Government` = 2.7, 
  `East Carolina University Center for Survey Research` = 2.6, 
  `Data for Progress` = 2.6, 
  `Hart Research Associates/Public Opinion Strategies` = 2.6, 
  `University of New Hampshire Survey Center` = 2.6, 
  `Stockton University William J. Hughes Center for Public Policy` = 2.6, 
  `Remington Research Group` = 2.6, 
  `Mason-Dixon Polling & Strategy` = 2.6, 
  `Roanoke College Institute for Policy and Opinion Research` = 2.6, 
  `Fairleigh Dickinson University` = 2.6, 
  `University of Arkansas Department of Political Science` = 2.5, 
  `Lake Research Partners/The Tarrance Group` = 2.5, 
  `Public Policy Institute of California` = 2.5, 
  `Michigan State University Institute for Public Policy and Social Research` = 2.5, 
  `Elon University` = 2.5, 
  `Southern Illinois University Paul Simon Public Policy Institute` = 2.5, 
  `Pew Research Center` = 2.5, 
  `University of Illinois Springfield Survey Research Office` = 2.5, 
  `Western New England University Polling Institute` = 2.5, 
  `High Point University Survey Research Center` = 2.5, 
  `Gallup` = 2.5, 
  `Abt Associates` = 2.5, 
  `The Winston Group` = 2.5, 
  `KFF` = 2.5, 
  `Winthrop University Center for Public Opinion & Policy Research` = 2.5, 
  `The Washington Post/University of Maryland Center for Democracy and Civic Engagement` = 2.5, 
  `University of California Berkeley Institute of Governmental Studies` = 2.5
)


#us states names
us_states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
  "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
  "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
  "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
  "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
  "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
  "Washington", "West Virginia", "Wisconsin", "Wyoming", "National"
)

#start date and end date
date_range <- seq.Date(from = as.Date("2021-01-20"), to = as.Date("2024-09-20"), by = "day")

#population_group
population_group <- c(
  "likely voters", "registered voters", "adults"
)

# Create a dataset by randomly assigning pollster, methodology, start date, end date, sample size, population_group and percentage
simulated_data <- tibble(
  pollster = sample(pollster, size = num_obs, replace = TRUE),
  methodology = sample(methodology, size = num_obs, replace = TRUE), 
  numerical_grade = grade[pollster],
  start_date = sample(date_range, size = num_obs, replace = TRUE)  # Randomly sample start dates
) %>%
  # Ensure end_date is greater than or equal to start_date
  mutate(
    end_date = start_date + sample(0:180, size = num_obs , replace = TRUE),  # Random end date within 180 days of start date
    sample_size = sample(100:20000, size = num_obs, replace = TRUE) ,
    state = sample(
      us_states, 
      size = num_obs, 
      replace = TRUE
    ),
    candidate_name = "Donald Trump",
    percent = sample(0:100, size = num_obs, replace = TRUE) ,
    population_group = sample(population_group, size = num_obs, replace = TRUE, 
                                        prob = c(0.45, 0.45, 0.1)), 
  )

# Save data
write_csv(simulated_data, "data/00-simulated_data/simulated_data.csv")
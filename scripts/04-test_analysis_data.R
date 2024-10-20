#### Preamble ####
# Purpose: Tests the structure and values of the analysis data 
  #of 2024 US election dataset is in the expected format
# Author: Xuanle Zhou, Yongqi Liu, Yuxuan Wei
# Date: 19 October 2024 
# Contact: isabella.zhou@mail.utoronto.ca
# License: MIT
# Pre-requisites: 02-analysis_data.R must have been run
# Any other information needed? N/A


#### Workspace setup ####
library(tidyverse)
library(testthat)
library(here)

analysis_data <- read_csv(here("data", "02-analysis_data", "cleaned_US_voting.csv"))


#### Test data ####

# Test1:  if the data was successfully loaded
test_that("Test if the data was successfully loaded", {
  expect_true(exists("analysis_data"), 
              info = "Test Failed: The dataset could not be loaded.")
  message("Test Passed: The dataset was successfully loaded.")
})


# Test2: Check if the dataset has 10 columns
test_that("Dataset has the correct number of columns", {
  expected_columns <- 10 
  actual_columns <- ncol(analysis_data)  # Get the actual number of columns
  expect_equal(actual_columns, expected_columns, 
               info = paste("Expected", expected_columns, "columns but got", actual_columns))
})


#Test3: Check pollster columns are in expected names #
expected_pollsters <- c( "The New York Times/Siena College", 
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
                         "University of California Berkeley Institute of Governmental Studies")
# Test to check if pollster column only contains expected names
test_that("All actual pollsters are in the expected names", {
  actual_pollsters <- unique(analysis_data$`pollster_rating_name`)
  
  # Function to check for partial matches
  matches_expected <- function(actual) {
    # Split the actual name into words
    actual_words <- str_split(actual, "\\s+")[[1]]
    
    # Check if any word in the actual name is contained in any expected name
    any(sapply(expected_pollsters, function(expected) {
      any(str_detect(expected, fixed(actual_words, ignore_case = TRUE)))
    }))
  }
  
  # Check for actual pollsters that do not match
  missing_pollsters <- actual_pollsters[!sapply(actual_pollsters, matches_expected)]
  
  expect_true(length(missing_pollsters) == 0, 
              info = paste("The following actual pollsters are not in the expected names:", 
                           paste(missing_pollsters, collapse = ", ")))
})


# Test 4: Check that the methodology columns are in the expected methodology#
expected_methodology <- c(  
  "App Panel", "Live Phone", "Online Ad", "Email", "Probability Panel", 
  "Text", "Text-to-Web", "Mixed", "Online Panel", "IVR" 
)

test_that("Methdology column contains only expected methodology", { 
  actual_methodology <- unique(analysis_data$`methodology`) 
  # Check that all actual methodologies are in the expected ones
  expect_true(all(actual_methodology %in% expected_methodology), 
              info = "Some actual methodologies are not in the expected list")
})

# Test 5: Check if the 'numerical grade' column is equal or higher than 2.5
test_that("Numerical grade column is equal to or higher than 2.5", {
  # Check that there are no values in the 'numerical grade' column lower than 2.5
  expect_true(all(analysis_data$numeric_grade >= 2.5), 
              info = "Some grades are lower than 2.5")
})

# Test 6: Check if end_date is greater than or equal to start_date
test_that("End date is greater than or equal to start date", {
  # Check that all values in 'end_date' are greater than or equal to 'start_date'
  expect_true(all(analysis_data$end_date >= analysis_data$start_date), 
              info = "Some end dates are earlier than start dates")
})

# Test 7: Check the sample size is in teh range of 100 to 20,000
test_that("Sample size is within the range of 100 to 20,000", {
  expect_true(all(analysis_data$sample_size >= 100 & analysis_data$sample_size <= 20000),
              info = "Some sample sizes are outside the range of 100 to 20,000.")
})

# Test 8: Check if state column contains all states in US
expected_state <- c(
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
test_that("State column contains all states in the US", {
  actual_state <- unique(analysis_data$`state`)
  
  # Check that all actual states are in the expected states (less strict matching)
  expect_true(all(actual_state %in% expected_state), info = paste("Actual states not in expected:", 
                                                                  paste(actual_state[!(actual_state %in% expected_state)], collapse = ", ")))
})


# Test 9: Check if candidate name only have Donald Trump

test_that("Candidate name only contains 'Donald Trump'", {
  # Check that all values in 'candidate_name' are "Donald Trump"
  expect_true(all(analysis_data$candidate_name == "Donald Trump"), 
              info = "Candidate name contains values other than 'Donald Trump'")
})

#Test 10: Check the percent is in range of 0 to 100
test_that("percent column is within the range of 0 to 100", {
  expect_true(all(analysis_data$percent >= 0 & analysis_data$percent <= 100),
              info = "Some sample sizes are outside the range of 0 to 100.")
})

# Test 11: Check if the population group only have expected groups
expected_group <- c(
  "likely voters", "registered voters", "adults"
)

test_that("populatioin group column contains expected groups", {
  actual_group <- unique(analysis_data$`population_group`)  
  # Check that the actual pollsters match the expected ones
  expect_setequal(actual_group, expected_group)
})

# Test 12: Check if the dataset has missing values in columns
test_that("No missing values in critical columns", {
  expect_equal(sum(is.na(analysis_data$`pollster_rating_name`)), 0)
  expect_equal(sum(is.na(analysis_data$`methodology`)), 0)
  expect_equal(sum(is.na(analysis_data$`numeric_grade`)), 0)
  expect_equal(sum(is.na(analysis_data$`start_date`)), 0)
  expect_equal(sum(is.na(analysis_data$`end_date`)), 0)
  expect_equal(sum(is.na(analysis_data$`sample_size`)), 0)
  expect_equal(sum(is.na(analysis_data$`candidate_name`)), 0)
  expect_equal(sum(is.na(analysis_data$`percent`)), 0)
  expect_equal(sum(is.na(analysis_data$`population_group`)), 0)
})

#Test 13: Check if the values are in the expcted format
test_that("Column types are correct", {
  expect_is(analysis_data$`pollster_rating_name`, "character")
  expect_is(analysis_data$`methodology`, "character")
  expect_is(analysis_data$`numeric_grade`, "numeric")
  expect_is(analysis_data$`start_date`, "Date")
  expect_is(analysis_data$`end_date`, "Date")
  expect_is(analysis_data$`sample_size`, "numeric")
  expect_is(analysis_data$`candidate_name`, "character")
  expect_is(analysis_data$`percent`, "numeric")
  expect_is(analysis_data$`population_group`, "character")
})


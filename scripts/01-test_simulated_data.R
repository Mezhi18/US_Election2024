#### Preamble ####
# Purpose: Tests the structure and validity of the simulated American election data
# Author: Sakura Noskor, Yan Mezhiborsky, Cristina Burca
# Date: 3 November 2024
# Contact: cristina.burca@mail.utoronto.ca, sakura.noskor@mail.utoronto.ca,  yan.mezhiborsky@mail.utoronto.ca
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded
  # - 00-simulate_data.R must have been run



install.packages("validate")
install.packages("testthat")

#### Workspace setup ####
library(tidyverse)

# Load necessary libraries
# Load necessary libraries
library(testthat)
library(validate)
library(readr)

# Load the simulated data
poll_data <- read_csv("data/00-simulated_data/simulated_data.csv")

# Define validation rules
rules <- validator(
  n_polls = nrow(poll_data) == 100,
  n_cols = ncol(poll_data) == 9,
  is_character_pollster = all(sapply(poll_data$pollster, is.character)),
  is_numeric_sample_size = all(sapply(poll_data$sample_size, is.numeric)),
  is_character_state = all(sapply(poll_data$state, is.character)),
  is_character_candidate = all(sapply(poll_data$candidate_name, is.character)),
  is_numeric_pct = all(sapply(poll_data$pct, is.numeric)),
  is_date_start = all(sapply(poll_data$start_date, is.Date)),
  is_date_end = all(sapply(poll_data$end_date, is.Date)),
  is_numeric_pollscore = all(sapply(poll_data$pollscore, is.numeric)),
  is_numeric_grade = all(sapply(poll_data$numeric_grade, is.numeric)),
  pollster_in_set = all(poll_data$pollster %in% c("YouGov", "New York Times", "Ipsos", "The Washington Post", "1892 Polling")),
  sample_size_in_set = all(poll_data$sample_size %in% c(800, 1000, 1200, 1500, 2000)),
  state_in_set = all(poll_data$state %in% c("Arizona", "Georgia", "Wisconsin", "Nevada", "Pennsylvania", "North Carolina", "Michigan")),
  candidate_in_set = all(poll_data$candidate_name %in% c("Donald Trump", "Kamala Harris", "Jill Stein", "Chase Oliver")),
  pct_range = all(poll_data$pct >= 40 & poll_data$pct <= 60),
  start_before_end = all(poll_data$start_date < poll_data$end_date),
  pollscore_negative = all(poll_data$pollscore < 0),
  grade_range = all(poll_data$numeric_grade >= 2.0 & poll_data$numeric_grade <= 5.0)
)

# Validate the data
validation_results <- confront(poll_data, rules)

# View validation results
print(validation_results)


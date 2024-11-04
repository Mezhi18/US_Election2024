#### Preamble ####
# Purpose: Simulates a dataset of American election
# Author: Sakura Noskor, Yan Mezhiborsky, Cristina Burca
# Date: 3 November 2024
# Contact: cristina.burca@mail.utoronto.ca, sakura.noskor@mail.utoronto.ca,  yan.mezhiborsky@mail.utoronto.ca
# Pre-requisites: The `tidyverse` package must be installed



#### Workspace setup ####
library(tidyverse)
# Load necessary library
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Parameters for the simulation
num_polls <- 100
pollsters <- c("YouGov", "New York Times", "Ipsos", "The Washington Post", "1892 Polling")
states <- c("Arizona", "Georgia", "Wisconsin", "Nevada", "Pennsylvania", "North Carolina", "Michigan")
candidates <- c("Donald Trump", "Kamala Harris", "Jill Stein", "Chase Oliver")

# Generate simulated data
poll_data <- data.frame(
  pollster = sample(pollsters, num_polls, replace = TRUE),
  sample_size = sample(c(800, 1000, 1200, 1500, 2000), num_polls, replace = TRUE),
  state = sample(states, num_polls, replace = TRUE),
  candidate_name = sample(candidates, num_polls, replace = TRUE),
  pct = round(runif(num_polls, 40, 60), 1),  # Simulate percentages between 40 and 60
  start_date = sample(seq(as.Date('2024-10-01'), as.Date('2024-10-10'), by="days"), num_polls, replace = TRUE),
  end_date = sample(seq(as.Date('2024-10-11'), as.Date('2024-10-20'), by="days"), num_polls, replace = TRUE),
  pollscore = round(runif(num_polls, -2.0, 0), 1),  # Simulate poll scores
  numeric_grade = round(runif(num_polls, 2.0, 5.0), 1)  # Simulate numeric grades
)

# View the first few rows of the simulated data
head(poll_data)


#### Save data ####
write_csv(poll_data, "data/00-simulated_data/simulated_data.csv")

#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(rstanarm)

#### Read data ####
preddata< - read_csv(preddata, "./data/02-analysis_data/cleaned.csv")

harrisdata <- preddata %>%
  filter(state %in% c("Georgia", "Arizona", "Nevada", "Michigan", "North Carolina", "Pennsylvania", "Wisconsin")) %>%
  filter(candidate_name == "Kamala Harris") %>%
  mutate(end_date = mdy(end_date)) %>%
  filter(end_date >= as.Date("2024-07-21")) %>%
  mutate(num_harris = round((pct / 100) * sample_size, 0))

### Model data ####
harrisdata <- harrisdata %>%
  mutate(
    end_date_num = as.numeric(end_date - min(end_date)))

harrisdata <- harrisdata |>
  mutate(state = factor(state))

# Define Model
model <- cbind(num_harris, sample_size - num_harris) ~ (1 | state) + (1 | pollster) + (1 | end_date_num)

priors <- normal(0, 2.5, autoscale = TRUE)

bayesian_model <- stan_glmer(
  formula = model,
  data = harrisdata,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  weights = harrisdata$numeric_grade,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95)



#### Save model ####
saveRDS(
  bayesian_model,
  file = "models/bay_model.rds"
)



#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Sakura Noskor, Yan Mezhiborsky, Cristina Burca
# Date: 3 November 2024
# Contact: cristina.burca@mail.utoronto.ca, sakura.noskor@mail.utoronto.ca,  yan.mezhiborsky@mail.utoronto.ca


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



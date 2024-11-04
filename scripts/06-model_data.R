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

trumpdata <- preddata %>%
  filter(state %in% c("Georgia", "Arizona", "Nevada", "Michigan", "North Carolina", "Pennsylvania", "Wisconsin")) %>%
  filter(candidate_name == "Donald Trump") %>%
  mutate(end_date = mdy(end_date)) %>%
  filter(end_date >= as.Date("2024-07-21")) %>%
  mutate(num_trump = round((pct / 100) * sample_size, 0))

ggplot(trumpdata, aes(x = reorder(state, pct, median), y = pct, fill = state)) +
  geom_boxplot() +
  coord_flip() +  # Flip for better readability
  labs(x = "State", y = "Support Percentage") +
  theme_minimal() +
  theme(legend.position = "none")

trumpdata <- trumpdata %>%
  mutate(
    end_date_num = as.numeric(end_date - min(end_date)))

trumpdata <- trumpdata |>
  mutate(state = factor(state))

# Define Model
model2 <- cbind(num_trump, sample_size - num_trump) ~ (1 | state) + (1 | pollster) + (1 | end_date_num)

priors <- normal(0, 2.5, autoscale = TRUE)

bayesian_model2 <- stan_glmer(
  formula = model2,
  data = trumpdata,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  weights = trumpdata$numeric_grade,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95)


#### Save model ####
saveRDS(
  bayesian_model2,
  file = "models/bayesian_model2.rds"
)



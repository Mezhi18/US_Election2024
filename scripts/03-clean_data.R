#### Preamble ####
# Purpose: Cleans the raw election data by filtering and deleting rows
# Author: Sakura Noskor, Yan Mezhiborsky, Cristina Burca
# Date: 3 November 2024
# Contact: cristina.burca@mail.utoronto.ca, sakura.noskor@mail.utoronto.ca,  yan.mezhiborsky@mail.utoronto.ca

#### Workspace setup ####
library(tidyverse)
library(arrow)
library(janitor)


#### Clean data ####
data <- read.csv("./data/01-raw_data/raw_data.csv") |>
  clean_names()

preddata <- data %>%
  filter(numeric_grade >1.5 & transparency_score >6) %>%
  select(pollster, sample_size, state, candidate_name, pct, start_date, end_date, pollscore, numeric_grade) %>%
  filter(!is.na(pollscore)) %>%
  filter(!is.na(state) & state != "")



#### Save data ####
write_csv(preddata, "./data/02-analysis_data/cleaned.csv")
arrow::write_parquet(preddata, "./data/02-analysis_data/cleaned.parquet")

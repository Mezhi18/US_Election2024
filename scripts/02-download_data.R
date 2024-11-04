#### Preamble ####
# Purpose: Downloads and saves the data from [data](https://projects.fivethirtyeight.com/polls/president-general/2024/national/)
# Author: Sakura Noskor, Yan Mezhiborsky, Cristina Burca
# Date: 3 November 2024
# Contact: cristina.burca@mail.utoronto.ca, sakura.noskor@mail.utoronto.ca,  yan.mezhiborsky@mail.utoronto.ca
# Pre-requisites: go to the site above and search for “Download the data”, then select Presidential general election polls (current cycle), then “Download”



library(tidyverse)

# Load necessary libraries
library(testthat)
library(validate)
library(readr)


# Load the simulated data
#the data is already downloaded in the repository. 
data <- read.csv("../data/01-raw_data/raw_data.csv")

#### Save data ####



         

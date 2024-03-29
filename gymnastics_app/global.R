library(shiny)
library(bslib)
library(DT)
library(dplyr)

# load in data
female_data <- readRDS("data/female_results.RDS")
male_data <- readRDS("data/male_results.RDS")

female_detailed <- readRDS("data/female_medals.RDS")
male_detailed <- readRDS("data/male_medals.RDS")

# define number of trials as a parameter
n_w_trials <- length(female_detailed[['team_combo_1']])
n_m_trials <- length(male_detailed[['team_combo_1']])

# get unique competitors
all_female_athletes <- unique(c(female_data$athlete_1, female_data$athlete_2, female_data$athlete_3, female_data$athlete_4, female_data$athlete_5))
all_male_athletes <- unique(c(male_data$athlete_1, male_data$athlete_2, male_data$athlete_3, male_data$athlete_4, male_data$athlete_5))

# create subsetted data
sub_female_data <- female_data %>% dplyr::select(athlete_1, athlete_2, athlete_3, athlete_4, athlete_5)
sub_male_data   <- male_data %>% dplyr::select(athlete_1, athlete_2, athlete_3, athlete_4, athlete_5)



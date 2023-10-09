
# load in data
female_data <- readRDS("data/female_results.RDS")
male_data <- readRDS("data/male_results.RDS")

# get unique competitors
all_female_athletes <- unique(c(female_data$athlete_1, female_data$athlete_2, female_data$athlete_3, female_data$athlete_4, female_data$athlete_5))
all_male_athletes <- unique(c(male_data$athlete_1, male_data$athlete_2, male_data$athlete_3, male_data$athlete_4, male_data$athlete_5))
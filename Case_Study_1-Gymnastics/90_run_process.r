## This script sources the sub-scripts needed for our project!

# Grab data
source("Case_Study_1-Gymnastics/00_get_data.r")

# Prepare and clean data for modeling 
source("Case_Study_1-Gymnastics/10_prep_data.r")

# Run initial models to predict scores and variance for each athlete
source("Case_Study_1-Gymnastics/20_fit_model.r")

# Prepare for team selection (create combinations of teams, etc)
source("Case_study_2-Gymnastics/30_prepare_to_select.r")

# Use predicted scores and additional considerations/probabilities to select team
source("Case_Study_1-Gymnastics/40_select_female_team.r")
source("Case_Study_1-Gymnastics/40_select_male_team.r")

# Last step: Create final output (not sure what this will look like yet exactly, 
# so not creating a script for now)

## This script sources the sub-scripts needed for our project!

#==================#
#=== PARAMETERS ===#
#==================#

# set relevant parameters
trials <- 10
n_team_combos <- NA # if you only want to run some team combos instead of all

womens_apparatus <- c('fx', 'vt', 'bb', 'ub')
mens_apparatus   <- c('fx', 'vt', 'hb', 'pb', 'sr', 'ph')

# select countries that will compete in the Olympics
womens_known_qualifiers <- c('USA', 'CAN', 'GBR')
mens_known_qualifiers   <- c('CHN', 'JPN', 'GBR')

#===================#
#=== RUN SCRIPTS ===#
#===================#

# Grab data
source("Case_Study_1-Gymnastics/00_get_data.r")

# Prepare and clean data for modeling 
source("Case_Study_1-Gymnastics/10_prep_data.r")

# Run initial models to predict scores and variance for each athlete
source("Case_Study_1-Gymnastics/20_fit_model.r")

# Prepare for team selection (create combinations of teams, etc)
source("Case_Study_1-Gymnastics/30_prepare_to_select.r")
source("Case_Study_1-Gymnastics/35_define_team_selection_functions.r")

# Use predicted scores and additional considerations/probabilities to select team
source("Case_Study_1-Gymnastics/40_select_female_team.r")
source("Case_Study_1-Gymnastics/40_select_male_team.r")

# Last step: Create final output (not sure what this will look like yet exactly, 
# so not creating a script for now)

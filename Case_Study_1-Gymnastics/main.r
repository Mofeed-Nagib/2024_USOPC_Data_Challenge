## This script sources the sub-scripts needed for our project!

# Grab data
source("Case_Study_1-Gymnastics/get.data.r")

# Prepare and clean data for modeling 
source("Case_Study_1-Gymnastics/prep.data.r")

# Run initial models to predict scores and variance for each athlete
source("Case_Study_1-Gymnastics/fit.model.r")

# Use predicted scores and additional considerations/probabilities to select team
source("Case_Study_1-Gymnastics/select.team.r")

# Last step: Create final output (not sure what this will look like yet exactly, 
# so not creating a script for now)

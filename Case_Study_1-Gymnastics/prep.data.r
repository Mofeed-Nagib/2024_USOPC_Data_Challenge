## This script cleans the raw gymnastics data in preparation for modeling.
source("Case_Study_1-Gymnastics/get.data.r")

in_earlier_scores
in_later_scores
# Create function to clean data
clean_data <- function(in_data) {
  
  # Make working copy of data
  gym_data <- in_data
  
  # Format column names
  colnames(gym_data) <- tolower(colnames(gym_data))
  
  # Format gymnast names, fix capitalization
  gym_data <- gym_data %>% mutate(firstname = str_to_title(firstname),
                                  lastname  = str_to_title(lastname))
  
  # Fix dates
  # get rid of day of the week
  
  # handle ranges of dates
  
  # make them Date types
}
# Run function on both datasets
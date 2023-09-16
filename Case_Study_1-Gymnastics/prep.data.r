## This script cleans the raw gymnastics data in preparation for modeling.

in_earlier_scores
in_later_scores

# things to ask Brian:
## how do we make a distribution using lack of data points

# Create function to clean data
clean_data <- function(in_data) {
  
  # Make working copy of data
  gym_data <- in_data
  
  # Format column names
  colnames(gym_data) <- tolower(colnames(gym_data))
  
  # Fix location names
  gym_data$location <- gsub("\\.", "", gym_data$location)
  
  # Fix country codes
  gym_data$country <- gsub("\\<GE1\\>|\\<GE2\\>", "GER", gym_data$country)
  gym_data$country[gym_data$country == ""] <- NA
  
  # Format gymnast names, fix capitalization
  gym_data <- gym_data %>% mutate(firstname = str_to_title(firstname),
                                  lastname  = str_to_title(lastname))
  
  # Combine gymnast first and last names
  gym_data$fullname <- trimws(paste(gym_data$firstname, gym_data$lastname))
  
  # Fix penalties NAs be 0s
  gym_data$penalty[is.na(gym_data$penalty)] <- 0
  
  # Remove observations with NAs
  gym_data <- gym_data %>% filter(!is.na(score))
  
  # Fix apparatus names
  gym_data$apparatus <- gsub("\\<hb\\>", "HB", gym_data$apparatus)
  gym_data$apparatus <- gsub("\\<VT1\\>|\\<VT2\\>", "VT", gym_data$apparatus)
  
  # Fix dates
  gym_data <- gym_data %>%
    # get rid of days of the week
    mutate(date = gsub("sun|thu", "", date, ignore.case = TRUE)) %>% 
    
    # handle ranges of dates
    mutate(date = gsub(".*[-]", "", date)) %>% 
  
    # fix capitalization, remove extra whitespace
    mutate(date = str_trim(str_to_title(date))) %>% 
    
    # a few other adhoc fixes
    mutate(date = gsub("July", "Jul", date)) %>% 
    mutate(date = gsub(",", "", date)) %>% 
    
    # convert the month name into month number
    mutate(date = strptime(gsub(" ", "-", date), format = "%d-%b-%Y")) %>% 
  
    # make them Date types
    mutate(date = as.Date(date))
  
  # error catch for date handling
  if (any(is.na(gym_data$date))) {message("Warning: dates may not have processed properly. Some dates now NA.")}
  
  # Remove rows that are exact duplicates
  gym_data <- gym_data[!duplicated(gym_data), ]
    
  return(gym_data)
}

# Run function on both datasets
# earlier_scores <- clean_data(in_earlier_scores)
later_scores   <- clean_data(in_later_scores)

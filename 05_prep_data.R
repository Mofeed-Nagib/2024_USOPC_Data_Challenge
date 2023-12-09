## This script cleans the raw gymnastics data in preparation for modeling.

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
  gym_data$country <- gsub("\\<ENG\\>", "GBR", gym_data$country)
  gym_data$country <- gsub("NIR", "IRL", gym_data$country)
  gym_data$country <- gsub("SIN", "SGP", gym_data$country)
  gym_data$country[gym_data$country == ""] <- NA
  
  # Format gymnast names, fix capitalization
  gym_data <- gym_data %>% 
              mutate(firstname = str_to_title(firstname),
                     firstname = str_trim(firstname, side = "both"),
                     lastname  = str_to_title(lastname),
                     lastname = str_trim(lastname, side = "both"))
  
  # Combine gymnast first and last names
  gym_data$fullname <- trimws(paste(gym_data$firstname, gym_data$lastname))
  
  # Calculate NA penalty values based on scores
  gym_data$penalty[which(is.na(gym_data$penalty))] <- pmax(0, round(gym_data$d_score[which(is.na(gym_data$penalty))] +
                                                                    gym_data$e_score[which(is.na(gym_data$penalty))] -
                                                                    gym_data$score[which(is.na(gym_data$penalty))], 3))
  
  # Filter competition locations to country
  gym_data$location <- gsub(".*, ", "", gym_data$location)
  # Change US states to be US
  gym_data$location <- sub("Utah|Illinois|FL|CA|Kentucky|Texas", "United States", gym_data$location)
  # Substitute England for United Kingdom
  gym_data$location <- sub("England", "United Kingdom", gym_data$location)
  # Substitute country codes
  gym_data$location <- countrycode::countrycode(gym_data$location, "country.name.en", "ioc")
  
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
    mutate(date = gsub("June", "Jun", date)) %>%
    mutate(date = gsub("July", "Jul", date)) %>%
    mutate(date = gsub("Sept", "Sep", date)) %>%
    mutate(date = gsub(",", "", date)) %>% 
    
    # convert the month name into month number
    mutate(date = strptime(gsub(" ", "-", date), format = "%d-%b-%Y")) %>% 
  
    # make them Date types
    mutate(date = as.Date(date))
  
  # error catch for date handling
  if (any(is.na(gym_data$date))) {
    message("Warning: dates may not have processed properly. Some dates now NA.")}
  
  # Remove rows that are exact duplicates
  gym_data <- gym_data[!duplicated(gym_data), ]
    
  return(gym_data)
}

# Run function on both datasets
earlier_scores <- clean_data(in_earlier_scores)
later_scores   <- clean_data(in_later_scores)

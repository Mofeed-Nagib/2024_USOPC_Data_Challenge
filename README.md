# UCSAS 2024 USOPC Data Challenge


## File Hierarchy

* **00_get_data.R:** This R script initializes all the necessary R packages for our data processing and analytics model, and reads in the raw gymnastics score files stored in the data folder
* **05_prep_data.R:** This R script contains a data cleaning function that creates a full name column (combining the first and last name of gymnasts), cleans gymnast country, competition location, competition date, calculates penalty, and performs other necessary data preprocessing steps
* **10_clean_name.R:** This R script is where we go through the full names of gymnasts country-by-country and clean up any minor misspellings or repeats
* **15_clean_qualifiers.R:** This R script contains more intensive data cleaning for all the known individual qualifiers
* **20_mixed_effects_model.R:** This R script contains the code for our mixed effects models, specifically one for each gender and apparatus, so 4 for female (Vault, Uneven Bars,  Balance Beam, Floor Exercise) and 6 for male (Floor Exercise, Pommel Horse, Still Rings, Vault, Parallel Bars, Horizontal Bar)
* **25_fit_model.R:** This R script builds a distribution of scores for each gymnast and apparatus using the gymnasts' empirical scores
* **30_prepare_to_select.R:** This R script prepares for simulations by: selecting the teams and individual athletes we believe will make it to the Olympics, creating all possible 'teams' (combinations of athletes), and paring down to all 'reasonable' combinations
* **35_define_team_selection_function.R:** This R script defines some helper functions for the team selection / simulation scripts
* **40_select_female_team.R:** This R script uses the distributions we created / scores we predicted in previous scripts to select the optimal female gymnastics team (i.e. it runs the simulation for female gymnasts)
* **40_select_male_team.R:** This R script uses the distributions we created / scores we predicted in previous scripts to select the optimal male gymnastics team (i.e. it runs the simulation for male gymnasts)
* **50_export_output.R:** This R script saves the simulation output in the appropriate folders for male and female
* **90_run_process.R:** This R script holds several of our global variables and runs the analysis, builds the model, and simulates the output (i.e. calls all the previous files)
* **Case_Study_1-Gymnastics.Rproj:**
* **data/**
	* **data_2017_2021.csv:** This CSV file is the older gymnast data downloaded from https://github.com/ucsas/gym2024data/tree/main/cleandata
	* **data_2022_2023.csv:** This CSV file is the newer gymnast data downloaded from https://github.com/ucsas/gym2024data/tree/main/cleandata
* **final_output/**
	* **female_results.csv:** This CSV file contains the simulation results for the female gymnasts (a dataframe with all 126 combinations and 100 trials for each combination)
	* **female_results.RDS:** This RDS object file contains the simulation results for the female gymnasts (a dataframe with all 126 combinations and 100 trials for each combination)
	* **male_results.csv:** This CSV file contains the simulation results for the male gymnasts (a dataframe with all 1287 combinations and 100 trials for each combination)
	* **male_results.RDS:** This RDS object file contains the simulation results for the male gymnasts (a dataframe with all 1287 combinations and 100 trials for each combination)
* **gymnastics_app/**
	* **data/**
		* **female_medals.RDS:** This RDS object file contains 
		* **female_results.csv:** This CSV file contains the simulation results for the female gymnasts (a dataframe with all 126 combinations and 100 trials for each combination)
		* **female_results.RDS:** This RDS object file contains the simulation results for the female gymnasts (a dataframe with all 126 combinations and 100 trials for each combination)
		* **male_medals.RDS:** This RDS object file contains 
		* **male_results.csv:** This CSV file contains the simulation results for the male gymnasts (a dataframe with all 1287 combinations and 100 trials for each combination)
		* **male_results.RDS:** This RDS object file contains the simulation results for the male gymnasts (a dataframe with all 1287 combinations and 100 trials for each combination)
	* **global.R:** This R script reads in the simulation output data to be displayed by our Shiny application
	* **server.R:** This R script holds the backend code for our Shiny application
	* **ui.R:** This R script holds the frontend code for our Shiny application

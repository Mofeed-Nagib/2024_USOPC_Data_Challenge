## This script uses the distributions we created / scores we predicted in previous scripts
## to select the optimal gymnastics team!

#=================================#
#=== athlete/country selection ===#
#=================================#

# select countries that will compete in the Olympics
womens_known_qualifiers <- c('USA', 'CAN', 'GBR')
mens_known_qualifiers <- c('CHN', 'JPN', 'GBR')

# for each athlete, average all of each player's scores
later_player_scores <- later_scores %>% 
                       group_by(fullname, country, gender) %>% 
                       summarise(avg_score = mean(score))

# for teams, select top 5 avg scores for each country and sum together to get a team score
country_sums <- later_player_scores %>% 
                group_by(country, gender) %>% 
                filter(length(unique(fullname)) >= 5 & !is.na(country)) %>% 
                slice_max(avg_score, n = 5) %>% 
                summarise(sum_score = sum(avg_score))

# pick top 9 teams (that haven't already qualified) based on estimated team score
top_men   <- country_sums %>% 
             filter(gender == 'm' & !(country %in% mens_known_qualifiers)) %>% 
             arrange(-sum_score) %>% head(9)

top_women <- country_sums %>% 
             filter(gender == 'w' & !(country %in% womens_known_qualifiers)) %>% 
             arrange(-sum_score) %>% head(9)

# concatenate countries that have already qualified and our estimated qualifiers
men_countries   <- c(mens_known_qualifiers, top_men$country)
women_countries <- c(womens_known_qualifiers, top_women$country)

# determine 5 person teams by taking 5 athletes with highest scores from each country
men_athletes   <- later_player_scores %>% 
                  filter(country %in% men_countries & country != 'USA' & gender == 'm') %>% 
                  group_by(country) %>% 
                  slice_max(avg_score, n = 5) %>% 
                  mutate(flag_team = 1)

women_athletes <- later_player_scores %>% 
                  filter(country %in% women_countries & country != 'USA' & gender == 'w') %>% 
                  group_by(country) %>% 
                  slice_max(avg_score, n = 5) %>% 
                  mutate(flag_team = 1)

# determine the best 36 gymnasts whose teams did not qualify (maximum of 3 individuals per country)
best_men_dnq   <- later_player_scores %>% 
                  filter(!(country %in% men_countries) & gender == 'm') %>% 
                  group_by(fullname, country) %>% 
                  arrange(-avg_score) %>% 
                  head(36) %>% 
                  mutate(flag_team = 0)

best_women_dnq <- later_player_scores %>% 
                  filter(!(country %in% women_countries) & gender == 'w') %>% 
                  group_by(fullname, country) %>% 
                  arrange(-avg_score) %>% 
                  head(36) %>% 
                  mutate(flag_team = 0)

#===================#
#=== simulations ===#
#===================#
# start by getting names of us male and female athletes
us_males <- later_scores %>% 
            filter(country == 'USA' & gender == 'm') %>% 
            distinct(fullname) %>% 
            pull(fullname)

us_women <- later_scores %>% 
            filter(country == 'USA' & gender == 'w') %>% 
            distinct(fullname) %>% 
            pull(fullname)

# find all possible 5 person teams for the usa 
male_us_teams  <- combn(us_males, 5, simplify = F)
women_us_teams <- combn(us_women, 5, simplify = F)

# convert teams into dataframes 
df_male_us_teams   <- setNames(as.data.frame(do.call(rbind, male_us_teams)), paste0("athlete_", 1:5))
df_female_us_teams <- setNames(as.data.frame(do.call(rbind, women_us_teams)), paste0("athlete_", 1:5))

# add columns for simulation results
col_names <- paste0('simulation_', 1:1000)
df_male_us_teams[ , col_names]   <- NA
df_female_us_teams[ , col_names] <- NA

# Run female simulations
# test
# team_combo <- 1
for (team_combo in 1:nrow(df_female_us_teams)) {

  # get US athletes
  current_us_athletes <- data.frame(fullname = as.character(df_female_us_teams[team_combo,1:5]), country = 'USA', flag_team = 1)
  
  # making a dataframe of all competing athletes
  current_athletes <- rbind(women_athletes[,c('fullname', 'country', 'flag_team')],
                            best_women_dnq[,c('fullname', 'country', 'flag_team')],
                            current_us_athletes)
  
  # for each athlete, get mean scores for each apparatus
  # define function to get each ath'ete's mean scores for each apparatus
  # test
  # in_athlete <- as.character(current_athletes[1, 'fullname'])
  get_mean_scores <- function(in_athlete) {
    current_athletes <- current_athletes %>% 
                        filter(fullname == in_athlete) %>% 
                        mutate(fx_mean = gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'FX', 'mean'])
  }
  
  ## QUALIFYING ROUND
  # Rule: 4 of the 5 athletes on each team will compete on each appartus
  # Individual athletes can compete on all apparatuses, so let's just assume they do that ('worst case')
  
  # Pick the 4 athletes for each country that will compete on each apparatus
  
  

}
# Use distributions of earlier/later scores to simulate Olympic performances of other teams
# Maybe factor in additional considerations: potential injury, experience level. etc??

# Run through different possible US team constructions (is there a more efficient way than like looping through them? how to do this?)
# Simulate US team performance

# Create individual all around scores

# Create team scores (note to self: would we need to simulate through a qualifying round?)

# measure performance of US teams (by calculating a weighted medal count)

# Keep track of which USA team construction performs best

# At end of simulations, compare performances of different US team constructions

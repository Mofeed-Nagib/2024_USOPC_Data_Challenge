## This script uses the distributions we created / scores we predicted in previous scripts
## to select the optimal gymnastics team!

## Select countries that will compete in the Olympics
womens_known_qualifiers <- c('USA', 'CAN', 'GBR')
mens_known_qualifiers <- c('CHN', 'JPN', 'GBR')

# for each athlete
# average all of each player's score
# for teams, select top 5 for each country and sum together to get a team score
later_player_scores <- later_scores %>% group_by(fullname, country, gender) %>% summarise(avg_score = mean(score))

country_sums <- later_player_scores %>% group_by(country, gender) %>% filter(length(unique(fullname)) >= 5 & !is.na(country)) %>% slice_max(avg_score, n = 5) %>% summarise(sum_score = sum(avg_score))

top_men <- country_sums %>% filter(gender == 'm' & !(country %in% mens_known_qualifiers)) %>% arrange(-sum_score) %>% head(9)
top_women <- country_sums %>% filter(gender == 'w' & !(country %in% womens_known_qualifiers)) %>% arrange(-sum_score) %>% head(9)

# Run simulations
# In each simulation:

# Use distributions of earlier/later scores to simulate Olympic performances of other teams
# Maybe factor in additional considerations: potential injury, experience level. etc??

# Run through different possible US team constructions (is there a more efficient way than like looping through them? how to do this?)
# Simulate US team performance

# Create individual all around scores

# Create team scores (note to self: would we need to simulate through a qualifying round?)

# measure performance of US teams (by calculating a weighted medal count)

# Keep track of which USA team construction performs best

# At end of simulations, compare performances of different US team constructions

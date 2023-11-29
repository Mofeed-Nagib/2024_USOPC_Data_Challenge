# This script prepares for simulations by:
# selecting the teams and individual athletes we believe will make it to the Olympics
# creating all possible 'teams' (combinations of athletes)
# and paring down to all 'reasonable' combinations.

#=================================#
#=== athlete/country selection ===#
#=================================#

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
top_men <- country_sums %>%
           filter(gender == 'm' & !(country %in% mens_known_qualifiers)) %>%
           arrange(-sum_score) %>% head(9)

top_women <- country_sums %>%
             filter(gender == 'w' & !(country %in% womens_known_qualifiers)) %>%
             arrange(-sum_score) %>% head(9)

# concatenate countries that have already qualified and our estimated qualifiers
men_countries <- c(mens_known_qualifiers) #, top_men$country)
women_countries <- c(womens_known_qualifiers) #, top_women$country)

# determine 5 person teams by taking 5 athletes with highest scores from each country
men_athletes <- later_player_scores %>% 
                filter(country %in% men_countries & country != 'USA' & gender == 'm') %>% 
                group_by(country) %>% 
                slice_max(avg_score, n = 5) %>% 
                mutate(flag_team = 1)

women_athletes <- later_player_scores %>% 
                  filter(country %in% women_countries & country != 'USA' & gender == 'w') %>% 
                  group_by(country) %>% 
                  slice_max(avg_score, n = 5) %>% 
                  mutate(flag_team = 1)


# known qualifiers of the best 36 gymnasts whose teams did not qualify
known_men_dnq <- c("Milad Karimi", "Artem Dolgopyat", "Artur Davtyan",
                   "Krisztofer Meszaros", "Lee Jun-ho", "Diogo Soares",
                   "Luka Van Den Keybus", "Andrei Vasile Muntean",
                   "Rhys McClenaghan", "Eleftherios Petrounias", "Kevin Penev",
                   "Noah Kuavita", "Tin Srbic")

best_men_dnq <- later_player_scores %>% 
                filter(fullname %in% known_men_dnq) %>% 
                group_by(fullname, country) %>% 
                arrange(-avg_score)

known_women_dnq <- c("Kaylia Nemour", "Pauline Schaefer Betz", "Alexa Moreno",
                     "Filipa Martins", "Aleah Finnegan", "Bettina Lili Czifra",
                     "Alba Petisco", "Anna Lashchevska", "Lena Bickel",
                     "Hillary Heron Soto", "Caitlin Rooskrantz", "Sona Artamonova",
                     "Lihie Raz", "Lucija Hribar", "Csenge Maria Bacskay",
                     "Ahtziri Sandoval", "Ana Perez", "Sarah Voss")

best_women_dnq <- later_player_scores %>% 
                  filter(fullname %in% known_women_dnq) %>% 
                  group_by(fullname, country) %>% 
                  arrange(-avg_score)

# determine the rest of the best 36 gymnasts whose teams did not qualify
# (maximum of 3 individuals per country)
best_men_dnq <- later_player_scores %>% 
                filter(!(country %in% men_countries) &
                       !(fullname %in% known_men_dnq) & gender == 'm') %>% 
                group_by(fullname, country) %>% 
                arrange(-avg_score) %>% 
                head(36 - nrow(best_men_dnq)) %>% 
                mutate(flag_team = 0) %>%
                rbind(best_men_dnq)

best_women_dnq <- later_player_scores %>% 
                  filter(!(country %in% women_countries) &
                         !(fullname %in% known_women_dnq) & gender == 'w') %>% 
                  group_by(fullname, country) %>% 
                  arrange(-avg_score) %>% 
                  head(36 - nrow(best_women_dnq)) %>% 
                  mutate(flag_team = 0) %>%
                  rbind(best_women_dnq)

#=======================#
#=== get us athletes ===#
#=======================#

# start by getting names of usa male and female athletes
us_men <- later_scores %>% 
          filter(country == 'USA' & gender == 'm') %>% 
          distinct(fullname) %>% 
          pull(fullname)

us_women <- later_scores %>% 
            filter(country == 'USA' & gender == 'w') %>% 
            distinct(fullname) %>% 
            pull(fullname)

#===============================#
#=== pare down combinations ===#
#===============================#

# get list of athletes that haven't competed in 1 year
expired_players <- later_scores %>% 
                   filter(country == 'USA') %>% 
                   group_by(fullname) %>% 
                   summarise(most_recent_compete = max(date, na.rm = TRUE)) %>% 
                   filter(most_recent_compete < ymd(Sys.Date()) - years(past_years)) %>% 
                   pull(fullname)

# remove them from our bank of us athletes
sub_us_men <- us_men[!(us_men %in% expired_players)]

sub_us_women <- us_women[!(us_women %in% expired_players)]

# now, we'll cut some people out based on score!
# take people who have been in top 8 aa or by apparatus
keep_scores_waa <- later_scores %>% 
                   filter(country == 'USA' & gender == 'w') %>% 
                   group_by(fullname) %>% 
                   summarize(avg_aa = mean(score)) %>% 
                   arrange(desc(avg_aa)) %>% head(5) %>% pull(fullname)

keep_scores_maa <- later_scores %>% 
                   filter(country == 'USA' & gender == 'm') %>% 
                   group_by(fullname) %>% 
                   summarize(avg_aa = mean(score)) %>% 
                   arrange(desc(avg_aa)) %>% head(5) %>% pull(fullname)   

keep_scores_wapp <- later_scores %>% 
                    filter(country == 'USA' & gender == 'w') %>% 
                    group_by(fullname, apparatus) %>% 
                    summarize(avg = mean(score)) %>% 
                    ungroup() %>% group_by(apparatus) %>% 
                    slice_max(order_by = avg, n = 5) %>% pull(fullname)

keep_scores_mapp <- later_scores %>% 
                    filter(country == 'USA' & gender == 'm') %>% 
                    group_by(fullname, apparatus) %>% 
                    summarize(avg = mean(score)) %>% 
                    ungroup() %>% group_by(apparatus) %>% 
                    slice_max(order_by = avg, n = 3) %>% pull(fullname)

sub_us_men <- sub_us_men[sub_us_men %in% c(keep_scores_maa, keep_scores_mapp)]
sub_us_women <- sub_us_women[sub_us_women %in% c(keep_scores_waa, keep_scores_wapp)]

#===================================#
#=== create us team combinations ===#
#===================================#

# find all possible 5 person teams for the usa
male_us_teams <- combn(sub_us_men, 5, simplify = FALSE)
women_us_teams <- combn(sub_us_women, 5, simplify = FALSE)

# convert teams into dataframes
df_male_us_teams <- setNames(as.data.frame(do.call(rbind, male_us_teams)), paste0("athlete_", 1:5))
df_female_us_teams <- setNames(as.data.frame(do.call(rbind, women_us_teams)), paste0("athlete_", 1:5))

## This script uses the distributions we created / scores we predicted in previous scripts
## to select the optimal gymnastics team!

#==================#
#=== PARAMETERS ===#
#==================#

trials <- 2
womens_apparatus <- c('fx', 'vt', 'bb', 'ub')
mens_apparatus <- c('fx', 'vt', 'hb', 'pb', 'sr', 'ph')

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

# start by getting names of usa male and female athletes
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

# create list to hold output
out_medal_winners <- list()

#=======================#
#=== FEMALE ATHLETES ===#
#=======================#

# Run female simulations
# test
# team_combo <- 1
# only running simulation for two teams for now!!
for (team_combo in c(1, 2)) {

  # get US athletes
  current_us_athletes <- data.frame(fullname = as.character(df_female_us_teams[team_combo,1:5]), country = 'USA', flag_team = 1)
  
  # making a dataframe of all competing athletes
  current_athletes <- rbind(women_athletes[,c('fullname', 'country', 'flag_team')],
                            best_women_dnq[,c('fullname', 'country', 'flag_team')],
                            current_us_athletes)
  
  # for each athlete, get mean scores for each apparatus
  # create loop to get mean scores for each apparatus
  ## FIX surely we could vectorize this
  # test
  # in_athlete <- as.character(current_athletes[2, 'fullname'])
  athlete_mean_scores <- data.frame()
  
  for (in_athlete in unique(current_athletes$fullname)) {
    
    # get mean scores
    fx <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'FX', 'mean']
    bb <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'BB', 'mean']
    vt <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'VT', 'mean']
    ub <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'UB', 'mean']    
    
    # get sd scores
    sd_fx <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'FX', 'sd']
    sd_bb <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'BB', 'sd']
    sd_vt <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'VT', 'sd']
    sd_ub <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'UB', 'sd']  
    
    current_row <- current_athletes %>% 
                   filter(fullname == in_athlete) %>% 
                   mutate(fx_mean = ifelse(length(fx) == 1, fx, NA),
                          bb_mean = ifelse(length(bb) == 1, bb, NA),
                          vt_mean = ifelse(length(vt) == 1, vt, NA),
                          ub_mean = ifelse(length(ub) == 1, ub, NA),
                          fx_sd = ifelse(length(sd_fx) == 1, sd_fx, NA),
                          bb_sd = ifelse(length(sd_bb) == 1, sd_bb, NA),
                          vt_sd = ifelse(length(sd_vt) == 1, sd_vt, NA),
                          ub_sd = ifelse(length(sd_ub) == 1, sd_ub, NA))
  
    athlete_mean_scores <- rbind(athlete_mean_scores, current_row)
  }
  
  
  
  ## QUALIFYING ROUND
  # ATHLETE SELECTION
  # Rule: 4 of the 5 athletes on each team will compete on each appartus
  # Pick the 4 athletes for each country that will compete on each apparatus
  
  select_competitors <- function(apparatus, select_from_data, from_each_country, add_individuals = 1) {
    
    # pick top athletes by country using mean score
    form_teams <- select_from_data %>%
                  filter(flag_team == 1 & !is.na(get(paste0(apparatus, "_mean")))) %>%
                  group_by(country) %>%
                  slice_max(order_by = get(paste0(apparatus, "_mean")), n = from_each_country)
    
    # dedup in case there were < 4 athletes with a score
    qual_apparatus_competitors <- form_teams[!duplicated(form_teams),]
    
    if (add_individuals == 1) {
      
      # Individual athletes can compete on all apparatuses, so let's just assume they do that ('worst case')
      form_individuals <- select_from_data %>%
                          filter(flag_team == 1 & !is.na(get(paste0(apparatus, "_mean")))) 
      
      # stack on the individual qualifiers that have scores for that apparatus
      qual_apparatus_competitors <- rbind(qual_apparatus_competitors, form_individuals)
    
    }
    
    # filter down columns
    out_qual_competitors <- qual_apparatus_competitors[,c('fullname', 'country', 'flag_team', 
                                                          paste0(apparatus, "_mean"), paste0(apparatus, "_sd"))]
    # return
    return(out_qual_competitors)
  }

  # apply over the apparatuses
  ls_qual_competitors <- purrr::map(womens_apparatus, ~select_competitors(.x, select_from_data = athlete_mean_scores, from_each_country = 4))
  names(ls_qual_competitors) <- womens_apparatus
  
  # stack together and clean up 
  qual_competitors <- as.data.frame(data.table::rbindlist(ls_qual_competitors, fill = TRUE))
  
  # collapse rows and deduplicate
  qual_competitors <- qual_competitors %>% 
                      group_by(fullname, country, flag_team) %>% 
                      fill(fx_mean, bb_mean, ub_mean, vt_mean, fx_sd, bb_sd, ub_sd, vt_sd, .direction = 'updown')
  
  qual_competitors <- qual_competitors[!duplicated(qual_competitors),]
  
  # reorder for visual purposes
  qual_competitors <- qual_competitors %>% arrange(country)
  
  # QUALIFYING ROUND SIMULATION
  # now that we have the competitors, we can actually simulate their scores!!!!! wahoo!
  # make working copy
  simulated_scores <- qual_competitors
  
  # for each simulation
  # trial<- 1
  for (trial in 1:trials) {
    
    # simulate qualifying scores in each event and create an aa score by summing them
    simulated_scores <- simulated_scores %>% 
                        mutate(current_fx_sim = rnorm(1, fx_mean, fx_sd),
                               current_vt_sim = rnorm(1, vt_mean, vt_sd),
                               current_bb_sim = rnorm(1, bb_mean, bb_sd),
                               current_ub_sim = rnorm(1, ub_mean, ub_sd)) %>% 
                        mutate(current_aa_sim = current_fx_sim + current_vt_sim + current_bb_sim + current_ub_sim)
    
    # rename cols
    data.table::setnames(simulated_scores, c('current_fx_sim', 'current_vt_sim', 'current_bb_sim', 'current_ub_sim'),
                                 c(paste0(womens_apparatus, "_", trial)))
    data.table::setnames(simulated_scores, 'current_aa_sim', paste0("aa_", trial))
    
    
  }
  
  # Create team scores 
  # create list to hold the teams moving on to final
  teams_in_final <- list()
  
  # for each trial, calculate team scores
  for (trial in 1:trials) {
    
    # 4 up, 3 count rule: only the top 3 scores on each event count for each country
    bb_scores <- simulated_scores %>% 
                 filter(flag_team == 1) %>% 
                 group_by(country) %>% 
                 slice_max(get(paste0("bb_", trial)), n = 3) %>% 
                 summarise(bb_score = sum(get(paste0("bb_", trial)), na.rm = T))
    
    ub_scores <- simulated_scores %>% 
                 filter(flag_team == 1) %>% 
                 group_by(country) %>% 
                 slice_max(get(paste0("ub_", trial)), n = 3) %>% 
                 summarise(ub_score = sum(get(paste0("ub_", trial)), na.rm = T))
    
    fx_scores <- simulated_scores %>% 
                 filter(flag_team == 1) %>% 
                 group_by(country) %>% 
                 slice_max(get(paste0("fx_", trial)), n = 3) %>% 
                 summarise(fx_score = sum(get(paste0("fx_", trial)), na.rm = T))
    
    vt_scores <- simulated_scores %>% 
                 filter(flag_team == 1) %>% 
                 group_by(country) %>% 
                 slice_max(get(paste0("vt_", trial)), n = 3) %>% 
                 summarise(vt_score = sum(get(paste0("vt_", trial)), na.rm = T))
   
    # merge them together into one dataframe so we can sum
    team_by_apparatus <- bb_scores %>% 
                         left_join(ub_scores, by = 'country') %>% 
                         left_join(vt_scores, by = 'country') %>% 
                         left_join(fx_scores, by = 'country')
    
    # sum across rows to get one final team score for the trial
    team_scores <- team_by_apparatus %>% 
                   mutate(team_score = bb_score + ub_score + vt_score + fx_score)
    
    # select top 8 teams to move on
    teams_in_final[[paste0("trial_", trial)]] <- team_scores %>% 
                                                 arrange(desc(team_score)) %>% 
                                                 head(8) %>% 
                                                 pull(country)
  }
  
  # Simulate the final rounds
  # create function to simulate event finals
  # test parameters: 
  # in_apparatus = 'fx'
  # in_trial_number = 1
  event_final <- function(in_apparatus, in_trial_number) {
    
    # 2 per country rule: can have max of 2 athletes per country in each final
    sub_simulated_scores <- simulated_scores %>% 
                            filter(!is.na(get(paste0(in_apparatus, "_", in_trial_number)))) %>% 
                            group_by(country) %>% 
                            slice_max(order_by = get(paste0(in_apparatus, "_", in_trial_number)), n = 2)
    
    # select top 8 competitors
    competitors <- sub_simulated_scores %>% 
                   arrange(desc(get(paste0(in_apparatus, "_", in_trial_number)))) %>% 
                   head(8)
    
    if (in_apparatus != 'aa') {
        # now simulate final score for each competitor
        competitors <- competitors %>% 
                       ungroup() %>% 
                       mutate(final_score = rnorm(1, get(paste0(in_apparatus, "_mean")),
                                                  get(paste0(in_apparatus, "_sd"))))
    } else {
      
      # if apparatus is na, we calculate the final score by sampling each apparatus and summing
      competitors <- competitors %>% 
                     ungroup() %>% 
                     mutate(final_score = rnorm(1, vt_mean, vt_sd) +
                              rnorm(1, fx_mean, fx_sd) +
                              rnorm(1, bb_mean, bb_sd) +
                              rnorm(1, ub_mean, ub_sd))
    }
        
    # get the medal winners, add on apparatus as a column
    winners <- competitors %>% 
               arrange(desc(final_score)) %>% 
               head(3) %>% 
               select(fullname, country, final_score) %>% 
               mutate(final_type = in_apparatus)
    
    # add on medal color as column and subset to us only 
    out_winners <- data.frame(winners, medal = c('gold', 'silver', 'bronze')) %>% 
                   filter(country == 'USA')
    
    return(out_winners)
  }
  
  # create list to hold medal winners
  ls_medal_winners <- list()
  
  # Now, for each trial 
  # test trial <- 1
  for (trial in 1:trials) {
    
    #====================#
    #=== event finals ===#
    #====================#
    
    # simulate event finals 
    vt_final <- event_final('vt', trial)
    bb_final <- event_final('bb', trial)
    ub_final <- event_final('ub', trial)
    fx_final <- event_final('fx', trial)
    
    #========================#
    #=== all around final ===#
    #========================#
    
    aa_final <- event_final('aa', trial)
    
    #==================#
    #=== team final ===#
    #==================#
    
    # subset mean scores dataframe to countries that qualified for the team final
    sub_mean_scores <- athlete_mean_scores %>%
                       filter(country %in% teams_in_final[[paste0("trial_", trial)]])
    
    # now run select competitors function to decide who will compete on what event
    # 3 athletes compete on each event for each country -- we will pick the 3 with the highest mean score
    # apply over the apparatuses
    ls_team_competitors <- purrr::map(womens_apparatus, ~select_competitors(.x, select_from_data = sub_mean_scores, from_each_country = 3, add_individuals = 0))
    names(ls_team_competitors) <- womens_apparatus
    
    # stack together and clean up 
    team_competitors <- as.data.frame(data.table::rbindlist(ls_team_competitors, fill = TRUE))
    
    # collapse rows and deduplicate
    team_competitors <- team_competitors %>% 
                        group_by(fullname, country, flag_team) %>%
                        fill(fx_mean, bb_mean, ub_mean, vt_mean, fx_sd, bb_sd, ub_sd, vt_sd, .direction = 'updown')
    
    team_competitors <- team_competitors[!duplicated(team_competitors),]
    
    # simulate their scores!!
    team_final <- team_competitors %>% 
                  mutate(vt_score = rnorm(1, vt_mean, vt_sd),
                         fx_score = rnorm(1, fx_mean, fx_sd),
                         bb_score = rnorm(1, bb_mean, bb_sd),
                         ub_score = rnorm(1, ub_mean, ub_sd))
    
    # tally scores by country
    team_final <- team_final %>% 
                  group_by(country) %>% 
                  summarise(final_score = sum(vt_score, na.rm = T) +
                                          sum(fx_score, na.rm = T) +
                                          sum(bb_score, na.rm = T) +
                                          sum(ub_score, na.rm = T)) 
    
    # get the team winners, add on apparatus as a column
    team_winners <- team_final %>% 
                    arrange(desc(final_score)) %>% 
                    head(3) %>% 
                    select(country, final_score) %>% 
                    mutate(final_type = "team")
    
    # add on medal color as column and subset to us only 
    out_team_winners <- data.frame(team_winners, medal = c('gold', 'silver', 'bronze')) %>% 
                        filter(country == 'USA')
    
    #==============#
    #=== output ===#
    #==============#
    
    # stack together the us medal results from all finals
    out_us_results <- plyr::rbind.fill(vt_final, bb_final, ub_final, fx_final, aa_final, out_team_winners)

    # save results to an object
    ls_medal_winners[[paste0("trial_", trial)]] <- out_us_results
  }
  
  # Use the US outcomes to calculate 'weighted medal count' for each trial
  # test trial <- 2
  for (trial in 1:trials) {
    
    # get medal winners
    medals <- ls_medal_winners[[paste0("trial_", trial)]]$medal
    
    # calculate weighted medal count
    # gold is worth 3 points, silver 2, bronze 1
    wt_count <- 3*sum(medals == 'gold') + 2*sum(medals == 'silver') + sum(medals == 'bronze')
    
    # add weighted count to dataframe
    df_female_us_teams[team_combo, paste0('wt_count_trial_', trial)] <- wt_count
    
  }
  
  # save medal winners to an object
  out_medal_winners[[paste0("team_combo_", team_combo)]] <- ls_medal_winners
}


#=====================#
#=== MALE ATHLETES ===#
#=====================#

# Run male simulations
# test
# team_combo <- 1
# only running simulation for two teams for now!!
for (team_combo in c(1, 2)) {
  
  # get US athletes
  current_us_athletes <- data.frame(fullname = as.character(df_male_us_teams[team_combo,1:5]), country = 'USA', flag_team = 1)
  
  # making a dataframe of all competing athletes
  current_athletes <- rbind(men_athletes[,c('fullname', 'country', 'flag_team')],
                            best_men_dnq[,c('fullname', 'country', 'flag_team')],
                            current_us_athletes)
  
  # for each athlete, get mean scores for each apparatus
  # create loop to get mean scores for each apparatus
  ## FIX surely we could vectorize this
  # test
  # in_athlete <- as.character(current_athletes[2, 'fullname'])
  athlete_mean_scores <- data.frame()
  
  for (in_athlete in unique(current_athletes$fullname)) {
    
    # get mean scores
    fx <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'FX', 'mean']
    vt <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'VT', 'mean']
    hb <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'HB', 'mean']
    pb <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'PB', 'mean']
    sr <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'SR', 'mean']
    ph <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'PH', 'mean']
    
    
    # get sd scores
    sd_fx <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'FX', 'sd']
    sd_vt <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'VT', 'sd']
    sd_hb <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'HB', 'sd']
    sd_pb <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'PB', 'sd']
    sd_sr <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'SR', 'sd']
    sd_ph <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'PH', 'sd']
    
    current_row <- current_athletes %>% 
      filter(fullname == in_athlete) %>% 
      mutate(fx_mean = ifelse(length(fx) == 1, fx, NA),
             vt_mean = ifelse(length(vt) == 1, vt, NA),
             hb_mean = ifelse(length(hb) == 1, hb, NA),
             pb_mean = ifelse(length(pb) == 1, pb, NA),
             sr_mean = ifelse(length(sr) == 1, sr, NA),
             ph_mean = ifelse(length(ph) == 1, ph, NA),
             fx_sd = ifelse(length(sd_fx) == 1, sd_fx, NA),
             vt_sd = ifelse(length(sd_vt) == 1, sd_vt, NA),
             hb_sd = ifelse(length(sd_hb) == 1, sd_hb, NA),
             pb_sd = ifelse(length(sd_pb) == 1, sd_pb, NA),
             sr_sd = ifelse(length(sd_sr) == 1, sd_sr, NA),
             ph_sd = ifelse(length(sd_ph) == 1, sd_ph, NA))
    
    athlete_mean_scores <- rbind(athlete_mean_scores, current_row)
  }
  
  
  
  ## QUALIFYING ROUND
  # ATHLETE SELECTION
  # Rule: 4 of the 5 athletes on each team will compete on each appartus
  # Pick the 4 athletes for each country that will compete on each apparatus
  
  select_competitors <- function(apparatus, select_from_data, from_each_country, add_individuals = 1) {
    
    # pick top athletes by country using mean score
    form_teams <- select_from_data %>%
                  filter(flag_team == 1 & !is.na(get(paste0(apparatus, "_mean")))) %>%
                  group_by(country) %>%
                  slice_max(order_by = get(paste0(apparatus, "_mean")), n = from_each_country)
    
    # dedup in case there were < 4 athletes with a score
    qual_apparatus_competitors <- form_teams[!duplicated(form_teams),]
    
    if (add_individuals == 1) {
      
      # Individual athletes can compete on all apparatuses, so let's just assume they do that ('worst case')
      form_individuals <- select_from_data %>%
                          filter(flag_team == 1 & !is.na(get(paste0(apparatus, "_mean")))) 
      
      # stack on the individual qualifiers that have scores for that apparatus
      qual_apparatus_competitors <- rbind(qual_apparatus_competitors, form_individuals)
      
    }
    
    # filter down columns
    out_qual_competitors <- qual_apparatus_competitors[,c('fullname', 'country', 'flag_team', 
                                                          paste0(apparatus, "_mean"), paste0(apparatus, "_sd"))]
    # return
    return(out_qual_competitors)
  }
  
  # apply over the apparatuses
  ls_qual_competitors <- purrr::map(mens_apparatus, ~select_competitors(.x, select_from_data = athlete_mean_scores, from_each_country = 4))
  names(ls_qual_competitors) <- mens_apparatus
  
  # stack together and clean up 
  qual_competitors <- as.data.frame(data.table::rbindlist(ls_qual_competitors, fill = TRUE))
  
  # collapse rows and deduplicate
  qual_competitors <- qual_competitors %>%
                      group_by(fullname, country, flag_team) %>%
                      fill(fx_mean, vt_mean, hb_mean, pb_mean, sr_mean, ph_mean, fx_sd, vt_sd, hb_sd, pb_sd, sr_sd, ph_sd, .direction = 'updown')
  
  qual_competitors <- qual_competitors[!duplicated(qual_competitors),]
  
  # reorder for visual purposes
  qual_competitors <- qual_competitors %>% arrange(country)
  
  # QUALIFYING ROUND SIMULATION
  # now that we have the competitors, we can actually simulate their scores!!!!! wahoo!
  # make working copy
  simulated_scores <- qual_competitors
  
  # for each simulation
  # trial<- 1
  for (trial in 1:trials) {
    
    # simulate qualifying scores in each event and create an aa score by summing them
    simulated_scores <- simulated_scores %>%
                        mutate(current_fx_sim = rnorm(1, fx_mean, fx_sd),
                               current_vt_sim = rnorm(1, vt_mean, vt_sd),
                               current_hb_sim = rnorm(1, hb_mean, hb_sd),
                               current_pb_sim = rnorm(1, pb_mean, pb_sd),
                               current_sr_sim = rnorm(1, sr_mean, sr_sd),
                               current_ph_sim = rnorm(1, ph_mean, ph_sd)) %>%
                        mutate(current_aa_sim = current_fx_sim + current_vt_sim + current_hb_sim + current_pb_sim + current_sr_sim + current_ph_sim)
    
    # rename cols
    data.table::setnames(simulated_scores, c('current_fx_sim', 'current_vt_sim', 'current_hb_sim', 'current_pb_sim', 'current_sr_sim', 'current_ph_sim'),
                         c(paste0(mens_apparatus, "_", trial)))
    data.table::setnames(simulated_scores, 'current_aa_sim', paste0("aa_", trial))
    
  }
  
  # Create team scores 
  # create list to hold the teams moving on to final
  teams_in_final <- list()
  
  # for each trial, calculate team scores
  for (trial in 1:trials) {
    
    # 4 up, 3 count rule: only the top 3 scores on each event count for each country
    fx_scores <- simulated_scores %>%
                 filter(flag_team == 1) %>% 
                 group_by(country) %>% 
                 slice_max(get(paste0("fx_", trial)), n = 3) %>% 
                 summarise(fx_score = sum(get(paste0("fx_", trial)), na.rm = T))
    
    vt_scores <- simulated_scores %>% 
                 filter(flag_team == 1) %>% 
                 group_by(country) %>% 
                 slice_max(get(paste0("vt_", trial)), n = 3) %>% 
                 summarise(vt_score = sum(get(paste0("vt_", trial)), na.rm = T))
    
    hb_scores <- simulated_scores %>% 
                 filter(flag_team == 1) %>% 
                 group_by(country) %>% 
                 slice_max(get(paste0("hb_", trial)), n = 3) %>% 
                 summarise(hb_score = sum(get(paste0("hb_", trial)), na.rm = T))
    
    pb_scores <- simulated_scores %>% 
                 filter(flag_team == 1) %>% 
                 group_by(country) %>% 
                 slice_max(get(paste0("pb_", trial)), n = 3) %>% 
                 summarise(pb_score = sum(get(paste0("pb_", trial)), na.rm = T))
    
    sr_scores <- simulated_scores %>% 
                 filter(flag_team == 1) %>% 
                 group_by(country) %>% 
                 slice_max(get(paste0("sr_", trial)), n = 3) %>% 
                 summarise(sr_score = sum(get(paste0("sr_", trial)), na.rm = T))
    
    ph_scores <- simulated_scores %>% 
                 filter(flag_team == 1) %>% 
                 group_by(country) %>% 
                 slice_max(get(paste0("ph_", trial)), n = 3) %>% 
                 summarise(ph_score = sum(get(paste0("ph_", trial)), na.rm = T))
    
    # merge them together into one dataframe so we can sum
    team_by_apparatus <- fx_scores %>%
                         left_join(vt_scores, by = 'country') %>%
                         left_join(hb_scores, by = 'country') %>% 
                         left_join(pb_scores, by = 'country') %>% 
                         left_join(sr_scores, by = 'country') %>% 
                         left_join(ph_scores, by = 'country')
    
    # sum across rows to get one final team score for the trial
    team_scores <- team_by_apparatus %>%
                   mutate(team_score = fx_score + vt_score + hb_score + pb_score + sr_score + ph_score)
    
    # select top 8 teams to move on
    teams_in_final[[paste0("trial_", trial)]] <- team_scores %>% 
                                                 arrange(desc(team_score)) %>% 
                                                 head(8) %>% 
                                                 pull(country)
  }
  
  # Simulate the final rounds
  # create function to simulate event finals
  # test parameters: 
  # in_apparatus = 'fx'
  # in_trial_number = 1
  event_final <- function(in_apparatus, in_trial_number) {
    
    # 2 per country rule: can have max of 2 athletes per country in each final
    sub_simulated_scores <- simulated_scores %>% 
                            filter(!is.na(get(paste0(in_apparatus, "_", in_trial_number)))) %>% 
                            group_by(country) %>% 
                            slice_max(order_by = get(paste0(in_apparatus, "_", in_trial_number)), n = 2)
    
    # select top 8 competitors
    competitors <- sub_simulated_scores %>% 
                   arrange(desc(get(paste0(in_apparatus, "_", in_trial_number)))) %>% 
                   head(8)
    
    if (in_apparatus != 'aa') {
      # now simulate final score for each competitor
      competitors <- competitors %>% 
                     ungroup() %>% 
                     mutate(final_score = rnorm(1, get(paste0(in_apparatus, "_mean")),
                                               get(paste0(in_apparatus, "_sd"))))
    } else {
      
      # if apparatus is na, we calculate the final score by sampling each apparatus and summing
      competitors <- competitors %>% 
                     ungroup() %>% 
                     mutate(final_score = rnorm(1, fx_mean, fx_sd) +
                                          rnorm(1, vt_mean, vt_sd) +
                                          rnorm(1, hb_mean, hb_sd) +
                                          rnorm(1, pb_mean, pb_sd) +
                                          rnorm(1, sr_mean, sr_sd) +
                                          rnorm(1, ph_mean, ph_sd))
    }
    
    # get the medal winners, add on apparatus as a column
    winners <- competitors %>% 
               arrange(desc(final_score)) %>% 
               head(3) %>% 
               select(fullname, country, final_score) %>% 
               mutate(final_type = in_apparatus)
    
    # add on medal color as column and subset to us only 
    out_winners <- data.frame(winners, medal = c('gold', 'silver', 'bronze')) %>% 
                   filter(country == 'USA')
    
    return(out_winners)
  }
  
  # create list to hold medal winners
  ls_medal_winners <- list()
  
  # Now, for each trial 
  # test trial <- 1
  for (trial in 1:trials) {
    
    #====================#
    #=== event finals ===#
    #====================#
    
    # simulate event finals 
    fx_final <- event_final('fx', trial)
    vt_final <- event_final('vt', trial)
    hb_final <- event_final('hb', trial)
    pb_final <- event_final('pb', trial)
    sr_final <- event_final('sr', trial)
    ph_final <- event_final('ph', trial)
    
    #========================#
    #=== all around final ===#
    #========================#
    
    aa_final <- event_final('aa', trial)
    
    #==================#
    #=== team final ===#
    #==================#
    
    # subset mean scores dataframe to countries that qualified for the team final
    sub_mean_scores <- athlete_mean_scores %>%
                       filter(country %in% teams_in_final[[paste0("trial_", trial)]])
    
    # now run select competitors function to decide who will compete on what event
    # 3 athletes compete on each event for each country -- we will pick the 3 with the highest mean score
    # apply over the apparatuses
    ls_team_competitors <- purrr::map(mens_apparatus, ~select_competitors(.x, select_from_data = sub_mean_scores, from_each_country = 3, add_individuals = 0))
    names(ls_team_competitors) <- mens_apparatus
    
    # stack together and clean up 
    team_competitors <- as.data.frame(data.table::rbindlist(ls_team_competitors, fill = TRUE))
    
    # collapse rows and deduplicate
    team_competitors <- team_competitors %>% 
                        group_by(fullname, country, flag_team) %>% 
                        fill(fx_mean, vt_mean, hb_mean, pb_mean, sr_mean, hb_mean, fx_sd, vt_sd, hb_sd, pb_sd, sr_sd, ph_sd, .direction = 'updown')
    
    team_competitors <- team_competitors[!duplicated(team_competitors),]
    
    # simulate their scores!!
    team_final <- team_competitors %>% 
                  mutate(fx_score = rnorm(1, fx_mean, fx_sd),
                         vt_score = rnorm(1, vt_mean, vt_sd),
                         hb_score = rnorm(1, hb_mean, hb_sd),
                         pb_score = rnorm(1, pb_mean, pb_sd),
                         sr_score = rnorm(1, sr_mean, sr_sd),
                         ph_score = rnorm(1, ph_mean, ph_sd))
    
    # tally scores by country
    team_final <- team_final %>% 
                  group_by(country) %>% 
                  summarise(final_score = sum(fx_score, na.rm = T) +
                                          sum(vt_score, na.rm = T) +
                                          sum(hb_score, na.rm = T) +
                                          sum(pb_score, na.rm = T) +
                                          sum(sr_score, na.rm = T) +
                                          sum(ph_score, na.rm = T)) 
    
    # get the team winners, add on apparatus as a column
    team_winners <- team_final %>% 
                    arrange(desc(final_score)) %>% 
                    head(3) %>% 
                    select(country, final_score) %>% 
                    mutate(final_type = "team")
    
    # add on medal color as column and subset to us only 
    out_team_winners <- data.frame(team_winners, medal = c('gold', 'silver', 'bronze')) %>% 
      filter(country == 'USA')
    
    #==============#
    #=== output ===#
    #==============#
    
    # stack together the us medal results from all finals
    out_us_results <- plyr::rbind.fill(fx_final, vt_final, hb_final, pb_final, sr_final, ph_final, aa_final, out_team_winners)
    
    # save results to an object
    ls_medal_winners[[paste0("trial_", trial)]] <- out_us_results
  }
  
  # Use the US outcomes to calculate 'weighted medal count' for each trial
  # test trial <- 2
  for (trial in 1:trials) {
    
    # get medal winners
    medals <- ls_medal_winners[[paste0("trial_", trial)]]$medal
    
    # calculate weighted medal count
    # gold is worth 3 points, silver 2, bronze 1
    wt_count <- 3*sum(medals == 'gold') + 2*sum(medals == 'silver') + sum(medals == 'bronze')
    
    # add weighted count to dataframe
    df_male_us_teams[team_combo, paste0('wt_count_trial_', trial)] <- wt_count
    
  }
  
  # save medal winners to an object
  out_medal_winners[[paste0("team_combo_", team_combo)]] <- ls_medal_winners
  
}

#=====================#
#=== MALE ATHLETES ===#
#=====================#

# create list to hold output
out_male_medal_winners <- list()

# Run male simulations
# test
# team_combo <- 1
# only running simulation for two teams for now!!
if (is.na(n_team_combos)) {n_team_combos <- nrow(df_male_us_teams)}

for (team_combo in 1:n_team_combos) {
  
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
  # create list to hold medal winners
  ls_medal_winners <- list()
  
  # Now, for each trial 
  # test trial <- 1
  for (trial in 1:trials) {
    
    #====================#
    #=== event finals ===#
    #====================#
    
    # simulate event finals 
    fx_final <- event_final('fx', trial, opt_gender = 'm')
    vt_final <- event_final('vt', trial, opt_gender = 'm')
    hb_final <- event_final('hb', trial, opt_gender = 'm')
    pb_final <- event_final('pb', trial, opt_gender = 'm')
    sr_final <- event_final('sr', trial, opt_gender = 'm')
    ph_final <- event_final('ph', trial, opt_gender = 'm')
    
    #========================#
    #=== all around final ===#
    #========================#
    
    aa_final <- event_final('aa', trial, opt_gender = 'm')
    
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
  out_male_medal_winners[[paste0("team_combo_", team_combo)]] <- ls_medal_winners
  
}

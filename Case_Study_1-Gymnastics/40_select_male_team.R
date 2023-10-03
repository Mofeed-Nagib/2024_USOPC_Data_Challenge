## This script uses the distributions we created / scores we predicted in previous scripts
## to select the optimal male gymnastics team!

# create list to hold output
out_male_medal_winners <- list()

# making a dataframe of all competing athletes
all_male_athletes <- rbind(men_athletes[,c('fullname', 'country', 'flag_team')],
                           best_men_dnq[,c('fullname', 'country', 'flag_team')],
                           data.frame(fullname = sub_us_men, country = 'USA', flag_team = 1))

# for each athlete, get mean scores for each apparatus
men_mean_scores <- data.frame()

for (in_athlete in unique(all_male_athletes$fullname)) {
  
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
  
  current_row <- all_male_athletes %>% 
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
  
  men_mean_scores <- rbind(men_mean_scores, current_row)
}

# get list of just international competitors and just US
intl_mean_scores <- men_mean_scores %>% filter(country != 'USA')
us_mean_scores <- men_mean_scores %>% filter(country == 'USA')

# let's go ahead and simulate their scores ahead of time!
# start by choosing who will do what events
# apply over the apparatuses
ls_qual_competitors <- purrr::map(mens_apparatus, ~select_competitors(.x, select_from_data = intl_mean_scores, from_each_country = 4))
names(ls_qual_competitors) <- mens_apparatus

# stack together and clean up 
qual_competitors <- as.data.frame(data.table::rbindlist(ls_qual_competitors, fill = TRUE))

# collapse rows and deduplicate
qual_competitors <- qual_competitors %>% 
  group_by(fullname, country, flag_team) %>% 
  fill(fx_mean, vt_mean, hb_mean, pb_mean, sr_mean, ph_mean, fx_sd, vt_sd, hb_sd, pb_sd, sr_sd, ph_sd, .direction = 'updown')

qual_competitors <- qual_competitors[!duplicated(qual_competitors),]

# stack on us competitors
qual_competitors <- rbind(qual_competitors, us_mean_scores)

# now actually simulate qual scores
qual_scores <- qual_competitors %>%
  rowwise() %>%
  mutate(current_fx_sim = list(setNames(rnorm(trials, fx_mean, fx_sd), paste0('fx_', 1:trials))),
         current_vt_sim = list(setNames(rnorm(trials, vt_mean, vt_sd), paste0('vt_', 1:trials))),
         current_hb_sim = list(setNames(rnorm(trials, hb_mean, hb_sd), paste0('hb_', 1:trials))),
         current_pb_sim = list(setNames(rnorm(trials, pb_mean, pb_sd), paste0('pb_', 1:trials))),
         current_sr_sim = list(setNames(rnorm(trials, sr_mean, sr_sd), paste0('sr_', 1:trials))),
         current_ph_sim = list(setNames(rnorm(trials, ph_mean, ph_sd), paste0('ph_', 1:trials)))) %>%
  unnest_wider(current_fx_sim) %>%
  unnest_wider(current_vt_sim) %>%
  unnest_wider(current_hb_sim) %>%
  unnest_wider(current_pb_sim) %>%
  unnest_wider(current_sr_sim) %>%
  unnest_wider(current_ph_sim)


# and final scores too
event_final_scores <- qual_competitors %>%
  rowwise() %>%
  mutate(current_fx_sim = list(setNames(rnorm(trials, fx_mean, fx_sd), paste0('fx_', 1:trials))),
         current_vt_sim = list(setNames(rnorm(trials, vt_mean, vt_sd), paste0('vt_', 1:trials))),
         current_hb_sim = list(setNames(rnorm(trials, hb_mean, hb_sd), paste0('hb_', 1:trials))),
         current_pb_sim = list(setNames(rnorm(trials, pb_mean, pb_sd), paste0('pb_', 1:trials))),
         current_sr_sim = list(setNames(rnorm(trials, sr_mean, sr_sd), paste0('sr_', 1:trials))),
         current_ph_sim = list(setNames(rnorm(trials, ph_mean, ph_sd), paste0('ph_', 1:trials)))) %>%
  unnest_wider(current_fx_sim) %>%
  unnest_wider(current_vt_sim) %>%
  unnest_wider(current_hb_sim) %>%
  unnest_wider(current_pb_sim) %>%
  unnest_wider(current_sr_sim) %>%
  unnest_wider(current_ph_sim)

aa_final_scores <- qual_competitors %>%
  rowwise() %>%
  mutate(current_fx_sim = list(setNames(rnorm(trials, fx_mean, fx_sd), paste0('fx_', 1:trials))),
         current_vt_sim = list(setNames(rnorm(trials, vt_mean, vt_sd), paste0('vt_', 1:trials))),
         current_hb_sim = list(setNames(rnorm(trials, hb_mean, hb_sd), paste0('hb_', 1:trials))),
         current_pb_sim = list(setNames(rnorm(trials, pb_mean, pb_sd), paste0('pb_', 1:trials))),
         current_sr_sim = list(setNames(rnorm(trials, sr_mean, sr_sd), paste0('sr_', 1:trials))),
         current_ph_sim = list(setNames(rnorm(trials, ph_mean, ph_sd), paste0('ph_', 1:trials)))) %>%
  unnest_wider(current_fx_sim) %>%
  unnest_wider(current_vt_sim) %>%
  unnest_wider(current_hb_sim) %>%
  unnest_wider(current_pb_sim) %>%
  unnest_wider(current_sr_sim) %>%
  unnest_wider(current_ph_sim)

team_final_scores <- qual_competitors %>%
  rowwise() %>%
  mutate(current_fx_sim = list(setNames(rnorm(trials, fx_mean, fx_sd), paste0('fx_', 1:trials))),
         current_vt_sim = list(setNames(rnorm(trials, vt_mean, vt_sd), paste0('vt_', 1:trials))),
         current_hb_sim = list(setNames(rnorm(trials, hb_mean, hb_sd), paste0('hb_', 1:trials))),
         current_pb_sim = list(setNames(rnorm(trials, pb_mean, pb_sd), paste0('pb_', 1:trials))),
         current_sr_sim = list(setNames(rnorm(trials, sr_mean, sr_sd), paste0('sr_', 1:trials))),
         current_ph_sim = list(setNames(rnorm(trials, ph_mean, ph_sd), paste0('ph_', 1:trials)))) %>%
  unnest_wider(current_fx_sim) %>%
  unnest_wider(current_vt_sim) %>%
  unnest_wider(current_hb_sim) %>%
  unnest_wider(current_pb_sim) %>%
  unnest_wider(current_sr_sim) %>%
  unnest_wider(current_ph_sim)

# and calculate aa scores where necessary
for (trial in 1:trials) {
  qual_scores <- qual_scores %>% 
    mutate(!!as.name(paste0("aa_", trial)) := get(paste0("fx_", trial)) + get(paste0("vt_", trial)) + get(paste0("hb_", trial)) + get(paste0("pb_", trial)) + get(paste0("sr_", trial)) + get(paste0("ph_", trial)))
  
  aa_final_scores <- aa_final_scores %>% 
    mutate(!!as.name(paste0("aa_", trial)) := get(paste0("fx_", trial)) + get(paste0("vt_", trial)) + get(paste0("hb_", trial)) + get(paste0("pb_", trial)) + get(paste0("sr_", trial)) + get(paste0("ph_", trial))) 
}

#=======================#
#==== MALE ATHLETES ====#
#=======================#

# Run male simulations
# test
# team_combo <- 1
# only running simulation for two teams for now!!
if (is.na(n_team_combos)) {n_team_combos <- nrow(df_male_us_teams)}

for (team_combo in 1:n_team_combos) {
  # print statement
  print(paste0("Running simulation ", team_combo, " out of ", n_team_combos))
  
  ## QUALIFYING ROUND
  # start by subsetting down our simulated scores to only the athletes we want
  simulated_scores <- qual_scores %>% filter(country != 'USA' | fullname %in% as.character(df_male_us_teams[team_combo,1:5]))
  sub_event_scores <- event_final_scores %>% filter(country != 'USA' | fullname %in% as.character(df_male_us_teams[team_combo,1:5]))
  sub_aa_scores <- aa_final_scores %>% filter(country != 'USA' | fullname %in% as.character(df_male_us_teams[team_combo,1:5]))
  sub_team_scores <- team_final_scores %>% filter(country != 'USA' | fullname %in% as.character(df_male_us_teams[team_combo,1:5]))
  
  # Create team scores 
  # create list to hold the teams moving on to final
  teams_in_final <- list()
  
  # for each trial, calculate team scores and aa scores
  for (trial in 1:trials) {
    
    # 4 up, 3 count rule: only the top 3 scores on each event count for each country
    fx_scores <- simulated_scores %>% 
      filter(flag_team == 1) %>% 
      group_by(country) %>% 
      slice_max(fx_mean, n = 4) %>%
      slice_max(get(paste0("fx_", trial)), n = 3) %>% 
      summarise(fx_score = sum(get(paste0("fx_", trial)), na.rm = T))
    
    vt_scores <- simulated_scores %>% 
      filter(flag_team == 1) %>% 
      group_by(country) %>% 
      slice_max(vt_mean, n = 4) %>%
      slice_max(get(paste0("vt_", trial)), n = 3) %>% 
      summarise(vt_score = sum(get(paste0("vt_", trial)), na.rm = T))
    
    hb_scores <- simulated_scores %>% 
      filter(flag_team == 1) %>% 
      group_by(country) %>% 
      slice_max(hb_mean, n = 4) %>%
      slice_max(get(paste0("hb_", trial)), n = 3) %>% 
      summarise(hb_score = sum(get(paste0("hb_", trial)), na.rm = T))
    
    pb_scores <- simulated_scores %>% 
      filter(flag_team == 1) %>% 
      group_by(country) %>% 
      slice_max(pb_mean, n = 4) %>%
      slice_max(get(paste0("pb_", trial)), n = 3) %>% 
      summarise(pb_score = sum(get(paste0("pb_", trial)), na.rm = T))
    
    sr_scores <- simulated_scores %>% 
      filter(flag_team == 1) %>% 
      group_by(country) %>% 
      slice_max(sr_mean, n = 4) %>%
      slice_max(get(paste0("sr_", trial)), n = 3) %>% 
      summarise(sr_score = sum(get(paste0("sr_", trial)), na.rm = T))
    
    ph_scores <- simulated_scores %>% 
      filter(flag_team == 1) %>% 
      group_by(country) %>% 
      slice_max(ph_mean, n = 4) %>%
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
  
  # us team selection
  fx_min <- sub_team_scores %>% filter(country == 'USA') %>% slice_min(order_by = fx_mean, n = 1) %>% pull(fullname)
  vt_min <- sub_team_scores %>% filter(country == 'USA') %>% slice_min(order_by = vt_mean, n = 1) %>% pull(fullname)
  hb_min <- sub_team_scores %>% filter(country == 'USA') %>% slice_min(order_by = hb_mean, n = 1) %>% pull(fullname)
  pb_min <- sub_team_scores %>% filter(country == 'USA') %>% slice_min(order_by = pb_mean, n = 1) %>% pull(fullname)
  sr_min <- sub_team_scores %>% filter(country == 'USA') %>% slice_min(order_by = sr_mean, n = 1) %>% pull(fullname)
  ph_min <- sub_team_scores %>% filter(country == 'USA') %>% slice_min(order_by = ph_mean, n = 1) %>% pull(fullname)
  
  sub_team_scores[sub_team_scores$fullname == fx_min,startsWith(colnames(sub_team_scores),"fx")] <- NA
  sub_team_scores[sub_team_scores$fullname == vt_min,startsWith(colnames(sub_team_scores),"vt")] <- NA
  sub_team_scores[sub_team_scores$fullname == hb_min,startsWith(colnames(sub_team_scores),"hb")] <- NA
  sub_team_scores[sub_team_scores$fullname == pb_min,startsWith(colnames(sub_team_scores),"pb")] <- NA
  sub_team_scores[sub_team_scores$fullname == sr_min,startsWith(colnames(sub_team_scores),"sr")] <- NA
  sub_team_scores[sub_team_scores$fullname == ph_min,startsWith(colnames(sub_team_scores),"ph")] <- NA
  
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
    
    # subset team scores dataframe to countries that qualified for the team final
    team_competitors <- sub_team_scores %>%
      filter(country %in% teams_in_final[[paste0("trial_", trial)]])
    
    # tally scores by country
    team_final <- team_competitors %>% 
      group_by(country) %>% 
      summarise(final_score = sum(get(paste0('fx_', trial)), na.rm = T) +
                              sum(get(paste0('vt_', trial)), na.rm = T) +
                              sum(get(paste0('hb_', trial)), na.rm = T) +
                              sum(get(paste0('pb_', trial)), na.rm = T) +
                              sum(get(paste0('sr_', trial)), na.rm = T) +
                              sum(get(paste0('ph_', trial)), na.rm = T))
    
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
  #lapply(ls_medal_winners, medal_count, in_team_combo = team_combo)
  
  for (trial in 1:trials) {
    
    medals <- ls_medal_winners[[paste0("trial_", trial)]]$medal
    wt_count <- 3*sum(medals == 'gold') + 2*sum(medals == 'silver') + sum(medals == 'bronze')
    # add weighted count to dataframe
    df_male_us_teams[team_combo, paste0('wt_count_trial_', trial)] <- wt_count
    
  }
  
  # save medal winners to an object
  out_male_medal_winners[[paste0("team_combo_", team_combo)]] <- ls_medal_winners
}
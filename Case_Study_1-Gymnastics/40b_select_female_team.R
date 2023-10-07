## This script uses the distributions we created / scores we predicted in previous scripts
## to select the optimal female gymnastics team!
library(doSNOW)
library(foreach)
library(parallel)

## Setup
n.cores = detectCores()-2 ## leave 2 cores unused so I can keep using computer

print(Sys.time())
cl = makeCluster(n.cores)
registerDoSNOW(cl)

# create list to hold output
out_female_medal_winners <- list()

# making a dataframe of all competing athletes
all_female_athletes <- rbind(women_athletes[,c('fullname', 'country', 'flag_team')],
                          best_women_dnq[,c('fullname', 'country', 'flag_team')],
                          data.frame(fullname = sub_us_women, country = 'USA', flag_team = 1))

# for each athlete, get mean scores for each apparatus
women_mean_scores <- data.frame()

for (in_athlete in unique(all_female_athletes$fullname)) {
  
  # get mean scores
  fx <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'FX', 'mean']
  vt <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'VT', 'mean']
  bb <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'BB', 'mean']
  ub <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'UB', 'mean']    
  
  # get sd scores
  sd_fx <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'FX', 'sd']
  sd_vt <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'VT', 'sd']
  sd_bb <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'BB', 'sd']
  sd_ub <- gymnast_dist[gymnast_dist$fullname == in_athlete & gymnast_dist$apparatus == 'UB', 'sd']  
  
  current_row <- all_female_athletes %>% 
    filter(fullname == in_athlete) %>% 
    mutate(fx_mean = ifelse(length(fx) == 1, fx, NA),
           vt_mean = ifelse(length(vt) == 1, vt, NA),
           bb_mean = ifelse(length(bb) == 1, bb, NA),
           ub_mean = ifelse(length(ub) == 1, ub, NA),
           fx_sd = ifelse(length(sd_fx) == 1, sd_fx, NA),
           vt_sd = ifelse(length(sd_vt) == 1, sd_vt, NA),
           bb_sd = ifelse(length(sd_bb) == 1, sd_bb, NA),
           ub_sd = ifelse(length(sd_ub) == 1, sd_ub, NA))
  
  women_mean_scores <- rbind(women_mean_scores, current_row)
}

# get list of just international competitors and just US
intl_mean_scores <- women_mean_scores %>% filter(country != 'USA')
us_mean_scores <- women_mean_scores %>% filter(country == 'USA')

# let's go ahead and simulate their scores ahead of time!
# start by choosing who will do what events
# apply over the apparatuses
ls_qual_competitors <- purrr::map(womens_apparatus, ~select_competitors(.x, select_from_data = intl_mean_scores, from_each_country = 4))
names(ls_qual_competitors) <- womens_apparatus

# stack together and clean up 
qual_competitors <- as.data.frame(data.table::rbindlist(ls_qual_competitors, fill = TRUE))

# collapse rows and deduplicate
qual_competitors <- qual_competitors %>% 
  group_by(fullname, country, flag_team) %>% 
  fill(fx_mean, vt_mean, bb_mean, ub_mean, fx_sd, vt_sd, bb_sd, ub_sd, .direction = 'updown')

qual_competitors <- qual_competitors[!duplicated(qual_competitors),]

# stack on us competitors
qual_competitors <- rbind(qual_competitors, us_mean_scores)

# now actually simulate qual scores
qual_scores <- qual_competitors %>%
  rowwise() %>%
  mutate(current_fx_sim = list(setNames(rnorm(trials, fx_mean, fx_sd), paste0('fx_', 1:trials))),
         current_vt_sim = list(setNames(rnorm(trials, vt_mean, vt_sd), paste0('vt_', 1:trials))),
         current_bb_sim = list(setNames(rnorm(trials, bb_mean, bb_sd), paste0('bb_', 1:trials))),
         current_ub_sim = list(setNames(rnorm(trials, ub_mean, ub_sd), paste0('ub_', 1:trials)))) %>%
  unnest_wider(current_fx_sim) %>% 
  unnest_wider(current_vt_sim) %>% 
  unnest_wider(current_bb_sim) %>% 
  unnest_wider(current_ub_sim)

# and final scores too
event_final_scores <- qual_competitors %>%
  rowwise() %>%
  mutate(current_fx_sim = list(setNames(rnorm(trials, fx_mean, fx_sd), paste0('fx_', 1:trials))),
         current_vt_sim = list(setNames(rnorm(trials, vt_mean, vt_sd), paste0('vt_', 1:trials))),
         current_bb_sim = list(setNames(rnorm(trials, bb_mean, bb_sd), paste0('bb_', 1:trials))),
         current_ub_sim = list(setNames(rnorm(trials, ub_mean, ub_sd), paste0('ub_', 1:trials)))) %>%
  unnest_wider(current_fx_sim) %>% 
  unnest_wider(current_vt_sim) %>% 
  unnest_wider(current_bb_sim) %>% 
  unnest_wider(current_ub_sim)

aa_final_scores <- qual_competitors %>%
  rowwise() %>%
  mutate(current_fx_sim = list(setNames(rnorm(trials, fx_mean, fx_sd), paste0('fx_', 1:trials))),
         current_vt_sim = list(setNames(rnorm(trials, vt_mean, vt_sd), paste0('vt_', 1:trials))),
         current_bb_sim = list(setNames(rnorm(trials, bb_mean, bb_sd), paste0('bb_', 1:trials))),
         current_ub_sim = list(setNames(rnorm(trials, ub_mean, ub_sd), paste0('ub_', 1:trials)))) %>%
  unnest_wider(current_fx_sim) %>% 
  unnest_wider(current_vt_sim) %>% 
  unnest_wider(current_bb_sim) %>% 
  unnest_wider(current_ub_sim)

team_final_scores <- qual_competitors %>%
  rowwise() %>%
  mutate(current_fx_sim = list(setNames(rnorm(trials, fx_mean, fx_sd), paste0('fx_', 1:trials))),
         current_vt_sim = list(setNames(rnorm(trials, vt_mean, vt_sd), paste0('vt_', 1:trials))),
         current_bb_sim = list(setNames(rnorm(trials, bb_mean, bb_sd), paste0('bb_', 1:trials))),
         current_ub_sim = list(setNames(rnorm(trials, ub_mean, ub_sd), paste0('ub_', 1:trials)))) %>%
  unnest_wider(current_fx_sim) %>% 
  unnest_wider(current_vt_sim) %>% 
  unnest_wider(current_bb_sim) %>% 
  unnest_wider(current_ub_sim)

# and calculate aa scores where necessary
for (trial in 1:trials) {
  qual_scores <- qual_scores %>% 
    mutate(!!as.name(paste0("aa_", trial)) := get(paste0("fx_", trial)) + get(paste0("vt_", trial)) + get(paste0("bb_", trial)) + get(paste0("ub_", trial)))
  
  aa_final_scores <- aa_final_scores %>% 
    mutate(!!as.name(paste0("aa_", trial)) := get(paste0("fx_", trial)) + get(paste0("vt_", trial)) + get(paste0("bb_", trial)) + get(paste0("ub_", trial))) 
}

#=======================#
#=== FEMALE ATHLETES ===#
#=======================#

# Run female simulations
# test
# team_combo <- 1
# only running simulation for two teams for now!!
if (is.na(n_team_combos)) {n_team_combos <- nrow(df_female_us_teams)}

for (team_combo in 1:n_team_combos) {
  
  # print statement
  print(paste0("Running simulation ", team_combo, " out of ", n_team_combos))
  
  ## QUALIFYING ROUND
  # start by subsetting down our simulated scores to only the athletes we want
  simulated_scores <- qual_scores %>% filter(country != 'USA' | fullname %in% as.character(df_female_us_teams[team_combo,1:5]))
  sub_event_scores <- event_final_scores %>% filter(country != 'USA' | fullname %in% as.character(df_female_us_teams[team_combo,1:5]))
  sub_aa_scores <- aa_final_scores %>% filter(country != 'USA' | fullname %in% as.character(df_female_us_teams[team_combo,1:5]))
  sub_team_scores <- team_final_scores %>% filter(country != 'USA' | fullname %in% as.character(df_female_us_teams[team_combo,1:5]))
  
  # Create team scores 
  # create list to hold the teams moving on to final
  teams_in_final <- list()
  
  # for each trial, calculate team scores and aa scores
  foreach(trial = 1:trials) %dopar% {
    
    library(dplyr)
    
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
    
    bb_scores <- simulated_scores %>% 
                 filter(flag_team == 1) %>% 
                 group_by(country) %>% 
                 slice_max(bb_mean, n = 4) %>% 
                 slice_max(get(paste0("bb_", trial)), n = 3) %>% 
                 summarise(bb_score = sum(get(paste0("bb_", trial)), na.rm = T))
    
    ub_scores <- simulated_scores %>% 
                 filter(flag_team == 1) %>% 
                 group_by(country) %>% 
                 slice_max(ub_mean, n = 4) %>%
                 slice_max(get(paste0("ub_", trial)), n = 3) %>% 
                 summarise(ub_score = sum(get(paste0("ub_", trial)), na.rm = T))
   
    # merge them together into one dataframe so we can sum
    team_by_apparatus <- fx_scores %>% 
                         left_join(vt_scores, by = 'country') %>% 
                         left_join(bb_scores, by = 'country') %>% 
                         left_join(ub_scores, by = 'country')
    
    # sum across rows to get one final team score for the trial
    team_scores <- team_by_apparatus %>% 
                   mutate(team_score = fx_score + vt_score + bb_score + ub_score)
    
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
  bb_min <- sub_team_scores %>% filter(country == 'USA') %>% slice_min(order_by = bb_mean, n = 1) %>% pull(fullname)
  ub_min <- sub_team_scores %>% filter(country == 'USA') %>% slice_min(order_by = ub_mean, n = 1) %>% pull(fullname)
  
  sub_team_scores[sub_team_scores$fullname %in% fx_min,startsWith(colnames(sub_team_scores),"fx")] <- NA
  sub_team_scores[sub_team_scores$fullname %in% vt_min,startsWith(colnames(sub_team_scores),"vt")] <- NA
  sub_team_scores[sub_team_scores$fullname %in% bb_min,startsWith(colnames(sub_team_scores),"bb")] <- NA
  sub_team_scores[sub_team_scores$fullname %in% ub_min,startsWith(colnames(sub_team_scores),"ub")] <- NA
  
  # Now, for each trial 
  # test trial <- 1
  for (trial in 1:trials) {
    
    library(dplyr)
    source("Case_Study_1-Gymnastics/35_define_team_selection_functions.R")
    #====================#
    #=== event finals ===#
    #====================#
    
    # simulate event finals 
    fx_final <- event_final('fx', trial, opt_gender = 'w')
    vt_final <- event_final('vt', trial, opt_gender = 'w')
    bb_final <- event_final('bb', trial, opt_gender = 'w')
    ub_final <- event_final('ub', trial, opt_gender = 'w')
    
    #========================#
    #=== all around final ===#
    #========================#
    
    aa_final <- event_final('aa', trial, opt_gender = 'w')
    
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
                                          sum(get(paste0('bb_', trial)), na.rm = T) +
                                          sum(get(paste0('ub_', trial)), na.rm = T)) 
    
    # get the team winners, add on apparatus as a column
    team_winners <- team_final %>% 
                    arrange(desc(final_score)) %>% 
                    head(3) %>% 
                    select(country, final_score) %>% 
                    mutate(final_type = "team")
    
    if (nrow(team_winners) == 3) {}
    # add on medal color as column and subset to us only 
    out_team_winners <- data.frame(team_winners, medal = c('gold', 'silver', 'bronze')) %>% 
                        filter(country == 'USA')
    
    #==============#
    #=== output ===#
    #==============#
    
    # stack together the us medal results from all finals
    out_us_results <- plyr::rbind.fill(fx_final, vt_final, bb_final, ub_final, aa_final, out_team_winners)

    # save results to an object
    ls_medal_winners[[paste0("trial_", trial)]] <- ifelse(nrow(out_us_results) == 0, NA, out_us_results)
  }
  
  # Use the US outcomes to calculate 'weighted medal count' for each trial
  #lapply(ls_medal_winners, medal_count, in_team_combo = team_combo)
  
  foreach(trial = 1:trials) %dopar% {
    
    medals <- ls_medal_winners[[paste0("trial_", trial)]]$medal
    wt_count <- 3*sum(medals == 'gold') + 2*sum(medals == 'silver') + sum(medals == 'bronze')
    # add weighted count to dataframe
    df_female_us_teams[team_combo, paste0('wt_count_trial_', trial)] <- wt_count
    
  }
  
  # save medal winners to an object
  out_female_medal_winners[[paste0("team_combo_", team_combo)]] <- ls_medal_winners
}

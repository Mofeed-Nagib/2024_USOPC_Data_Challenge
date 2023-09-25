# This script defines some helper functions for the team selection / simulation scripts.

# Define function to select top n competitors from each country using mean score
select_competitors <- function(apparatus, select_from_data, from_each_country, add_individuals = 1) {
  
  # pick top athletes by country using mean score
  form_teams <- select_from_data %>%
    filter(flag_team == 1 & !is.na(get(paste0(apparatus, "_mean")))) %>%
    group_by(country) %>%
    slice_max(order_by = get(paste0(apparatus, "_mean")), n = from_each_country)
  
  # dedup
  qual_apparatus_competitors <- form_teams[!duplicated(form_teams),]
  
  # if we also want to consider individual competitors
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


# create function to simulate event finals
# test parameters: 
# in_apparatus = 'fx'
# in_trial_number = 1
event_final <- function(in_apparatus, in_trial_number, opt_gender) {
  
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
  } else if (opt_gender == 'w') {
    
    # if apparatus is aa, we calculate the final score by sampling each apparatus and summing
    competitors <- competitors %>% 
      ungroup() %>% 
      mutate(final_score = rnorm(1, vt_mean, vt_sd) +
               rnorm(1, fx_mean, fx_sd) +
               rnorm(1, bb_mean, bb_sd) +
               rnorm(1, ub_mean, ub_sd))
    
  } else if (opt_gender == 'm') {

      # if apparatus is aa, we calculate the final score by sampling each apparatus and summing
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
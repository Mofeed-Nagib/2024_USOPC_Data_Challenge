library(shiny)
library(bslib)
library(DT)
library(dplyr)

# Define server logic required to draw a histogram
function(input, output, session) {

  #=====================================================#
  #=== calculate medal counts based on weight inputs ===#
  #=====================================================#
  # create datasets with weighted medals
  
  mod_female_teams <- reactive({
    
    for (team_combo in 1:length(female_detailed)) {
      
      ls_medal_winners <- female_detailed[[team_combo]]
      
      for (trial in 1:n_w_trials) {
        
        # calculate weighted count using input weights
        medals <- ls_medal_winners[[trial]]$medal
        wt_count <- input$gold_female_weight*sum(medals == 'gold') + input$silver_female_weight*sum(medals == 'silver') + input$bronze_female_weight*sum(medals == 'bronze')
        
        # add weighted count to dataframe
        sub_female_data[team_combo, paste0('wt_count_trial_', trial)] <- wt_count
        
      } # close inner loop
      
    } # close outer loop
    
    # return data with new team vector column added
    out_data <- sub_female_data %>% rowwise() %>% mutate(teamvec = list(c(athlete_1, athlete_2, athlete_3, athlete_4, athlete_5)))
    
    return(out_data)
  })
  
  mod_male_teams <- reactive({
    
    for (team_combo in 1:length(male_detailed)) {
      
      ls_medal_winners <- male_detailed[[team_combo]]
      
      for (trial in 1:n_m_trials) {
        
        # calculate weighted count using input weights
        medals <- ls_medal_winners[[trial]]$medal
        wt_count <- input$gold_male_weight*sum(medals == 'gold') + input$silver_male_weight*sum(medals == 'silver') + input$bronze_male_weight*sum(medals == 'bronze')
        
        # add weighted count to dataframe
        sub_male_data[team_combo, paste0('wt_count_trial_', trial)] <- wt_count
        
      } # close inner loop
      
    } # close outer loop
    
    # return data with new team vector column added
    out_data <- sub_male_data %>% rowwise() %>% mutate(teamvec = list(c(athlete_1, athlete_2, athlete_3, athlete_4, athlete_5)))
    
    return(out_data)
  })
  
  #========================================================#
  #=== create subsetted datasets based on athlete input ===#
  #========================================================#
  
  # format and subset data tables for display
  display_female_teams <- reactive({
    
    sub_female_teams <- mod_female_teams()
    
    # subset down to the male teams that have been selected
    for (athlete in input$select_females) {sub_female_teams <- sub_female_teams %>% filter(athlete %in% teamvec)}
    
    # format data tables for display
    sub_female_teams %>% rowwise() %>% mutate(avg_wt_medals = rowMeans(across(starts_with("wt_count_trial")), na.rm = T)) %>% 
      dplyr::select(teamvec, avg_wt_medals)
  })
  
  display_male_teams <- reactive({
    
    sub_male_teams <- mod_male_teams()
    
    # subset down to the male teams that have been selected
    for (athlete in input$select_males) {sub_male_teams <- sub_male_teams %>% filter(athlete %in% teamvec)}
    
    # format data tables for display
    sub_male_teams %>% rowwise() %>% mutate(avg_wt_medals = rowMeans(across(starts_with("wt_count_trial")), na.rm = T)) %>% 
      dplyr::select(teamvec, avg_wt_medals)
  })
  
  #============================================================#
  #=== create detailed view datasets based on selected team ===#
  #============================================================#
  
  selected_women <-  reactive({
    
    index <- input$female_teams_rows_selected
    
    if (is.null(input$female_teams_rows_selected)) {return(NULL)}
    
    selected_team <- paste(unlist(display_female_teams()[index,'teamvec']))
    
    use_female_teams <- mod_female_teams()
    
    # add row numbers into a column
    use_female_teams$row_id <- seq(1, nrow(use_female_teams))
    
    # get team combo number we need
    selected_combo <- use_female_teams %>% filter(athlete_1 %in% selected_team & athlete_2 %in% selected_team & athlete_3 %in% selected_team & athlete_4 %in% selected_team & athlete_5 %in% selected_team) %>% pull(row_id)
    
    # get current medal winners
    selected_medal_winners <- data.table::rbindlist(female_detailed[[paste0("team_combo_", selected_combo)]])
    
    # let's aggregte them
    agg_medal_winners <- selected_medal_winners %>% 
                         mutate(final_type = as.factor(toupper(final_type)),
                                medal = as.factor(stringr::str_to_title(medal))) %>% 
                         group_by(fullname, final_type, medal) %>% 
                         summarise(proportion = 100 * n()/n_w_trials)
    
     return(agg_medal_winners)
  })
  
  selected_men <-  reactive({
    
    index <- input$male_teams_rows_selected
    
    if (is.null(input$male_teams_rows_selected)) {return(NULL)}
    
    selected_team <- paste(unlist(display_male_teams()[index,'teamvec']))
    
    use_male_teams <- mod_male_teams()
    
    # add row numbers into a column
    use_male_teams$row_id <- seq(1, nrow(use_male_teams))
    
    # get team combo number we need
    selected_combo <- use_male_teams %>% filter(athlete_1 %in% selected_team & athlete_2 %in% selected_team & athlete_3 %in% selected_team & athlete_4 %in% selected_team & athlete_5 %in% selected_team) %>% pull(row_id)
    
    # get current medal winners
    selected_medal_winners <- data.table::rbindlist(male_detailed[[paste0("team_combo_", selected_combo)]])
    
    # let's aggregte them
    agg_medal_winners <- selected_medal_winners %>% 
      mutate(final_type = as.factor(toupper(final_type)),
             medal = as.factor(stringr::str_to_title(medal))) %>% 
      group_by(fullname, final_type, medal) %>% 
      summarise(proportion = 100 * n()/n_m_trials)
    
    return(agg_medal_winners)
  })
  
  #============================================#
  #=== render data tables for display in ui ===#
  #============================================#
  
  # create data tables
  output$female_teams <- renderDT(datatable(display_female_teams(), 
                                            colnames = c("Team", "Average Weighted Medal Count"),
                                            selection = list(mode = 'single')))
  output$male_teams <- renderDT(datatable(display_male_teams(), 
                                          colnames = c("Team", "Average Weighted Medal Count"),
                                          selection = list(mode = 'single')))
  
  output$female_medals_detailed <- renderUI(
    if (is.null(selected_women())) {HTML("Select a row above to see a more detailed view.")} else {
      
        datatable(selected_women(),
                  colnames = c('Athlete', 'Event', 'Medal Outcome', "Percent of Trials with Outcome"),
                  filter = 'top')
      }
    )
  
  output$male_medals_detailed <- renderUI(
    if (is.null(selected_men())) {HTML("Select a row above to see a more detailed view.")} else {
      
      datatable(selected_men(),
                colnames = c('Athlete', 'Event', 'Medal Outcome', "Percent of Trials with Outcome"),
                filter = 'top')
    }
  )
  
}

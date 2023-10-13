library(shiny)
library(bslib)
library(DT)
library(dplyr)

# Define server logic required to draw a histogram
function(input, output, session) {

  # format and subset data tables for display
  display_female_teams <- reactive({
    
    sub_female_teams <- mod_female_teams
    # subset down to the male teams that have been selected
    for (athlete in input$select_females) {sub_female_teams <- sub_female_teams %>% filter(athlete %in% teamvec)}
    
    # format data tables for display
    sub_female_teams %>% rowwise() %>% mutate(avg_wt_medals = rowMeans(across(starts_with("wt_count_trial")), na.rm = T)) %>% 
      select(teamvec, avg_wt_medals)
  })
  
  # format and subset data tables for display
  display_male_teams <- reactive({
    
    sub_male_teams <- mod_male_teams
    # subset down to the male teams that have been selected
    for (athlete in input$select_males) {sub_male_teams <- sub_male_teams %>% filter(athlete %in% teamvec)}
    
    # format data tables for display
    sub_male_teams %>% rowwise() %>% mutate(avg_wt_medals = rowMeans(across(starts_with("wt_count_trial")), na.rm = T)) %>% 
      select(teamvec, avg_wt_medals)
  })
  
  # print the selected team
  selected_women <-  reactive({
    index <- input$female_teams_rows_selected
    
    selected_team <- paste(unlist(display_female_teams()[index,'teamvec']))
    
    # add row numbers into a column
    mod_female_teams$row_id <- seq(1, nrow(mod_female_teams))
    
    # get team combo number we need
    selected_combo <- mod_female_teams %>% filter(athlete_1 %in% selected_team & athlete_2 %in% selected_team & athlete_3 %in% selected_team & athlete_4 %in% selected_team & athlete_5 %in% selected_team) %>% pull(row_id)
    
    # get current medal winners
    selected_medal_winners <- data.table::rbindlist(female_detailed[[paste0("team_combo_", selected_combo)]])
    
    # let's aggregte them
    agg_medal_winners <- selected_medal_winners %>% 
                         mutate(final_type = as.factor(toupper(final_type)),
                                medal = as.factor(stringr::str_to_title(medal))) %>% 
                         group_by(fullname, final_type, medal) %>% 
                         summarise(proportion = 100 * n()/n_trials)
    
    agg_medal_winners
  })
  
  # create data tables
  output$female_teams <- renderDT(datatable(display_female_teams(), 
                                            colnames = c("Team", "Average Weighted Medal Count"),
                                            selection = list(mode = 'single')))
  output$male_teams   <- renderDT(display_male_teams())
  
  output$female_medals_detailed <- renderDT(datatable(selected_women(),
                                                      colnames = c('Athlete', 'Event', 'Medal Outcome', "Percent of Trials with Outcome"),
                                                      filter = 'top'))
  
}

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

  # format and subset data tables for display
  display_female_teams <- reactive({
    
    # subset down to the male teams that have been selected
    sub_female_teams <- female_data %>% rowwise() %>% mutate(teamvec = list(c(athlete_1, athlete_2, athlete_3, athlete_4, athlete_5)))
    for (athlete in input$select_females) {sub_female_teams <- sub_female_teams %>% filter(athlete %in% teamvec)}
    
    # format data tables for display
    sub_female_teams %>% rowwise() %>% mutate(avg_wt_medals = rowMeans(across(starts_with("wt_count_trial")), na.rm = T)) %>% 
      select(teamvec, avg_wt_medals)
  })
  
  
  #sub_male_teams <- male_data %>% rowwise() %>% mutate(teamvec = list(c(athlete_1, athlete_2, athlete_3, athlete_4, athlete_5)))
  #for (athlete in input$select_males) {sub_male_teams <- sub_male_teams %>% filter(athlete %in% teamvec)}
  
  # create data tables
  output$female_teams <- renderDT(display_female_teams())
}

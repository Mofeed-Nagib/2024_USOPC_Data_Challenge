library(shiny)
library(bslib)
library(DT)
library(dplyr)

# source global
source("global.R")

# Define UI for application that draws a histogram
fluidPage(

    # apply theme for aesthetic purposes
    theme = bs_theme(bootswatch = "flatly"),

    # create navbar layout
    navbarPage(title = 'UCSAS 2024 USPOC Data Challenge: US Olympic Gymnastics',
                
      # male tab
      tabPanel("Men's Team",
               
               selectizeInput("select_males",
                           "Select up to 5 male athletes for your team:",
                           choices = all_male_athletes,
                           multiple = T,
                           options = list(maxItems = 5)),
               
               # display results
               DTOutput("male_teams")
               
               ), # end men's tab
      
      # women's tab
      tabPanel("Women's Team",
               
               # open siderbar layour
               sidebarLayout(
                 
                 # open sidebar panel
                 sidebarPanel(
                   
                   # select team
                   selectizeInput("select_females",
                                  "Select up to 5 female athletes for your team:",
                                  choices = all_female_athletes,
                                  multiple = T,
                                  options = list(maxItems = 5)) # close select input
                   ), # close sidebar panel
                 
                # open main panel
                mainPanel(
                  
                  # open well panel 
                  wellPanel(
                    
                    # explanatory text
                    h3("Team Results"),
                    p("The table below shows the average weighted medal count across our simulations. Weighted medal count is calculated as 3 * (# of gold medals) + 2 * (# of silver medals) + 1 * (# of bronze medals). Click on a team to see more detailed results below."),
                    
                    # display results
                    DTOutput("female_teams")
                  ), # close well panel
                  
                  hr(), 
                  # open well panel to display detailed results
                  wellPanel(
                    h3("Detailed View"),
                    DTOutput("female_medals_detailed")
                  )
                  
                ) # close main panewl
               ) # close sidebar layout
              ) # end women's tab
      
    ) # end tabset
) # close page

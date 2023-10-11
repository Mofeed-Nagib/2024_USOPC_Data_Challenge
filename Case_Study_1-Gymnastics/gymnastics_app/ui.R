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
               
               # select team
               selectizeInput("select_females",
                              "Select up to 5 female athletes for your team:",
                              choices = all_female_athletes,
                              multiple = T,
                              options = list(maxItems = 5)),
               
               
               # display results
               DTOutput("female_teams"),
               
               DTOutput("female_medals_detailed")
               ) # end women's tab
      
    ) # end tabset
) # close page

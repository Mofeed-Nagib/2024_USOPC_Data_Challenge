library(shiny)
library(bslib)

# Define UI for application that draws a histogram
fluidPage(

    # apply theme for aesthetic purposes
    theme = bs_theme(bootswatch = "flatly"),

    # create navbar layout
    navbarPage(title = 'UCSAS 2024 USPOC Data Challenge: US Olympic Gymnastics',
                
      # male tab
      tabPanel("Men's Team"), # end men's tab
      
      # women's tab
      tabPanel("Women's Team") # end women's tab
      
    ) # end tabset
) # close page

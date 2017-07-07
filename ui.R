
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(knitr)
source("global.R")

shinyUI(fluidPage(

  # Application title
  titlePanel("State TEA tracts"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(

    sidebarPanel(
      
      selectInput("state", "Choose a state:", 
                  choices = state_list),
      submitButton("Update State"),
      p(" "),
      uiOutput('choose_proj'),
      submitButton("Update Project")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot"),
      dataTableOutput('table')
    )
  )
))

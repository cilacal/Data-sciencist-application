library(shiny)
library(ggplot2)
library(data.table)

#Creating the user interface of the application
shinyUI(pageWithSidebar(
  
  headerPanel("Distribution of age by marital-status"),
  #Adding the filter field to the side bar of the panel of the application
  sidebarPanel(
    selectInput("marital.status","Choose a marital-status:",
                choices = levels(data_train$marital.status))
  ),
  mainPanel(
    plotOutput("distPlot")
  )
)
)

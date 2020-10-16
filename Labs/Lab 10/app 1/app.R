####################
#install.packages('rsconnect')
# Author: Laura C. Larregui
#
# Purpose: Week 10 App 1
#####################

library(shiny)
library(lubridate)

server<- function(input, output){
  
  }

ui<- fluidPage(
  mainPanel(paste("Laura's shinny app at", now()))
)
shinyApp(ui, server)
####################
#
# Author: Laura C. Larregui
#
# Purpose: Week 10 lab
#####################
library(shiny)

server<- function(input, output){
  output$myPie<- renderPlot({
    pie(c(8,12,3), main = "Hello World")
  })
}
ui<- fluidPage(
  mainPanel(plotOutput("myPie"))
)
shinyApp(ui, server)
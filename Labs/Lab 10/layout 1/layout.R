#layouts
library(shiny)
server<- function(input, output){
  piefunction<- function(x){
    par(mar=c(.5,.5,.5,.5))
    pie(1:x)
  }
  output$plot1<- renderPlot({piefunction(1)})
  output$plot2<- renderPlot({piefunction(2)})
  output$plot3<- renderPlot({piefunction(3)})
  output$plot4<- renderPlot({piefunction(4)})
  output$plot5<- renderPlot({piefunction(5)})
  output$plotlong<- renderPlot({barplot(sample(1:10,4), horiz=T)})
  
  
}
ui<- fluidPage(
  titlePanel("Hello Shiny!"),
  
  fluidRow(
    column(4, plotOutput("plot1")),
    column(2, plotOutput("plot2")),
    column(2, plotOutput("plot3")),
    column(4, plotOutput("plot4"))
    
  ),
  fluidRow(
    column(8, plotOutput("plotlong")),
    column(4, plotOutput("plot5"))
    
    
  )
)
shinyApp(ui,server)
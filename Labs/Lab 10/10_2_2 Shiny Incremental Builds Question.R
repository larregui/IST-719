Server <- function(input, output) {
  output$myOtherPlot <- renderPlot({
    var3 <- 80
  
  })
  
  var1 <- 10
  
  output$myPlot1 <- renderPlot({
    var2 <- 37
    
    local.function <- function () {
      print(var3)
    }    
    local.function()
  })
}

UI<- fluidPage(
  titlePanel("ACME Art Company Dashboard"),
  mainPanel(
    plotOutput("myOtherPlot")
  )
)
shinyApp(ui=UI, server=Server)

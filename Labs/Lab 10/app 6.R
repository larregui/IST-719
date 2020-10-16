#
# app.r is leaflet example library data
#
library(shiny)
library(leaflet) # raster!
library(ggplot2)
library(ggmap)
dir<- "C:\\Users\\laura\\OneDrive\\Documents\\Syracuse iSchool\\IST 719\\IST-719\\Labs\\Lab 7\\"
libs<-read.csv(paste0(dir, "newyorklibraries.xls"), stringsAsFactors = FALSE)
ny.libs<- nrow(libs)

server<- function(input, output, session){
  print("server::start")
  
  point<- eventReactive(input$num.libs, {
    index<- sample(1:nrow(libs), input$num.libs)
    addys<- paste(libs$ADDRESS[index], libs$CITY[index], libs$STABR[index], sep=",")
    g.codes<- geocode(addys, source ="dsk")
    df<- data.frame(lon=g.codes$lon, lat=gcodes$lat, addy=addys)
    df
      }, ignoreNULL = FALSE)
  output$mymap<- renderLeaflet({
    M<- leaflet()
    M<- addProviderTiles(M, providers$OpenStreetMap,options = providerTileOptions(noWrap = TRUE) )
    df<- point()
    addMarkers(M, lng = df[,1], lat= df[,2], popup = df[,3])
  })
}
ui<- fluidPage(
  leafletOutput("mymap"),
  numericInput("num.libs", "Number of Libraries", 10, min=1, max=ny.libs)
  )

shinyApp(ui,server)
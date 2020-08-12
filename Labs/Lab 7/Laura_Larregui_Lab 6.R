##############################
#
# Author: Laura c. Larregui
# Purpose: Week 7 Lab: Maps
#
##############################

d<- file.choose()
df<- read.csv(d, header=TRUE, stringsAsFactors = FALSE)

plot(df$x, df$y)
polygon(df$x, df$y, col ="firebrick1", border = NA)
#new york
# map data is a bunch of points

library(maps)
library(mapproj)

map(database = "world", regions = 'Puerto Rico', fill = TRUE, col= "lightblue3")
m<- map("state")

plot(m$x, m$y)

map("state", fill=TRUE, col=c("orange", "red", "yellow"))

map("county", region = "New York", fill = TRUE, col=terrain.colors(20))
    
library(rnaturalearth)
install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
library(rnaturalearthhires)
india<- ne_states(country="India")

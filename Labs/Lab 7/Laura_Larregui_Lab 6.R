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

map(india)
india
attributes(india)
names(india)
india$name
map(india, namefield="name", region=c("Gujarat", "Rajasthan", "Madhya","Pradesh")
    , fill= TRUE
    , col =c("orangered","white","springgreen4"))
install.packages("raster")
library(raster)
india<- raster::getData("GADM", country="IND", level=1)
map(india)
map(india, namefield = "NAME_1", region= "Gujarat")

india<- raster::getData("GADM", country="IND", level=2)
map(india)
map(india, namefield = "NAME_2", region= "North 24 Parganas"
    , fill=TRUE
    , col="springgreen4")

china<- raster:: getData("GADM", country= "CHN", level=2)
map(china)

pr<- raster::getData("GADM", country="Puerto Rico", level=1)
map(pr)
map(pr, namefield = "NAME_1"
    , fill=TRUE
    , col="springgreen4")
View(pr$NAME_1)

#######################################
# Choropleth map problem
#######################################
#thematic maps, color maps to 
s<- file.choose()
load(s)
#which state has the most mass-shooting victims in the US?
View(shootings)
shootings$Total.Number.of.Victims
sort(shootings$State)
tmp.vec<- gsub("^\\s+/\\s+$", "", shootings$State)
sort(tmp.vec)
shootings$State<- tmp.vec
agg.dat<- aggregate (shootings$Total.Number.of.Victims, list(shootings$State), sum)
colnames(agg.dat)<- c("State", "victims")
num.cols<- 100
my.color.vec<- rev(heat.colors(num.cols))
pie(rep(1, num.cols), col=my.color.vec)
my.color.vec[1]
my.color.vec[4]
agg.dat
library(plotrix)
agg.dat$index<- round(rescale(x= agg.dat$victims, c(1, num.cols)),0)
agg.dat$color<- my.color.vec[agg.dat$index]

tmp<-gsub("[[:upper:]]|\\s+|e", "", shootings$State)

###########################
# Choropleth Map problem
m<- map("state")
m$names
state.order<- match.map(database="state", regions = agg.dat$State, exact = FALSE, warn = TRUE)
cbind(m$names, agg.dat$state[state.order])
map("state", col= agg.dat$color[state.order], fill=TRUE, resolution = 0, lty= 1, projection ="polyconic", border ="tan")

###########################
# points and geocoding

library(ggmap)
libs<-file.choose()
lib<- read.csv(libs, header=TRUE, stringsAsFactors = FALSE, quote="\"")

map("world")
points(0,0, col="red", cex=3, pch=8)
abline(h=43, col="blue", lty=3)
abline(v=-76, col="blue", lty=3)

us.cities
map("state")
my.cols<- rep (rgb(1,.6,.2,.7), lenth(us.cities$name))
my.cols[us.cities$capital>0]<- rgb(.2,.6,1,.9)
points(us.cities$long, us.cities$lat, col=my.cols, pch=16, cex= rescale(us.cities$pop, c(.5,7)))

geocode("3649 Erie Blvd East, Dewitt, ny", source = "dsk")
table(lib$CITY)
index<- which(lib$CITY%in% c("SYRACUSE","DEWITT", "FAYETTEVILLE"))
addy<- paste(lib$ADDRESS[index], lib$CITY[index], lib$STABR[index])
map("county", "new york", fill = TRUE, col="orange")
#g.codes<- geocode(addy, source = "dsk")
#points(g.codes$lon, g.codes$lat, col="blue", cex=1.1, pch=16)

#######################
#RWorldMaps Package

library(rworldmap)
library(plotrix)
c<-file.choose()
countries<- read.delim(c, quote = "\"", header = TRUE, sep = ";", stringsAsFactors = FALSE)
range(countries$Life.expectancy)
#zap<- which(countries$Life.expectancy ==0.0)
countries<- countries[-zap,]
num.cat<- 10
iso3.codes<- tapply(countries$Country..en., 1:length(countries$Country..en.), rwmGetISO3)
df<- data.frame(country=iso3.codes, labels=countries$Country..en.
                , life=countries$Life.expectancy)
df.map<- joinCountryData2Map(df, joinCode= "ISO3"
                              , nameJoinColumn= "country")
par(mar=c(0,0,1,0))
mapCountryData(df.map
               , nameColumnToPlot= "life"
               , numCats= num.cat
               , catMethod= 
                  c("pretty","fixedwidth","diverging", "quantiles")[4]
               , colourPalette= colorRampPalette(
                 c("orangered", "palegoldenrod", "forestgreen")
               )(num.cat)
                ,oceanCol = "royalblue4"
                ,borderCol = "peachpuff4"
                , mapTitle = "Life expectancy"
               )

###########################
#ggmaps
library(ggmap)
library(ggplot2)
library(raster)
r<- file.choose()
reported<- read.csv(r, header=TRUE, quote= "\"", stringsAsFactors = FALSE)
india<- raster::getData("GADM", country="IND", level=1)
cbind(unique(reported$Area_Name), india$NAME_1)
india$NAME_1[india$NAME_1 == "NCT of Delhi"]<- "Delhi"
india$NAME_1<- gsub("and", "&", india$NAME_1)
map<- fortify(india, region="NAME_1")
head(map)
crimes<- aggregate(reported$Cases, list(reported$Area_Name), sum)
colnames(crimes)<- c("id", "ReportedRapes")
crimes[order(crimes$ReportedRapes),]
my.map<- merge(x=map, y=crimes, by="id")
ggplot()+ geom_map(data=my.map, map= my.map)+ 
    aes(x=long, y=lat, map_id = id, group=group, fill=ReportedRapes)
    #theme_minimal()+ ggtitle("Reported Rapes in India")

    ggplot(my.map, aes(map_id = id)) +
        geom_map(aes(fill = ReportedRapes), map = my.map) +
        expand_limits(x = my.map$long, y = my.map$lat) +theme_minimal()+ ggtitle("Reported Rapes in India")
####################
    
library(stringr)
library(rgdal)    
library(raster) 
install.packages("TeachingDemos")
library(TeachingDemos) 

dir<- "C:\\Users\\laura\\OneDrive\\Documents\\Syracuse iSchool\\IST 719\\IST-719\\Labs\\Lab 7\\"
b<-file.choose()
s<- file.choose()
bikes<- readRDS(b)
nypp<- readOGR(paste0(dir, "nyct2010_17a"), "nyct2010", stringsAsFactors = FALSE)
syr.n<- readOGR(s)

par(mar=c(.5,.5,.5,.5))
plot(nypp, border= "bisque4", lwd=.5)
zoomPlot(c(978000, 999800), ylim=c(185000, 225000))

df<- data.frame(lat=bikes$start.station.latitude,
                lon=bikes$start.station.longitude)
head(df)
point.tab<- sort(table(paste(df$lat, df$lon)), decreasing=TRUE)
point.tab[1:3]
df.2<- data.frame(lat=as.numeric(word(names(point.tab), 1)),
                  lon=as.numeric(word(names(point.tab), 2)))
df.2$size<- as.numeric(point.tab)
coordinates(df.2)<- ~lon + lat
crs(df.2)<- CRS("+proj=longlat+datum=WGS84")
df.2<- spTransform(df,crs(nypp))
tmp.size<- .2 + (2*df.2$size/max(df.2$size))
points(df.2$lon, df.2$lat, col = "red", pch=19, cex = tmp.size)

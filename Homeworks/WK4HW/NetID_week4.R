# NetID_week4.R
# Author: Laura C. Larregui
# Purpose: Homework 4
#Second Week (7/26/2020)

#######################
#
# Part 1: Modifying Plots with Illustrator
#
#######################
hotdogs<- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv", sep=",", header = TRUE)
#head(hotdogs)

# Bar chart Figure 4-11
fill_colors<- c()
for( i in 1:length(hotdogs$New.record)){
  if (hotdogs$New.record[i]==1){
    fill_colors<- c(fill_colors, "#821122")
  } else {
    fill_colors<- c(fill_colors, "#cccccc")
  }
}
barplot(hotdogs$Dogs.eaten, names.arg=hotdogs$Year, col = fill_colors
        ,border = NA, space = 0.3, xlab = "Year", ylab = "Hot dogs and buns (HDB) eaten"
        ,main = "Nathan's Hot Dog Eating Contest Results, 1980-2010")

# Stacked bar chart: Figure 4-22

hot_dog_places<- read.csv("http://datasets.flowingdata.com/hot-dog-places.csv", sep=",", header = TRUE)
#head(hot_dog_places)
names(hot_dog_places)<- c("2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
hot_dog_matrix<- as.matrix(hot_dog_places)
barplot(hot_dog_matrix, border = NA, space = 0.25, ylim = c(0,200)
        , xlab = "Year", ylab = "Hot dogs and buns (HDBs) eaten"
        , main = "Hot Dog Eating Contest Results, 1980-2010")

#	Scatterplot: Figure 4-28

subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv", sep = ",", header = TRUE)
subscribers[1:5,]
plot(subscribers$Subscribers, type = "h", ylim = c(0, 30000)
     , xlab="Day", ylab="Subcribers")
points(subscribers$Subscribers, pch = 19, col= "black")

#	Time series: Figure 4-34

population <- read.csv("http://datasets.flowingdata.com/world-population.csv", sep =",", header= TRUE)
head(population)
plot(population$Year, population$Population, type="l"
     , ylim = c(0, 7000000000), xlab="Year", ylab = "Population")

# Step chart: Figure 4-43
postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv", sep = ",", header = TRUE)
head(postage)
plot(postage$Year, postage$Price, type = "s"
     , main = "US Postage Rates for Letters, First Ounce, 1991-2010"
     , xlab = "Year", ylab = "Postage Rate (Dollars)")

#LOESS curve: Figure 4-47

unemployment<- read.csv("http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv"
                        , sep=",")
unemployment[1:10,]
plot(1:length(unemployment$Value), unemployment$Value)
scatter.smooth(x=1:length(unemployment$Value),
               y=unemployment$Value, ylim=c(0,11), degree=2, col="#CCCCCC", span =0.5)
#
#######################
#
# Part 2: VT, Chapter 6, Plots
#
#######################
#Scatterplot matrix Figure 6-9 
crime<-read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.csv"
                , sep=",", header = TRUE)
crime2<- crime[crime$state != "District of Columbia",]
crime2<- crime2[crime$state != "United States",]
plot(crime2[,2:9], panel=panel.smooth, main="Rates per 100,000 population", col="gray")

#Bubble chart Figure 6-15 
crime3<- crime[crime$state != "District of Columbia",]
radius<-sqrt(crime3$population/pi)
symbols(crime3$murder, crime3$burglary, circles = radius, inches = 0.60,
        fg="white", bg="red", xlab="Murder Rate", ylab="Burglary Rate", main= "MURDERS VERSUS BURGLARIES IN THE UNITED STATES" )
text(crime3$murder, crime3$burglary, crime3$state, cex = 0.5)

#Histogram Figure 6-24 
birth<- read.csv("http://datasets.flowingdata.com/birth-rate.csv")
stem(birth$X2008)
hist(birth$X2008, breaks = 15, col = "#800080", main="GLOBAL DISTRIBUTION OF BIRTH RATES")

#Density plot Figure 6-32
birth2008<- birth$X2008[!is.na(birth$X2008)]
d2008<- density(birth2008)
density.default(x=birth2008)
d2008frame<- data.frame(d2008$x, d2008$y)
write.table(d2008frame, "birthdensity.txt", sep = "\t")
write.table(d2008frame, "birthdensity.txt", sep = ",", row.names = FALSE)
plot(d2008, type = "n")
polygon(d2008, col = "#821122", border = "#cccccc")

#Multiplots
fname<- file.choose() # to find the path to a data set

art <- read.csv(file = fname
                , header = TRUE
                , stringsAsFactors = FALSE)
library(lattice)

birth_yearly<- read.csv("http://datasets.flowingdata.com/birth-rate-yearly.csv")
histogram(~rate | year, data=birth_yearly, layout=c(10,5))
birth_yearly.new<- birth_yearly[birth_yearly$year<132,]
birth_yearly$year<- as.character(birth_yearly$year)
h<-histogram(~ rate | year, data=birth_yearly, layout=c(10,5))
update(h, index.cond=list(c(41:50, 31:40, 21:30, 11:20, 1:10)))
#I'm using the dataset birth rate tearly from ch. 6

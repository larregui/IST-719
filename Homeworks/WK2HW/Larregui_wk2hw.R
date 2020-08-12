# Larregui_wk2hw.R
# Author: Laura C. Larregui
# Purpose: Homework 2
#Second Week (7/12/2020)

#######################
#
# Part 1
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

#######################
#
# Part 2
#
#######################
fname<- file.choose() # to find the path to a data set

 art <- read.csv(file = fname
                 , header = TRUE
                 , stringsAsFactors = FALSE)



# What is the distribution of total.sales for the whole dataset? Provide two different plots that show two different ways of showing distribution. 
# Title your plot(s): Distribution of total.sales
#plot(art$total.sale
#     , main = "Distribution of total.sales"
#     , pch = 8
#     , col = "lightblue3"
#     , ylab = "Amount in Dollars"
#     , xlab = "Frecuency"
#     )
#plot(sort(art$total.sale))
par(mfrow = c(2,2))
boxplot(art$total.sale
        , main = "Distribution of total.sales"
        , col = "lightblue3"
        , pch = 8
        , ylab = "Amount in Dollars")

d<- density(art$total.sale)
plot(d,main = "Distribution of total.sales")
polygon (d, col = "lightblue3")
        
# Next we want to compare the distributions of subsets of total.sales. 
# Use a third type of distribution plot (different from what you used for the question above) for both of these plots.
## What is the distribution of the totals sales for drawing paper? Title your plot "distribution of the totals sales for drawing paper"
## What is the distribution of the totals sales for watercolor paper? Title your plot: "distribution of the totals sales for watercolor paper"
art.D<-art[art$paper == "drawing" , ]
#View(art.D)
art.w<-art[art$paper == "watercolor" , ]
#View(art.D)

#par(mfrow = c(2,1), mar = c(2,3,1,2))
hist(art.D$total.sale, xlab = "Total Sale", main = "Distribution of the Totals Sales for Drawing Paper", col="#d5f4e6")
hist(art.w$total.sale, xlab = "Total Sale", main = "Distribution of the Totals Sales for Watercolor Paper", col="#d5f4e6")




#######################
#
# Part 3
#
#######################

# Is there a relationship between the unit price of art goods and their units sold? 
#If so, what kind of relationship is it? Indicate which plot answers this question.
par(mfrow = c(2,2))
cor(art$unit.price, art$units.sold)
#plot(art$unit.price, art$units.sold)
plot(unit.price~units.sold,
        data=art,
        main="Relationship between Units Sold and Unit Price",
        xlab="Units Sold",
        ylim= c(0,30),
        ylab="Unit Price in US Dollars",
        col="steelblue"
        )

### It seems that art pieces that are higher in price sell less units than those with a lower price.

# Does the art company sell more units of drawing paper or watercolor paper? Indicate which plot answers this question.
d<-sum(art.D$units.sold)
w<-sum(art.w$units.sold)
#table(sum(art.D$units.sold))
#table(sum(art.w$units.sold))
dw.bind <- cbind(d,w)
barplot(dw.bind,beside=T,names.arg = c("Drawing", "Watercolor"), col=c("#d5f4e6","#80ced6"),
        main="Comparison of Paper Units Sold", ylab = "Units Sold")


#Does the art company bring in more money (income) selling drawing paper or watercolor paper? Indicate which plot answers this question.
art1 <- tapply(art$total.sale,list(art$paper),FUN=sum)
barplot(art1,col=c("#d5f4e6","#80ced6"),beside = T,
        main="Paper Sales Income", ylab = "Income in US Dollars", ylim= c(1,120000))
#sum(art.D$total.sale)
#sum(art.w$total.sale)

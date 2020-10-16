# Team: Laura C. Larregui, , 
# Purpose Viz-A-Tho 9/7/2020

#Load Libraries
library(hrbrthemes)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(dplyr)
library(paletteer)
library(RColorBrewer)
#Load Data

dir<- "C:\\Users\\laura\\OneDrive\\Documents\\Syracuse iSchool\\IST 719\\IST-719\\vizathon\\"
buoy<-read.csv(paste0(dir, "BuoyData.csv"), header=TRUE, stringsAsFactors = FALSE)

#Check for missing values
(sum(is.na(buoy))) #4746 missing values
buoy<-na.omit(buoy) # Just to make sure there aren't any missing values

#Check for duplicates
sum(duplicated(buoy))# Zero duplicated record
#buoy<-buoy[!duplicated(buoy),]# Removed duplicated record

# Create Date variable
buoy$ï..DATE_TIME<- as.Date(buoy$ï..DATE_TIME, "%m/%d/%Y")
buoy$YEAR<- as.factor(format(buoy$ï..DATE_TIME,"%Y"))
#buoy$MONTH<-as.factor(format(buoy$ï..DATE_TIME,'%m'))
buoy$MONTH<-as.character(format(buoy$ï..DATE_TIME,'%m'))
#str(buoy)

#Correlation Plot
buoy2<-buoy[,-1]
buoy2<-buoy2[,-13]
buoy2<-buoy2[,-12]
buoy_cor <- cor(buoy2) 
round(buoy_cor, 2) 
corrplot(buoy_cor, method = 'number', type = "upper", tl.col = "black", tl.srt = 45)

#change the values in the variable 'month'
str(buoy)
buoy$MONTH[buoy$MONTH=="03"] <- "03-MAR"
buoy$MONTH[buoy$MONTH=="04"] <- "04-APR"
buoy$MONTH[buoy$MONTH=="05"] <- "05-MAY"
buoy$MONTH[buoy$MONTH=="06"] <- "06-JUN"
buoy$MONTH[buoy$MONTH=="07"] <- "07-JUL"
buoy$MONTH[buoy$MONTH=="08"] <- "08-AUG"
buoy$MONTH[buoy$MONTH=="09"] <- "09-SEP"
buoy$MONTH[buoy$MONTH=="10"] <- "10-OCT"
buoy$MONTH[buoy$MONTH=="11"] <- "11-NOV"

#buoy$MONTH<- factor(buoy$MONTH, levels = month.name)

#Barplot that shows tha amount of precipitation by year and by Month
x<- tapply(buoy$PRECIP_in, list(buoy$YEAR, buoy$MONTH),sum)
x
barplot(x, main="Amount of Precipitation by Year and by Month", beside = TRUE,  ylim = c(0,300),
        col= brewer.pal(3, "BuPu"),legend.text = rownames((x)),args.legend=list(x="topright",bty="s"))

plot(buoy$pH, buoy$PRECIP_in)
plot(buoy$T_DEGC, buoy$SC_us_cm, main="Temperature VS Conductance", xlab = "Temperature in Celsius", ylab = "Specific Conductance")

ggplot(buoy, aes(x=Dox_mg_L, y=Chl_ug_L, colour=pH)) + geom_point(aes(colour=pH)) +
   theme(legend.justification = c(1, 0), legend.position = c(1, 0))


#histogram of pH along the Years
p <- buoy %>%
  ggplot( aes(x=pH,fill=YEAR)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=brewer.pal(3, "BuPu")) +
  theme_ipsum() +
  labs(fill="")

##############################################################3
p<- ggplot(buoy)+ aes(x=pH, PRECIP_in)
p+geom_histogram()+ stat_bin(binwidth = 20)
p + stat_density()

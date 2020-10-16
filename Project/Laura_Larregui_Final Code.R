##########################################
# Author: Laura C. Larregui
# Purpose: IST 719 Final Poster Project
# Topic: A Look at Firefighter Fatalities 2000 - 2018
##########################################
# Load Dataset
##########################################

fire<-read.csv("C:\\Users\\laura\\OneDrive\\Documents\\Syracuse iSchool\\IST 719\\IST-719\\Project\\Dataset\\ff_data.csv"
               , header = TRUE, sep = ",", stringsAsFactors = TRUE)

##########################################
# Load necessary libraries
##########################################
library(ggvis)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(paletteer)
library(RColorBrewer)
library(packcircles)
library(viridis)
##########################################
# Checking Dataset
##########################################

str(fire)
#2291 obs. of  16 variables
# Dataset score
#(16*4)*(2291/100)
# = 1466.24

#Check for missing values
#(sum(is.na(fire))) #360 missing values
fire<-na.omit(fire) # Just to make sure there aren't any missing values
#str(fire)# 1931 obs. of  16 variables

#Check for duplicates
#duplicated(fire)# One duplicated record
#fire<-fire[!duplicated(fire),]# Removed duplicated record
#str(fire) #1931 obs. of  16 variables


##########################################
# Formating Dataset
##########################################

#Formatting data
fire <- fire %>% mutate(Fatality.Count = 1)

## Dates
fire$Incident.date <- as.Date(fire$Incident.date,"%b %d, %Y")
fire$Date.of.death <- as.Date(fire$Date.of.death,"%b %d, %Y")
#extract the year and convert to numeric format
fire$death.year <- as.numeric(format(fire$Date.of.death, "%Y"))
#extract the year and convert to numeric format
fire$death.month <- as.factor(format(fire$Date.of.death, "%b"))

#time convalencing
fire$Days<-fire$Date.of.death  - fire$Incident.date
fire<- fire[(fire$Days>=0),]
fire$Days<- as.numeric(fire$Days)

# Regrouping Wildland Classifications into one
fire$Classification <- sub("Wildland Full-Time", "Wildland", fire$Classification )
fire$Classification <- sub("Wildland Part-Time", "Wildland", fire$Classification )
fire$Classification <- sub("Wildland Contract", "Wildland", fire$Classification )

#Removing the empty records in fire$Classification
fire$Classification <- sub("^$", "Unknown", fire$Classification )
fire<- fire[!(fire$Classification == "Unknown"),]


#Only looking at 2000-2018
#fire<- fire[(fire$year>1999),]

##########################################
#Statistics
##########################################
#Average Age
avgAge<-mean(na.omit(fire$Age)) #47
#Average Days Convalencing
avgDaysConvalencing<-mean(na.omit(fire$Days)) #60
#Total number of deaths by month
monthHigh<- tapply(fire$Fatality.Count, list(fire$death.month), sum)
#Creating a subset were 9/11 deaths are excluded
ff<-fire[!(fire$Incident.date== "2001-09-11"),]
#Average deaths per month per year excluding 9/11
monthavg<- na.omit(ff) %>% group_by(death.month, death.year)%>% tally()
monthavg<- mean(monthavg$n)#8
#Number of deaths in 9/11
nineeleven<- count(fire[(fire$Incident.date == "2001-09-11"),])#344

##########################################
# One dimensional Plots
##########################################

#Pie Chart-Emergency Fatalities Distribution
emergency<- table(fire$Emergency)
pie(f, labels = emergency,col = c("#ffc10e","#dd3333"))
legend("topright", c("Non-Emergency","Emergency"), cex = 0.8,
       fill = c("#ffc10e","#dd3333"))

##Barplot-Top 10 causes of death
dataCause <- fire %>% group_by(Cause.of.death)%>% tally()
dataCauseSort <- arrange(dataCause, desc(n))[1:10,]
dataCauseSort$Cause.of.death <- sub("^$", "Unknown", dataCauseSort$Cause.of.death)
ggplot(dataCauseSort, aes(x=reorder(Cause.of.death,n), n)) + geom_bar(stat = "identity", fill="#dd3333") +coord_flip()+ labs(x = "Cause of Death", y = "Number of Fatalities", title = "Top 10 Causes of Death 2000-2018")+ geom_text(aes(label = scales::percent(n), y = n, group = Cause.of.death),position = position_dodge(width = 0.5), vjust = 1.5)


#Bubble plot-Property
ff1<-aggregate(fire$Fatality.Count, list(fire$Property.type),sum)
ff1$Group.1 <- sub("^$", "Unknown", ff1$Group.1)
ff1$Group.1 <- sub("N/A", "Unknown", ff1$Group.1)
ff1<- ff1[!(ff1$Group.1== "Unknown"),]
packing <- circleProgressiveLayout(ff1$x, sizetype='area')

# We can add these packing information to the initial data frame
data <- cbind(ff1, packing)
colnames(data)<- c("Group.1", "x1","x", "y", "radius")

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot
ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  scale_fill_manual(values = inferno(nrow(data))) +
  # Add text in the center of each bubble + control its size
  geom_text(data = data, aes(x, y, size=x1, label = Group.1)) +
  scale_size_continuous(range = c(1,4)) +
  
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()

#Line Plot Trend
#Creating a subset were 9/11 deaths are excluded
ff<-fire[!(fire$Incident.date== "2001-09-11"),]
dataYear <- ff %>% group_by(death.year)%>% tally()
plot(dataYear, type = "b")

##########################################
# Multivariate Plots
##########################################

#Question 1: What is the average age of death per classification?
#Average age per Classification
ff<-na.omit(fire)
ageRank<- aggregate(ff$Age, list(ff$Classification), mean)
ageRank <- arrange(ageRank, desc(x))
ageRank<- ageRank[!(ageRank$Group.1== " "),]
ggplot(ageRank, aes(Group.1, x)) +geom_bar(stat = "identity", fill="#dd3333") + theme_classic() + ylab("Age")

#           Group.1        x
#1        Volunteer 50.02302
#2     Paid-on-Call 47.12121
#3       Industrial 46.60000
#4 Part-Time (Paid) 45.57143
#5           Career 44.20280
#6         Wildland 37.01408

#Question 2: What is the average age of the fallen firefighter per year?
#Barplot Average age per Classification
ff$year<- as.factor(ff$year)
byyear<- aggregate(ff$Age, list(ff$death.year), mean)
ggplot(byyear, aes(Group.1, x)) +geom_bar(stat = "identity", fill="#dd3333") + theme_classic() + ylab("Age")

#   Group.1        x
#1     2000 46.56731
#2     2001 45.52679
#3     2002 41.38614
#4     2003 45.12174
#5     2004 47.96581
#6     2005 46.28182
#7     2006 45.09174
#8     2007 44.88983
#9     2008 44.15574
#10    2009 48.00000
#11    2010 50.44318
#12    2011 47.18605
#13    2012 47.61628
#14    2013 42.30909
#15    2014 52.51042
#16    2015 49.40909
#17    2016 47.86957
#18    2017 51.94444
#19    2018 48.31395

#violin plot Avg Age per Classification
fireclass<- fire
fireclass[(fireclass$classification== "Wildland Part-Time"),]<- fireclass$classification== "Wildland"
fireclass[(fireclass$classification== "Wildland Full-Time"),]<- fireclass$classification== "Wildland"
fireclass[(fireclass$classification== "Wildland Contract"),]<- fireclass$classification== "Wildland"
fireclass<- fireclass[!(fireclass$classification== " "),]
fire %>%
  mutate(class = fct_reorder(Classification, Age, .fun='median')) %>%
  ggplot( aes(x=reorder(Classification, Age), y=Age, fill=Classification)) + 
  geom_violin() +
  xlab("Classification") +
  theme(legend.position="none") +
  xlab("")

#Question 3: What is the mortality rate of Career and Volunteer firefighter per year?
#Deaths per year per classification

#Creating a subset were 9/11 deaths are excluded
multi<-fire[!(fire$Incident.date== "2001-09-11"),]
multi <- multi %>% group_by(death.year, Classification)%>% tally()
multi$death.year <- as.numeric(multi$death.year)
multi<- multi[!(multi$Classification == "Wildland"),]
multi<- multi[!(multi$Classification == "Industrial"),]
multi<- multi[!(multi$Classification == "Paid-on-Call"),]
multi<- multi[!(multi$Classification == "Part-Time (Paid)"),]

ggplot(multi, aes(death.year, fill = Classification)) + geom_bar(position='dodge') + ylab("Classification") +xlab ("Year") +theme_classic()+
  scale_fill_manual("legend", values = c("Career" = "#dd3333", "Volunteer" = "#ffc10e"))

ggplot(multi, aes(x=death.year, y=n, group=Classification)) +
  geom_line(aes(color=Classification, size=3))+
  geom_point(aes(color=Classification))+
  scale_color_manual(values=c("#dd3333", "#ffc10e"))

# Question 4: Which firefighter classification is at most risk and when (duty)?
#Heatmap
heat<- na.omit(fire) %>% group_by(Duty)%>% count(Classification)
heatSort <- arrange(heat, desc(n))
heatSort<- heatSort[!(heatSort$Classification == "Wildland"),]
heatSort<- heatSort[!(heatSort$Classification == "Industrial"),]
heatSort<- heatSort[!(heatSort$Classification == "Paid-on-Call"),]
heatSort<- heatSort[!(heatSort$Classification == "Part-Time (Paid)"),]
heatSort$Duty <- sub("^$", "Unknown", heatSort$Duty )
heatSort<- heatSort[!(heatSort$Duty == "Unknown"),]
ggplot(heatSort,aes(x = Classification, y = Duty, fill=n)) + geom_tile()+scale_fill_gradient(low="#ffc10e", high="#dd3333")

############################

#plot showing trends over the years
yearlydeaths<- aggregate(fire$Fatality.Count, list(fire$death.year),sum)
scatter.smooth(yearlydeaths)

##########################
#treemap
library(treemap)
##all years
library(d3treeR)
summary(fire$Duty)
dataNature <- fire %>% group_by(Cause.of.death)%>% count(Nature.of.death)
dataNatureSort <- arrange(dataNature, desc(n))
dataNatureSort$Nature.of.death <- sub("^$", "Unknown", dataNatureSort$Nature.of.death)
dataNatureSort$Cause.of.death <- sub("^$", "Unknown", dataNatureSort$Cause.of.death)
dataNatureSort<- dataNatureSort[!(dataNatureSort$Cause.of.death== "Unknown"),]
dataNatureSort<- dataNatureSort[!(dataNatureSort$Cause.of.death== "Other"),]
#dataNatureSort<- dataNatureSort[!(dataNatureSort$n<3),]
dataNatureSort<- dataNatureSort[!(dataNatureSort$Nature.of.death== "Unknown"),]
dataNatureSort<- dataNatureSort[!(dataNatureSort$Nature.of.death== "Other"),]
NatureCause<-treemap(dataNatureSort, index = c("Cause.of.death","Nature.of.death"), vSize = "n"
                     ,type = "index" ,palette="OrRd",range=c(-20000,60000),mapping=c(-20000,10000,60000)
                     ,fontsize.labels = c(15,12),fontcolor.labels="white"
                     , fontface.labels=c(8,7)
                     ,bg.labels=c("transparent")
                     ,align.labels = list(c("centre","centre"),c("left","top")), overlap.labels = 0, inflate.labels = F)

head(fire)
## Only 2018
dataNature1 <- fire %>% group_by(Cause.of.death,death.year)%>% count(Nature.of.death)
dataNatureSort1 <- arrange(dataNature1, desc(n))
dataNatureSort1$Nature.of.death <- sub("^$", "Unknown", dataNatureSort1$Nature.of.death)
dataNatureSort1$Cause.of.death <- sub("^$", "Unknown", dataNatureSort1$Cause.of.death)
dataNatureSort1<- dataNatureSort1[(dataNatureSort1$death.year== 2018),]
dataNatureSort1<- dataNatureSort1[!(dataNatureSort1$Cause.of.death== "Unknown"),]
dataNatureSort1<- dataNatureSort1[!(dataNatureSort1$Cause.of.death== "Other"),]
dataNatureSort1<- dataNatureSort1[!(dataNatureSort1$Nature.of.death== "Other"),]
dataNatureSort1<- dataNatureSort1[!(dataNatureSort1$Nature.of.death== "Unknown"),]
NatureCause1<-treemap(dataNatureSort1, index = c("Nature.of.death","Cause.of.death"), vSize = "n"
                      ,type = "index" ,palette="OrRd",range=c(-20000,60000),mapping=c(-20000,10000,60000)
                      ,fontsize.labels = c(15,12),fontcolor.labels="black"
                      , fontface.labels=c(8,7)
                      ,bg.labels=c("transparent")
                      ,align.labels = list(c("centre","centre"),c("left","top")), overlap.labels = 0, inflate.labels = F)



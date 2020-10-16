################################
#
#Author: Laura C. Larregui
#Purpose:Final Project
#8/13/2020
################################

# Look for csv file
fire<-read.csv("C:\\Users\\laura\\OneDrive\\Documents\\Syracuse iSchool\\IST 719\\IST-719\\Project\\Dataset\\ff_data.csv"
               , header = TRUE, sep = ",", stringsAsFactors = TRUE)

# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(paletteer)
library(RColorBrewer)
library(alluvial) #for alluvial plots
library(treemap)#for treemap

str(fire)
#2291 obs. of  16 variables
#(16*4)*(2291/100)
# = 1466.24

#Check for missing values
#(sum(is.na(fire))) #360 missing values
#fire<-na.omit(fire) # Just to make sure there aren't any missing values
#str(fire)# 1931 obs. of  16 variables

#Check for duplicates
#duplicated(fire)# One duplicated record
#fire<-fire[!duplicated(fire),]# Removed duplicated record
#str(fire) #1931 obs. of  16 variables

#Formatting data
fire <- fire %>% mutate(Fatality.Count = 1)
## Dates
fire$Incident.date <- as.Date(fire$Incident.date,"%b %d, %Y")
fire$Date.of.death <- as.Date(fire$Date.of.death,"%b %d, %Y")
#extract the year and convert to numeric format
fire$year <- as.numeric(format(fire$Incident.date, "%Y"))
#time convalencing
Days<-fire$Date.of.death  - fire$Incident.date
fire['Days']<-Days
#Only looking at 2000-2018
fire<- fire[(fire$year>1999),]


###########################
#Bar plot
fire.cause<-tapply(fire$Fatality.Count, list(fire$Cause.of.death, fire$Classification), length)
barplot(fire.cause,
        main = "Leading Causes of Death 2000-2018",
        xlab = "Number of Fatalities",
        ylab = "Cause of Death",
        col = "#dd3333",
        horiz = TRUE)

ggplot(data=fire, aes(x=Cause.of.death, y= Fatality.Count, fill=Cause.of.death)) +
  geom_bar(stat="identity")+
  scale_colour_gradient2()+
  coord_flip()+
  geom_text(aes(label = Cause.of.death), nudge_y = 2) + 
  scale_fill_viridis_d()


ggplot(fire,aes(reorder(Cause.of.death, -Fatality.Count),Fatality.Count )) + 
  geom_col(aes(fill = Fatality.Count)) + 
  coord_flip() + 
  labs(x = "Cause.of.death")


#Death by Cause
##Barplot
dataCause <- fire %>% group_by(Cause.of.death)%>% tally()
dataCauseSort <- arrange(dataCause, desc(n))[1:10,]
dataCauseSort$Cause.of.death <- sub("^$", "Unknown", dataCauseSort$Cause.of.death)
ggplot(dataCauseSort, aes(x=reorder(Cause.of.death,n), n)) + geom_bar(stat = "identity", fill="#dd3333") +coord_flip()+ labs(x = "Cause of Death", y = "Number of Fatalities", title = "Top 10 Causes of Death 2000-2018")+ geom_text(aes(label = scales::percent(n), y = n, group = Cause.of.death),position = position_dodge(width = 0.5), vjust = 1.5)

###########################
#alluvial plot

alluv.fire<- aggregate(fire$Fatality.Count, list(fire$Duty, fire$Classification, fire$Cause.of.death),sum)
colnames(alluv.fire)<- c("duty", "classification","cause of death", "fatality count")
alluv.fire<- alluv.fire[!(alluv.fire$classification== "Wildland Part-Time"),]
alluv.fire<- alluv.fire[!(alluv.fire$classification== "Wildland Full-Time"),]
alluv.fire<- alluv.fire[!(alluv.fire$classification== "Wildland Contract"),]
alluv.fire<- alluv.fire[!(alluv.fire$classification== "Paid on Call"),]
alluv.fire<- alluv.fire[!(alluv.fire$classification== "Industrial"),]
alluv.fire<- alluv.fire[!(alluv.fire$Nature.of.death== "Unknown"),]
alluv.fire<- alluv.fire[!(alluv.fire$Nature.of.death== "Other"),]
alluvial(alluv.fire[ , 2:3], freq = alluv.fire$`fatality count`)

##########################
#treemap
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
dataNature1 <- fire %>% group_by(Cause.of.death,year)%>% count(Nature.of.death)
dataNatureSort1 <- arrange(dataNature1, desc(n))
dataNatureSort1$Nature.of.death <- sub("^$", "Unknown", dataNatureSort1$Nature.of.death)
dataNatureSort1$Cause.of.death <- sub("^$", "Unknown", dataNatureSort1$Cause.of.death)
dataNatureSort1<- dataNatureSort1[(dataNatureSort1$year== 2018),]
dataNatureSort1<- dataNatureSort1[!(dataNatureSort1$Cause.of.death== "Unknown"),]
dataNatureSort1<- dataNatureSort1[!(dataNatureSort1$Cause.of.death== "Other"),]
dataNatureSort1<- dataNatureSort1[!(dataNatureSort1$Nature.of.death== "Other"),]
dataNatureSort1<- dataNatureSort1[!(dataNatureSort1$Nature.of.death== "Unknown"),]
NatureCause1<-treemap(dataNatureSort1, index = c("Cause.of.death","Nature.of.death"), vSize = "n"
                     ,type = "index" ,palette="OrRd",range=c(-20000,60000),mapping=c(-20000,10000,60000)
                     ,fontsize.labels = c(15,12),fontcolor.labels="black"
                     , fontface.labels=c(8,7)
                     ,bg.labels=c("transparent")
                     ,align.labels = list(c("centre","centre"),c("left","top")), overlap.labels = 0, inflate.labels = F)


dataNature2 <- fire %>% group_by(year)%>% count(Fatality.Count)
dataNatureSort2 <- arrange(dataNature2, desc(n))
NatureCause2<-treemap(dataNatureSort2, index = "year", vSize = "n"
                      ,type = "index" ,palette="OrRd",range=c(-20000,60000),mapping=c(-20000,10000,60000)
                      ,fontsize.labels = c(15,12),fontcolor.labels="black"
                      , fontface.labels=c(8,7)
                      ,bg.labels=c("transparent")
                      ,align.labels = list(c("centre","centre"),c("left","top")), overlap.labels = 0, inflate.labels = F)
##########################
# Deaths by year
fire %>% 
  group_by(year) %>%
  ggplot(aes(x = year, y = Fatality.Count)) + 
  geom_bar(stat = 'identity', fill = '#dd3333') 

##########################
# Training 
training<- na.omit(fire) %>% group_by(Duty, Activity, year)%>% count(Fatality.Count)
trainingSort <- arrange(training, desc(n))
trainingSort<- trainingSort[(trainingSort$Duty== "Training"),]
ggplot(trainingSort,aes(x = year, y = n)) + 
  geom_bar(stat = 'identity', fill = '#dd3333')

##########################
library(tidyverse)
library(tidytext, quietly = TRUE)
library(data.table)
library(tm)
library(wordcloud)

fire1<-fread("C:\\Users\\laura\\OneDrive\\Documents\\Syracuse iSchool\\IST 719\\IST-719\\Project\\Dataset\\ff_data.csv")
fire2 <- fire1 %>% unnest_tokens(word, 'Initial summary')               
fire3 <- fire2 %>% anti_join(stop_words, by = 'word')
fire3 %>% group_by(word) %>% tally %>% arrange(desc(n)) %>% head(20)
fire_np  <- fire3 %>% group_by(word) %>% tally %>% arrange(desc(n))
fire_np %>% 
  filter(n > 200) %>% 
  ggplot(aes(reorder(word, -n), n)) +
  geom_col(fill = 'steelblue') +
  ggtitle('Most frequent words') +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 30))
# Wordcloud
wordcloud(words = fire_np$word, freq = fire_np$n, max.words = 200, 
          random.order = FALSE, rot.per = 0.35, 
          colors=brewer.pal(9, "Oranges"))
#wordcloud 2
library(wordcloud2)
#https://www.r-graph-gallery.com/196-the-wordcloud2-library.html
colorVec = rep(brewer.pal(9, "YlOrRd"), length.out=nrow(fire_np))
wordcloud2(data=fire_np, size = 0.7, color = colorVec, shape = 'pentagon',backgroundColor="gray")
#########################
# Age and Rank
fire<-na.omit(fire)
fire$year<- as.factor(fire$year)
ageRank<- aggregate(fire$Age, list(fire$Classification), mean)

byyear<- aggregate(fire$Fatality.Count, list(fire$Classification,fire$year), length)
ageRank <- arrange(ageRank, desc(x))
ageRank<- ageRank[!(ageRank$Group.1== " "),]
ggplot(ageRank, aes(Group.1, x)) +geom_bar(stat = "identity", fill="#dd3333") + theme_classic() + ylab("Age")

############################
#remove.packages("ggplot2")
#install.packages('ggplot2', dep = TRUE)
library(ggplot2)
library(hrbrthemes)
library(ggvis)
heat<- na.omit(fire) %>% group_by(year)%>% count(Emergency)
heatSort <- arrange(heat, desc(n))
ggplot(heatSort,aes(x = year, y = Emergency, fill=n)) + geom_tile()
  
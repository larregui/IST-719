################################
#
#Author: Laura C. Larregui
#Purpose:Final Project
#8/13/2020
################################
# Look for csv file
fire<-read.csv("C:\\Users\\laura\\OneDrive\\Documents\\Syracuse iSchool\\IST 719\\IST-719\\Project\\Dataset\\ff_data.csv"
               , header = TRUE, sep = ",", stringsAsFactors = TRUE)

################################
# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(paletteer)
library(RColorBrewer)
library(alluvial) #for alluvial plots
library(treemap)#for treemap

################################
# Checking data
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

################################
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
#Only looking at 2000-2018
#fire<- fire[(fire$year>1999),]

###########################
#Death by Cause
##Barplot
dataCause <- fire %>% group_by(Cause.of.death)%>% tally()
dataCauseSort <- arrange(dataCause, desc(n))[1:10,]
dataCauseSort$Cause.of.death <- sub("^$", "Unknown", dataCauseSort$Cause.of.death)
ggplot(dataCauseSort, aes(x=reorder(Cause.of.death,n), n)) + geom_bar(stat = "identity", fill="#dd3333") +coord_flip()+ labs(x = "Cause of Death", y = "Number of Fatalities", title = "Top 10 Causes of Death 2000-2018")+ geom_text(aes(label = scales::percent(n), y = n, group = Cause.of.death),position = position_dodge(width = 0.5), vjust = 1.5)

###########################
#alluvial plot

alluv.fire<- aggregate(fire$Fatality.Count, list(fire$Duty, fire$Emergency, fire$Cause.of.death),sum)
colnames(alluv.fire)<- c("duty", "Emergency","cause of death", "fatality count")
alluv.fire<- alluv.fire[!(alluv.fire$classification== "Wildland Part-Time"),]
alluv.fire<- alluv.fire[!(alluv.fire$classification== "Wildland Full-Time"),]
alluv.fire<- alluv.fire[!(alluv.fire$classification== "Wildland Contract"),]
alluv.fire<- alluv.fire[!(alluv.fire$classification== "Paid on Call"),]
alluv.fire<- alluv.fire[!(alluv.fire$classification== "Industrial"),]
alluv.fire<- alluv.fire[!(alluv.fire$Nature.of.death== "Unknown"),]
alluv.fire<- alluv.fire[!(alluv.fire$Nature.of.death== "Other"),]
alluvial(alluv.fire[ , 1:2], freq = alluv.fire$`fatality count`)

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


##########################
# Deaths by year
fire %>% 
  group_by(death.year) %>%
  ggplot(aes(x = death.year, y = Fatality.Count)) + 
  geom_bar(stat = 'identity', fill = '#dd3333') 

##########################
# Training 
training<- na.omit(fire) %>% group_by(Duty, Activity, death.year)%>% count(Fatality.Count)
trainingSort <- arrange(training, desc(n))
trainingSort<- trainingSort[(trainingSort$Duty== "Training"),]
ggplot(trainingSort,aes(x = death.year, y = n)) + 
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
fire_np<- fire_np[!(fire_np$word== "fire"),]
fire_np<- fire_np[!(fire_np$word== "firefighter"),]
fire_np<- fire_np[!(fire_np$word== "firefighters"),]
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
ff<-na.omit(fire)
ff$year<- as.factor(ff$year)
ageRank<- aggregate(ff$Age, list(ff$Classification), mean)

byyear<- aggregate(ff$Fatality.Count, list(ff$Classification,ff$year), length)
ageRank <- arrange(ageRank, desc(x))
ageRank<- ageRank[!(ageRank$Group.1== " "),]
ggplot(ageRank, aes(Group.1, x)) +geom_bar(stat = "identity", fill="#dd3333") + theme_classic() + ylab("Age")

############################
#remove.packages("ggplot2")
#install.packages('ggplot2', dep = TRUE)
library(ggplot2)
library(hrbrthemes)
library(ggvis)
heat<- na.omit(fire) %>% group_by(Cause.of.death)%>% count(Property.type)
heatSort <- arrange(heat, desc(n))
ggplot(heatSort,aes(x = Cause.of.death, y = Property.type, fill=n)) + geom_tile()
#alluvial(heatSort[ , 1:2], freq = heatSort$n) 

multi <- fire %>% filter(Cause.of.death == "Stress/Overexertion") %>% group_by(year, Classification)
multi$year <- as.numeric(multi$year)
ggplot(multi, aes(year, fill = Classification)) + geom_bar() + ylab("Classification") +xlab ("Year") +theme_classic()

############################

#plot showing trends over the years
yearlydeaths<- aggregate(fire$Fatality.Count, list(fire$death.year),sum)
scatter.smooth(yearlydeaths)

############################
#average age in classification

fireclass<- fire
fireclass[(fireclass$classification== "Wildland Part-Time"),]<- fireclass$classification== "Wildland"
fireclass[(fireclass$classification== "Wildland Full-Time"),]<- fireclass$classification== "Wildland"
fireclass[(fireclass$classification== "Wildland Contract"),]<- fireclass$classification== "Wildland"
fireclass<- fireclass[!(fireclass$classification== " "),]
fireclass %>%
  mutate(class = fct_reorder(Classification, Age, .fun='median')) %>%
  ggplot( aes(x=reorder(Classification, Age), y=Age, fill=Classification)) + 
  geom_violin() +
  xlab("Classification") +
  theme(legend.position="none") +
  xlab("")
################################

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

yearlydeaths<- aggregate(fire$Fatality.Count, list(fire$death.year, fire$Classification),sum)
# Plot
yearlydeaths %>%
  ggplot(aes(x=Group.1, y=x, group=Group.2, color=Group.2)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  ylab("Number of Firefighter Fatalities")

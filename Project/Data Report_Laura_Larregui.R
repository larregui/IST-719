################################
#
#Author: Laura C. Larregui
#Purpose:Homework 3
#7/19/2020
################################

# Look for csv file
fema<-read.csv("C:\\Users\\laura\\OneDrive\\Documents\\Syracuse iSchool\\IST 719\\IST-719\\Project\\Dataset\\us_disaster_declarations.csv"
               , header = TRUE, sep = ",", stringsAsFactors = TRUE)

#libraries
library(ggplot2)
library(dplyr)
library(paletteer)
library(RColorBrewer)
str(fema)
#59181 obs. of  22 variables
#(22*4)*(59181/100)
# = 52079.28

#Check for missing values
(sum(is.na(fema))) #64861 missing values
fema<-na.omit(fema) # Just to make sure there aren't any missing values
str(fema)# 921 obs. of  22 variables

#Check for duplicates
duplicated(fema)# One duplicated record
fema<-fema[!duplicated(fema),]# Removed duplicated record
str(fema) #921 obs. of  22 variables

#Formatting data
## Dates
fema$declaration_date <- as.Date(fema$declaration_date,"%Y-%m-%d T%H:%M:%S Z")
fema$incident_begin_date <- as.Date(fema$incident_begin_date,"%Y-%m-%d T%H:%M:%S Z")
fema$incident_end_date <- as.Date(fema$incident_end_date,"%Y-%m-%d T%H:%M:%S Z")
fema$disaster_closeout_date <- as.Date(fema$disaster_closeout_date,"%Y-%m-%d T%H:%M:%S Z")

##Changing from numerical to nominal
fema$place_code<- factor(fema$place_code)
fema$hm_program_declared<-factor(fema$hm_program_declared)
fema$pa_program_declared<-factor(fema$pa_program_declared)
fema$ia_program_declared<-factor(fema$ia_program_declared)
fema$ih_program_declared<-factor(fema$ih_program_declared)
fema$declaration_request_number<-factor(fema$declaration_request_number)
fema$fy_declared<-factor(fema$fy_declared)
fema$disaster_number<-factor(fema$disaster_number)

##Dropping some columns
fema<-fema[,-20:-22]
head(fema)

# Check dataset structure
str(fema)

#Multidimensional plots
fema <- fema %>% mutate(Disaster.Count = 1)
#pr <- fema %>% filter(state == "PR") %>% group_by(declaration_date, fy_declared, incident_type) %>% summarise(Count = sum(Disaster.Count))
#pr$fy_declared <- as.numeric(pr$fy_declared)
#ggplot(pr, aes(fy_declared, fill = incident_type)) + geom_bar() + ylab("Days Deployed") +xlab ("Year") +theme_classic()+  ggtitle("FEMA Deployment in Puerto Rico", subtitle = "This barplot compares the number of deployments to the days deployed per year in Puerto Rico")

##################################################
#single dimension plots

#Organize states by regions
state <- c("AL",    "AK",   "AZ",   "AR",   "CA",   "CO",   "CT",   "DE",   "FL",   "GA",   "HI",   "ID",   "IL",   "IN",   "IA",   "KS",   "KY",   "LA",   "ME",   "MD",   "MA",   "MI",   "MN",   "MS",   "MO",   "MT",   "NE",   "NV",   "NH",   "NJ",   "NM",   "NY",   "NC",   "ND",   "OH",   "OK",   "OR",   "PA",   "RI",   "SC",   "SD",   "TN",   "TX",   "UT",   "VT",   "VA",   "WA",   "WV",   "WI",   "WY")
region <- c("South",    "West", "West", "South",    "West", "West", "Northeast",    "South",    "South",    "South",    "West", "West", "Midwest",  "Midwest",  "Midwest",  "Midwest",  "South",    "South",    "Northeast",    "South",    "Northeast",    "Midwest",  "Midwest",  "South",    "Midwest",  "West", "Midwest",  "West", "Northeast",    "Northeast",    "West", "Northeast",    "South",    "Midwest",  "Midwest",  "South",    "West", "Northeast",    "Northeast",    "South",    "Midwest",  "South",    "South",    "West", "Northeast",    "South",    "West", "South",    "Midwest",  "West")
subregion <- c("East South Central",    "Pacific",  "Mountain", "West South Central",   "Pacific",  "Mountain", "New England",  "South Atlantic",   "South Atlantic",   "South Atlantic",   "Pacific",  "Mountain", "East North Central",   "East North Central",   "West North Central",   "West North Central",   "East South Central",   "West South Central",   "New England",  "South Atlantic",   "New England",  "East North Central",   "West North Central",   "East South Central",   "West North Central",   "Mountain", "West North Central",   "Mountain", "New England",  "Middle Atlantic",  "Mountain", "Middle Atlantic",  "South Atlantic",   "West North Central",   "East North Central",   "West South Central",   "Pacific",  "Middle Atlantic",  "New England",  "South Atlantic",   "West North Central",   "East South Central",   "West South Central",   "Mountain", "New England",  "South Atlantic",   "Pacific",  "South Atlantic",   "East North Central",   "Mountain")
regional <- data.frame(state, region, subregion)
fema <- left_join(fema, regional, by = "state")

fema$region <- as.character(fema$region)
fema$subregion <- as.character(fema$subregion)
fema[c("region", "subregion")][is.na(fema[c("region", "subregion")])] <- "Territory"
fema$region <- as.factor(fema$region)
fema$subregion <- as.factor(fema$subregion)
###########################################################

layout(matrix(c(1,1), 2, 2, byrow = T), heights = c(3,1))

# barplot
region_inc<-tapply(fema$incident_type, list(fema$region), length)

barplot(region_inc, col=brewer.pal(8, "Set1"),
        main="Number of Incidents Per Region", xlab = "US Region")
# Add text
mtext("This barplot shows the distribition across regions")
title(sub="FEMA. (2020). US Natural Disaster Declarations (Version 3) [CSV file]. Retrieved from https://www.kaggle.com/headsortails/us-natural-disaster-declarations.", location="bottomleft")
###########################################################
# barplot
inc<-table(fema$incident_type)
inc1<- sort(inc, decreasing = TRUE)
inc1<-head(inc1)
barplot(inc1, col=brewer.pal(8, "Set1"),main="Number of Natural Disaster", xlab = "Natural Disasters")
# Add text
mtext("This barplot shows the distribition of the top 5 natural disasters")
title(sub="FEMA. (2020). US Natural Disaster Declarations (Version 3) [CSV file]. Retrieved from https://www.kaggle.com/headsortails/us-natural-disaster-declarations.", location="bottomleft")

###########################################################
layout(matrix(c(1,1), 2, 2, byrow = T), heights = c(3,1))

# Line Plot for Number of incidents occured between 1953-2020
fema_year <- fema %>% group_by(fy_declared) %>% summarise(Count = sum(Disaster.Count))
plot(fema_year, type="b",cex= 1.5, pch=8, col="red", main="Incidents declared by FEMA", xlab="Years")
# Add text
mtext("Line Plot for Number of incidents occured between 1953-2020")
title(sub="FEMA. (2020). US Natural Disaster Declarations (Version 3) [CSV file]. Retrieved from https://www.kaggle.com/headsortails/us-natural-disaster-declarations.", location="bottomleft")
#############################################################
state_inc<-tapply(fema$incident_type, list(fema$state), length)

barplot(state_inc, col="lightblue2",
        main="Number of Incidents Per State", xlab = "US States")

tab<-table(fema$state)
#barplot(tab, col="lightblue2", main="Number of Incidents Per State", xlab = "US States")

################################
#
#Author: Laura C. Larregui
#Purpose:Progress Report
#7/25/2020
################################

#Association Rule Mining
str(newfema)#test
newfema<- fema[,-20]
newfema<- newfema[,-16]
newfema<- newfema[,-3]
table(discretizeDF(newfema$incident_type))
newfema$incident_type<-discretizeDF(newfema$incident_type)
newfema$incident_type<-factor(newfema$incident_type)
suppVar <- 0.01
confVar <- 0.9
maxlenVar <- 3
rulesfemaRight <- apriori(newfema, parameter = list(supp = suppVar, conf = confVar, maxlen = maxlenVar), 
                           appearance = list (default = "lhs", rhs= "region=South"),control=list(verbose=F))
options(digits=2)
inspect(rulesfemaRight)

############################
#treemap
treemap(fema, index = c("region"), vSize = "Disaster.Count")
treemap(fema, index = c("subregion"), vSize = "Disaster.Count")
treemap(fema, index = c("state"), vSize = "Disaster.Count")
###########################
#alluvial plot

library(alluvial)
alluv.fema<- aggregate(fema$Disaster.Count, list(fema$region, fema$incident_type),sum)
colnames(alluv.fema)<- c("region", "incident type", "disaster count")
alluvial(alluv.fema[ , 1:2], freq = alluv.fema$`disaster count`)

#########################
#US map plot

library(usmap)
library(ggplot2)
alluv.fema1<- table(fema$state)
fema1<- data.frame(alluv.fema1)
colnames(fema1)<- c("state", "disaster count")
plot_usmap(data = fema1, values = "disaster count", color = "red") + 
  scale_fill_continuous(low = "white", high = "red", name = "Disaster Count (1953 - Present)", label = scales::comma) + theme(legend.position = "right")

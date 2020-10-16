## Authors: Sukhpal Dhillon, Rick Bump, Laura Larregui, Kevin Vogel
## Purpose: Vizathon Plots - IST 719

## Question 1: How does tempature effect the pH level and Algae content of the water?
## Question 2: How does tempature effect the Cloudiness and Saltiness of the water?

#############################################
## Part 1: Load Libraries
#############################################
library(readxl)
library(ggplot2)
library(dplyr)


#############################################
## Part 2: Load the Data and Preprocessing
#############################################
## Load the Data
#data <- read_excel('C:\\Users\\laura\\OneDrive\\Documents\\Syracuse iSchool\\IST 719\\IST-719\\vizathon\\BuoyData_2_2.xlsx', sheet = 2)
dir<- "C:\\Users\\laura\\OneDrive\\Documents\\Syracuse iSchool\\IST 719\\IST-719\\vizathon\\"
data<-read.csv(paste0(dir, "BuoyData.csv"), header=TRUE, stringsAsFactors = FALSE)

## Explore dataset
dim(data)
colnames(data)
str(data)
summary(data)


#############################################
## Part 3: Single-Dimension Descriptive Plots
#############################################

# pH Density Distribution Plot
plot1 <- ggplot(data) + aes(x=pH) + aes(y=..density..) + geom_density(alpha=.3, fill="blue", color="blue") +
         ggtitle("pH Density Distribution") + xlab("pH Level") +
         theme_light() + theme(plot.title = element_text(hjust = 0.5))
plot1

# Algae Distribution Plot
plot2 <- ggplot(data) + aes(x=Chl_ug_L) + aes(y=..density..) + geom_density(alpha=.3, fill="green", color="green") + 
         ggtitle("Algae Distribution") + xlab("Amount of Algae") +
         theme_light() + theme(plot.title = element_text(hjust = 0.5))
plot2

# Cloudiness Distribution Plot
plot3 <- ggplot(data) + aes(x=Tn_Ntu) + aes(y=..density..) + geom_density(alpha=.3, fill="gray", color="gray") +
         ggtitle("Particle Matter Distribution") + xlab("Number of Particles") +
         theme_light() + theme(plot.title = element_text(hjust = 0.5))
plot3

# Saltiness Distribution Plot
plot4 <- ggplot(data) + aes(x=SC_us_cm) + aes(y=..density..) + geom_density(alpha=.3, fill="red", color="red") +
         ggtitle("Ionic Content Distribution") + xlab("Amount of Ionic Content") +
         theme_light() + theme(plot.title = element_text(hjust = 0.5))
plot4


#############################################
## Part 4: Multi-Dimensional Descriptive Plot
#############################################

# Question 1 Plot
q1 <- data %>% group_by(T_DEGC, Chl_ug_L) %>% summarise(pH.mean = mean(pH))
plot5 <- ggplot(q1) + aes(x=T_DEGC, y=Chl_ug_L) + geom_jitter(aes(color=pH.mean), size = 0.5) +
         scale_colour_gradient(low="blue",high="green") +
         xlab("Temperature in Degrees C") + ylab("Amount of Algae") + 
         ggtitle("Variation in Algae and pH by Temperature") + 
         theme_light() + theme(plot.title = element_text(hjust = 0.5))
plot5


# Question 2 Plot
q2 <- data %>% group_by(T_DEGC, SC_us_cm) %>% summarise(cloudiness.mean = mean(Tn_Ntu))
plot6 <- ggplot(q2) + aes(x=T_DEGC, y=SC_us_cm) + geom_jitter(aes(color=cloudiness.mean), size = 0.5) +
         scale_colour_gradient(low="blue",high="gray") +
         xlab("Temperature in Degrees C") + ylab("Ionic Content") + 
         ggtitle("Variation in Ionic Content and Cloudiness by Temperature") + 
         theme_light() + theme(plot.title = element_text(hjust = 0.5))
plot6

# Alternative Approaches to Plots
plot5a <- ggplot(data) + aes(x=T_DEGC, y=Chl_ug_L) + geom_jitter(aes(color=pH), size = 0.5) +
          scale_colour_gradient(low="blue",high="green") +
          xlab("Temperature in Degrees C") + ylab("Amount of Algae") + 
          ggtitle("Variation in Algae and pH by Temperature") + 
          theme_light() + theme(plot.title = element_text(hjust = 0.5))
plot5a

plot6b <- ggplot(data) + aes(x=T_DEGC, y=SC_us_cm) + geom_jitter(aes(color=Tn_Ntu), size = 0.5) +
          scale_colour_gradient(low="blue",high="gray") +
          xlab("Temperature in Degrees C") + ylab("Ionic Content") + 
          ggtitle("Variation in Ionic Content and Cloudiness by Temperature") + 
          theme_light() + theme(plot.title = element_text(hjust = 0.5))
plot6b

## End Vizathon Code
# Lab2-Larregui.R
# Author: Laura C. Larregui
# Purpose: Lab 2, Data Interrogation or Data Exploration 
# 		and Distribution
#Second Week (7/12/2020)
#

# File.choose actually brings up a dialog box when you run a file.
# And it just allows you to pick a file. What file.choose actually returns is 
# what's called a fully-qualified path to the file.

fname<- file.choose() # to find the path to a data set

tips <- read.csv(file = fname
                 , header = TRUE
                 , stringsAsFactors = FALSE)
# 8 columns and 244 rows
colnames (tips) # names of the columns

# fix gives me a spreadsheet view of the data set, 
# and it actually lets me change the data if I needed to.

fix(tips)

#View lets you view the spreadsheet w/o changing it.
View(tips)

# str is a function that R gives us that allows us to look at the structure of the data.
str(tips)

# Indexes
# tips is not a single vector, it is a spreadsheet (it has rows and columns).

tips[1, ] # first row of data
tips[ ,1] # first column of data

tips[3,3]
tips [1:3, ] # looks at the first 3 rows

# length returns a vector
length(tips[1:3, 2])

# dim is for the number of dimensions in the data set, and it'll tell you the number of rows.
# dim returns a vector of two values.
dim(tips) 
dim(tips) [1]

tips$time # look at all the values in that column
tips[ , "time"]

plot(tips$total_bill) # most of the observations look like they fall around 15
plot(sort(tips$total_bill))
boxplot(tips$total_bill)
hist(tips$total_bill)
d<- density(tips$total_bill)
plot(d)
polygon (d, col = "orange")

plot(tips$tip) # most of the observations look like they fall between 2 and 4
plot(sort(tips$tip))
boxplot(tips$tip)
hist(tips$tip)
d1<- density(tips$tip)
plot(d1)
polygon (d1, col = "orange")

# 4 different ways to show distribution

par(mfrow = c(2,2))
boxplot(tips$total_bill)
hist(tips$total_bill)
d<- density(tips$total_bill)
plot(d)
polygon (d, col = "orange")

#violin plot
library(vioplot)
vioplot(tips$total_bill)
# the violin plot shows data mirrored on both sides. 
#It tells us that there is a central tendency probably around 15, 
#and that it skews a little bit upwards so that the values towards 
#the top tend to be less frequent than the values on the bottom. 
#You can also see that embedded in the violin plot is something that looks an awful lot like a box plot.


# Subsetting

tips$sex
unique(tips$sex)
# we only have 2 values

tips.M<-tips[tips$sex == "Male" , ]
View(tips.M)

tips.F<-tips[tips$sex == "Female" , ]
View(tips.F)

par(mfrow = c(2,1), mar = c(2,3,1,2))
boxplot(tips.F$tip, horizontal = TRUE, ylab = "F", ylim= c(1,10))
boxplot(tips.M$tip, horizontal = TRUE, ylab = "M", ylim= c(1,10))
# males tend to tip more than females

fname<- "C:\\Users\\laura\\OneDrive\\Documents\\Syracuse iSchool\\IST 719\\IST-719\\Labs\\Lab 2\\tweet.formated.json"
library(jsonlite)
raw.tweet<- fromJSON(fname, flatten = FALSE)
str(raw.tweet)
View(raw.tweet)
names(raw.tweet)
raw.tweet$text
raw.tweet$user$followers_count

raw.tweet[["user"]]
raw.tweet[["user"]]$followers_count
raw.tweet[["user"]][["followers_count"]]

fname<- "C:\\Users\\laura\\OneDrive\\Documents\\Syracuse iSchool\\IST 719\\IST-719\\Labs\\Lab 2\\tweets5814.json"
con<- file(fname, open = "r")
tweets <- stream_in(con)
close(con)
dim(tweets)

tweets$text[1:3]                    
tweets$user$followers_count
boxplot(log10(tweets$user$followers_count), horizontal =TRUE) 
#highly skewed data

# grouping dat atogether

task.time<- c(rnorm(n =30, mean = 30, sd=2.25)
              ,rnorm(n =30, mean = 25, sd=1.5))
hist(task.time)
 status<- c(rep("AMA",30), rep("PRO", 30))
df<- data.frame(time = task.time, status = status) 

#avg value of all pro and ama
df.grouped <- aggregate(df$time, list(df$status), mean)
df.grouped

#So what actually happened here is aggregate said, take all the values
#for the time variable and separate it into two groups, based on what's in status. 
#And what's in status is AMATEUR and PRO.

View(df)

colnames(df.grouped)<- c("stat", "time")
barplot(df.grouped$time, names.arg = df.grouped$stat)

# tapply

M.grouped<-tapply(df$time, list(df$status), mean)
class(M.grouped)
M.grouped
#tapply = vector or matrix 
#aggregate = dataframe

tapply(df$time, list(df$status), range)
range(task.time)

# range shows min and max

summary(task.time)

aggregate(df$time, list(df$status), summary)

#table just counts doesn't group
table(df$status)
table(df$time) #useless
table(round(df$time, 2)) #not helpful

df$sex <- sample(c("M", "F"), 60, replace = T)
aggregate(df$time, list (df$status, df$sex), mean)
M<- tapply(df$time, list (df$status, df$sex), mean)
M
barplot(M)
barplot(M, beside = TRUE)

################################
#
# reshaping data with tidyr
#
################################

# gather() makes 'wide' data longer
# spread() makes 'long' data wider
# seperate() splits a single column into multiple columns
# unite() combines multiple columns into a single column

library(tidyr)

n<- 5
year <- 2001:(2000 + n)
q1 <- runif(n = n, min=100, max = 120)
q2 <- runif(n = n, min=103, max = 130)
q3 <- runif(n = n, min=105, max = 140)
q4 <- runif(n = n, min=108, max = 150)

df.wide<- data.frame(year, q1, q2, q3, q4)
df.wide
gather(df.wide, qt, sales, q1:q4)

df.wide %>% gather(qt, sales, q1:q4)
#%>% takes this thing on the left and passes on the right

df.long<-df.wide %>% gather(qt, sales, q1:q4)

o<- order(df.long$year, df.long$qt)
df.long<- df.long[o,]
df.long

df <- data.frame(cat = rep(c("tap", "reg", "zed", "vum"),3)
                 , group = rep(letters[7:9],4)
                 , x =1:12)
df
spread(df, cat, x)
#more like a matrix format


###########################
#
# using rect function to build a custom plot
#
###########################
library(plotrix)
n<-70
age.min<- 1
age.max<- 90
age.range<- c(age.min, age.max)
m <- round(rescale(rbeta(n, 5, 2.5), age.range),0)
hist(m)
f <- round(rescale(rbeta(n, 5, 2.0), age.range),0)
x<- age.min:age.max
f.y<- m.y<- rep(0, length(x))

m.tab<- table(m)
m.y[as.numeric((names(m.tab)))] <- as.numeric(m.tab)

f.tab<- table(f)
f.y[as.numeric((names(f.tab)))] <- as.numeric(f.tab)

age.freqs<- data.frame(ages = x, males = m.y, females = f.y)
max.x <- round(1.2 * max(age.freqs[ , 2:3]), 0)
plot(c(-max.x, max.x), c(0,100), type = "n", bty ="n"
     , xaxt = "n", ylab ="age", xlab = "freq", main = "sample age distribution") 
# only creates plot space
# xaxt gets rid off the x axis

grid()
last.y<- 0
for ( i in 1:90) {
  
  i <- 1
  rect(xleft = 0, ybottom = last.y, xright = -age.freqs$males[i]
       , ytop = age.freqs$ages[i], col = "lightblue2", border = NA)
  
  rect(xleft =0, ybottom = last.y, xright = age.freqs$females [i]
       , ytop =age.freqs$ages[i], col= "lightpink", border = NA)
  
  last.y <- age.freqs$ages[i]
  
  
}


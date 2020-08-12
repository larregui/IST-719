#
# Author Laura Larregui
# Purpose: Lab 1 - First Week (7/5/2020)
#


pie(c(7,4,2,12))
#pie charts are not super useful
x<-c(7,6,7.2,12)
pie(x)
pie(x, main="Jeff's Pie", col=c("red", "orange", "yellow"))

pie(x
    , main="Jeff's Pie"
    , col = c("red", "orange", "tan", "yellow")
    , labels = c("a", "b", "c", "d")
)
plot(c(1,3,6,4),pch = 8)
plot(c(1,3,6,4),pch = 16,col = c("red", "orange", "tan", "yellow"))
plot(c(1,3,6,4),pch = 16,col = c("red", "orange", "tan", "yellow"),cex =3)

# Crl + S to save
#variables
x <- "jeff"
x
# R doesn't care for specifying datatypes for variables

x<- rnorm(n=10)
plot(x)
plot(x, type="l")# line plot
plot(x, type="p")# dot plot
plot(x, type="h")# like a histogram

plot(x, type="h", lwd = 5)# make the lines thicker

plot(x, type="h", lwd = 5, lend = 2, col = "orange")

plot(x, type="h", lwd = 5, lend = 2, col = "orange"
, main = "change in net worth"
, xlab = "time in years"
, ylab = "in millions"
,bty ="n")

par(bg="gray")
plot(x, type="h", lwd = 20, col = c("blue","orange")
     , bty ="n", lend = 2)

my.par<- par()
par(my.par)

n<- 27
my.letters<- sample(letters[1:3], size = n, replace = TRUE)
my.letters
my.letters[2]
letters[7:9]
letters[c(8,3,1)]
table(my.letters)
tab<- table(my.letters)
barplot(tab)
barplot(tab, col="brown")
barplot(tab, col=c("brown", "tan", "orange"))
barplot(tab, col=c("brown", "tan", "orange")
        , names.arg= c("sales", "ops", "it")
        , border ="white"
        , xlab = "departments"
        , ylab = "employees"
        , main = "company employees"
        , horiz = TRUE
        , las = 1
        , density = 20
        , angle = c(45, 90, 12))

x <- rnorm(n = 1000 , mean= 10, sd = 1)
hist(x , main = "what is the distribution of x")
boxplot ( x, horizontal=TRUE)
# Ch. 4 p. 195

x<- rlnorm(n = 1000, meanlog = 1, sdlog = 1 )
x
par(mfrow = c (2,1))
boxplot(x, horizontal =TRUE)
hist(x)
par(mfrow = c (1,1))

hist(log10(x))
hist(x)

#quiz#1
my.letters <- sample(letters[7:9], size = 50, replace =T)
barplot(table(my.letters))

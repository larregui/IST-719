#
# Author Laura Larregui
# Purpose: Breakout Activity - First Week (7/6/2020)
#
my.letters.1 <- sample(letters[1:4], size = 28, replace =T)
my.letters.table <- table(my.letters.1)
my.letters.table

#barplot(my.letters.table) 
barplot(my.letters.table
        , xlab = "Letters"
        , ylab = "Frecuency"
        , main = " Letter VS Frecuency"
        , horiz = TRUE
        , col = c("orange", " blue", "yellow", "red")
        , border= c("black")
        , density = 14
        , angle = c(45, 90, 180, 20)) 

my.bucket <- rnorm(n = 20)
#plot(my.bucket)
plot(my.bucket
     , col = "orange"
     , main = "my bucket plot"
     , xlab = "frecuency"
     , ylab = "number"
     , bty ="n"
     , pch = 16
     , cex.main = 2
     , cex = 1.5
     , bg = "blue"
     , type = "b"
     #, xlim = c(0,21)
     #, ylim c(-2,2)
     )

par(mfrow = c(2,1))
    
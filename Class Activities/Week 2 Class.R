#
# Author: Laura Larregui
# Week 2 In-Class Activity
#
# Using viz to answer business questions
# Data Set: Wine Sales

# Question : 

fname<- file.choose()
sales <- read.csv(file = fname
                  , header = TRUE
                  , stringsAsFactors = FALSE)

View(sales) # table look
str(sales)
colnames(sales)

sales$expenses[1:10]
sales$income[1:10]
#continous variables

plot(sales$expenses, sales$income
     , col = "orange"
     , main="Relationship between Expenses and Income"
     , xlab = "expenses"
     , ylab = "income")
abline(h=400, col="blue")
abline(v=9, col="blue")
rug( x= sales$income, side = 2, col ="orange")
rug( x= sales$income, side = 1, col ="orange")

boxplot(sales$units.sold~sales$type
        , xlab = "Wine Type")

###########################################
# Breakout Group

agg<-tapply(sales$units.sold, list(sales$type, sales$rep.region), sum)

barplot(agg,col=c("red","white"),beside = T,legend.text = rownames((agg)),
        main="Comparison of Wine Sales")
agg1<-tapply(sales$units.sold, list(sales$rep.region, sales$type), sum)

barplot(agg1,col=c("red","orange", "yellow", "green", "blue"),beside = T,
        main="units by region", legend.text = rownames((agg1)),args.legend=list(x="topright",bty="s"))

#########################
#
# Author: Laura C. Larregui
#Purpose: Week 6 Lab: ggplot
#
#########################

sale<-file.choose()
sales<- read.csv(sale, header=TRUE, stringsAsFactors = FALSE)

library(ggplot2)
p<- ggplot(sales)
class(p)
attributes(p)
p$data
p$layers
p$scales
summary(p)
View(p)

ggplot(sales) + aes(x=expenses)
range(sales$expenses)
plot(sales$expenses)

ggplot(sales, aes(x=expenses))

ggplot(sales) + aes(x=expenses, y=income) + geom_point()

p<- ggplot(sales) + aes(x=expenses, y=income) + geom_point()

p + geom_point(color = "blue")

save(p, file =)

ggplot(sales) + aes(x=expenses, y=income, color= type) + geom_point()

ggplot(sales) + aes(x=expenses, y=income, color = unit.price>14) + geom_point()

ggplot(sales) + aes(x=expenses, y=income) + geom_point(color = ifelse(sales$unit.price> 14, "red", "blue"))

ggplot(sales) + aes(x=expenses, y=income, color=unit.price) + geom_point()

ggplot(sales) + aes(x=expenses, y=income, color=unit.price, shape= type) + geom_point()

ggplot(sales) + aes(x=expenses, y=income, color=unit.price, shape= type, alpha =income) + geom_point()

ggplot(sales) + aes(x=expenses, y=income, color=rep.region, shape= type, alpha = unit.price, size =units.sold) + geom_point()

p1<- ggplot(sales)
p2<- ggplot(sales) + aes(x=expenses, y=income, shape= rep.region)
summary(p1)
attributes(p1)
p1$labels
p1$mapping
summary(p2)
attributes(p2)
p2$labels
p2$mapping

##############################
#geoms

ggplot(sales) + aes(x=expenses, y=income) + geom_point()+ geom_rug()

income.pred<- predict(lm(sales$income~sales$expenses))
ggplot(sales) + aes(x=expenses, y=income) + geom_point()+
  aes(y=income.pred) + geom_line(color = "red", lwd = 3) + geom_rug

ggplot(sales) + aes(x=expenses, y=income) + geom_point(color ="pink")+ geom_rug()+
  geom_line(aes(y= income.pred)) + geom_line(aes(y=income.pred+150))+
  geom_vline(xintercept = 10, color ="blue")+
  geom_hline(yintercept=500 , color = "orange")+
  geom_abline(intercept = 50, slope = 100, color ="red", lty = 3, lwd = 2)

ggplot(sales) + aes(x=expenses, y=income) + geom_point()+ geom_smooth()


ggplot(sales) + aes(x=expenses, y=income) + geom_bin2d()
ggplot(sales) + aes(x=expenses, y=income) + geom_bin2d(bins=50)

price<- ifelse(sales$unit.price>14, "expensive", "moderate")
price[sales$unit.price<9]<- "cheap"

ggplot(sales)+ aes(y=income, x= expenses, color =price)+ geom_bin2d(bins=50)

###############333
# other geoms

df<- aggregate(sales$units.sold, list (year=sales$year), sum)
df2<- aggregate(sales$units.sold, list (year=sales$year, region=sales$rep.region), sum)

g<- ggplot(sales)+ aes(x=income)
g + geom_blank()
g+ geom_histogram()
g+ geom_histogram(binwidth = 10)
g+ geom_histogram(binwidth = 10, fill="orange")+
  geom_vline(aes(xintercept = mean(income)), color= "blue", linetype = "dashed", size=1)
g+ geom_histogram(binwidth = 10, fill="orange", alpha =.9) +
  aes(y=..density..)+
  geom_density(alpha=.3, fill ="blue", color="blue")

## boxplot

ggplot(sales) +aes(x= "laura", y= income) +geom_boxplot()
ggplot(sales) +aes(x= rep.region, y= income) +geom_boxplot()
ggplot(df) + aes(x=year, y=x)+ geom_line()+ ylim(c(0,40000))
ggplot(df) + aes(x=year, y=x)+ geom_step()+ ylim(c(0,40000))
ggplot(df) + aes(x=year, y=x)+ geom_ribbon(aes(ymin = df$x-1000, ymax=df$x +1000, fill="blue"))+geom_line()+ylim(c(0,40000))

ggplot(df2) + aes(x=year, y=x, color=region)+ geom_line()+ ylim(c(0,10000))

ggplot(df) + aes(x=year, y=x)+ geom_tile()

ggplot(df) + aes(x=year, y=x)+ geom_violin()

#######################
# geom bar

df3<- aggregate(sales$units.sold, list(region=sales$rep.region), sum)
colnames(df3)[2]<- "sales"

ggplot(sales)+ aes(x=rep.region)+ geom_bar(fill = "orange", width= .5)+
  ggtitle("number of sales by region")

ggplot(sales)+ aes(x=rep.region, fill=type)+ geom_bar(position="fill")
ggplot(df3)+aes(x=region, y=sales, fill=region)+
  geom_bar(stat = "identity")

ggplot(df3)+aes(x="", y=sales, fill=region)+ geom_bar(width=1, stat = "identity")+
  coord_polar("y", start=45)

hist(sales$income)
p<- ggplot(sales)+ aes(x=income)
p+geom_histogram()+ stat_bin(binwidth = 20)


p + stat_density()

ggplot(sales)+ aes(y=income) + geom_boxplot()+ stat_boxplot()
ggplot(sales)+ aes(y=income) + stat_boxplot()
ggplot(sales)+ aes(x=expenses, y=income) + stat_bin2d()+ stat_density_2d(col="red")

ggplot(sales)+aes(x=rep.region)+geom_bar()
ggplot(sales)+aes(x=rep.region)+stat_count()

ggplot(df3) +aes(x=region, y =sales)+ geom_bar(stat= "identity")
ggplot(df3) +aes(x=region, y =sales)+ stat_identity()

ggplot(sales)+ aes(x=income)+
  geom_histogram(aes(fill= ..count..))+
  aes(y= ..density..)+
  geom_density(fill="yellow", alpha=.1)

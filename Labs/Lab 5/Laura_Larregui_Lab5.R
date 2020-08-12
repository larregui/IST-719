##########################
#
# Author: Laura Larregui
#Purpose: Week 5 Lab: Working with Twitter Data
# Uses: ClimateTweets_UseForLecture_25K.xls
#
##########################
tweet<- file.choose()
tweets<- read.csv(tweet , header = TRUE,stringsAsFactors = FALSE ,quote = "\"")
                    View(tweets)
                    
                    my.media<- tweets$media
                    my.media
                    table(my.media)
                    my.media[my.media == ""]<- "text only"
                    my.media<- gsub("\\|photo", "", my.media) 
                    # gsub says find all the things that match what's in here and then replace it
                    # | means or
                    100* round(table(my.media)/sum(table(my.media)), 4) #percents
                    #8:35 timestamp
                    pie(100* round(table(my.media)/sum(table(my.media)), 4)) # save pie chart 7.09 * 4.67
                    
                    ###########
                    #looking at dates
                    
                    tweets$created_at[1:3]
                    # "Wed Jul 06 03:35:37 +0000 2016"
                    
                    conversion.string<- "%a %b %d %H:%M:%S +0000 %Y"
                    
                    tmp<- strptime(tweets$created_at[1:3],conversion.string)
                    tmp
                    class(tmp) #POSIX comes from UNIX
                    
                    is.na(tmp) #check for missing values
                    any(is.na(tmp))
                    tmp<-na.omit(tmp)
                    
                    ##################### 
                    rm(tmp)
                    tweets$date<- strptime(tweets$created_at,conversion.string)
                    
                    tmp<-"10AM and 27 minutes, on June 22, 1999"
                    #to convert it to a real data object
                    strptime(tmp, "%H%p and %M minutes, on %B %d, %Y")
                    
                    min(tweets$date)
                    max(tweets$date)
                    range(tweets$date)
                    summary(tweets$date)
                    
                    
                    difftime(min(tweets$date), max(tweets$date))
                    difftime(min(tweets$date), max(tweets$date), units ="min")
                    difftime(min(tweets$date), max(tweets$date), units ="weeks")
                    
                    install.packages("lubridate")
                    library(lubridate)
                    
                    wday(tweets$date[1:3], label= TRUE, abbr = TRUE)
                    
                    barplot(table(wday(tweets$date, label= TRUE, abbr = TRUE)))
                    # converted 25000 date objs into day of week factor
                    
                    tmp<- tweets$user_utc_offset
                    tweets$date[7:10] + tmp #converts the time
                    
                    know.times<- tweets$date + tweets$user_utc_offset
                    know.times
                    index<- which(is.na(know.times))
                    know.times<-know.times[-index]
                    barplot(table(hour(know.times))) #13:42
                    
                    start.date<- as.POSIXct("2016-06-24 23:59:19")
                    end.date<- as.POSIXct("2016-06-26 00:00:00")
                    
                    index<- which((tweets$date > start.date) & (tweets$date < end.date))
                    tweets.25th<- tweets$date[index]
                    
                    format.Date(tweets.25th, "%Y%m%d%H%M") #returns strings not dates
                    
                    tmp.date<- as.POSIXct(strptime(format.Date(tweets.25th, "%Y%m%d%H%M"), "%Y%m%d%H%M"))
                                          
                                          plot(table(tmp.date))
                                          #simple plot 
                                          length (plot(table(tmp.date))) #324 blindspot
                                          24*60 #1440
                                          tmp.tab<- table(tmp.date)
                                          any(is.na(tmp.tab))
                                          plot(as.POSIXct(names(tmp.tab)), as.numeric(tmp.tab), type ="h")
                                          #a table is like a vector
                                          #creat an x value where there is a 0 where there is no value
                                          x<- seq.POSIXt(from=start.date + 1, to = end.date - 1, by ="min")
                                          any(is.na(x))
                                          length(x)
                                          y<-rep(0, length(x))
                                          y[match(names(tmp.tab), as.character(x))]<- as.numeric(tmp.tab)
                                          y
                                          plot(x,,y,type = "p", pch = 16, cex = .4)
                                          plot(x,,y,type = "p", pch = ".", cex = .4)
                                          plot(x,,y,type = "l")
                                          
                                          #############################
                                          # hashtag world cloud
                                          
tweets$text[5:10]
library(stringr)                                
tags<- str_extract_all(tweets$text, "#\\S+", simplify = FALSE)                                          
tags<- tags[lengths(tags)>0]                                          
tags<- unlist(tags)
tags <- tolower(tags)
tags<- gsub("#|[[:punct:]]", "", tags)
tag.tab<- sort(table(tags), decreasing = TRUE)
tag.tab[1:10]
zap<- which(tag.tab<3)
tag.tab<-tag.tab[-zap]
boxplot(as.numeric(tag.tab))
plot(as.numeric(tag.tab))
df<- data.frame(words=names(tag.tab), count=as.numeric(tag.tab), stringsAsFactors = FALSE)
par(mfrow=c(3,3))
plot(df$count, main="raw")
y<- df$count/max(df$count)
plot(y, main="0-1")
plot(df$count^2, main="^2")
plot(df$count^(1/2), main="^(1/2)")
plot(log10(df$count), main="log10")
plot(log(df$count), main="nlog")
#save plots
library(wordcloud)
myPal<- colorRampPalette(c("gold", "red","orange3"))
# clears r mem gc()
df

index<- which(df$count>9)
par(mar=c(0,0,0,0), bg="white")
my.counts<- (df$count[index]^(1/2))
wordcloud(df$words[index], my.counts, scale = c(5, .5), min.freq = 1, max.words = Inf , random.order = FALSE, random.color = FALSE, ordered.colors = TRUE, rot.per=0, myPal(length(df$words[index])))

##############3
#Alluvual plot & treemap plots

sale<- file.choose()
sales<- read.csv(sale, header = TRUE, stringsAsFactors = FALSE)
library(alluvial)
dat<- as.data.frame(Titanic, stringsAsFactors = FALSE)
alluvial(dat[,1:4], freq= dat$Freq, bg="white")
alluv.df<- aggregate(sales$units.sold, list(sales$rep.region, sales$type),sum)
colnames(alluv.df)<- c("reg", "type", "units.sold")
alluvial(alluv.df[ , 1:2], freq = alluv.df$units.sold)
my.cols<- rep("gold", nrow(alluv.df))
my.cols[alluv.df$type == "red"]<- "red"
alluvial(alluv.df[ , 1:2] , freq = alluv.df$units.sold, col=my.cols)
alluvial(alluv.df[ , 1:2 ], freq = alluv.df$units.sold, col=ifelse(alluv.df$type=="red","red","gold"))
options(stringsAsFactors = FALSE)

alluv.df<- aggregate(sales$units.sold, list(sales$rep.region, sales$type, sales$wine),sum)
colnames(alluv.df)<- c("reg", "type", "wine", "units.sold")
alluvial(alluv.df[ , 1:3 ], freq = alluv.df$units.sold, col=ifelse(alluv.df$type=="red","red","gold"))

library(RColorBrewer)
library(treemap)

treemap(sales, index = c("rep.region"), vSize = "income", palette = "Greens", fontsize.labels = 18 )
treemap(sales, index = c("rep.region"), vSize = "income", vColor = "units.sold", type= "dens",palette = "Greens", fontsize.labels = 18 )
treemap(sales, index = c("rep.region"), vSize = "income", vColor = "units.sold", type= "value",palette = "OrRd", fontsize.labels = 18 )
treemap(sales, index = c("rep.region", "sales.rep", "type"), vSize = "income", vColor = "units.sold", type= "index",palette = brewer.pal(8, "Set1"))

# river plot

library (riverplot)
river<- riverplot.example()
par(mfrow=c(2,1))
plot(river, srt =90, lty =1)
class(river)
x<- river
x$edges
x$nodes
x$edges$Value[2]<- 45
x$edges$Value[1]<- 15
x$nodes$x[5]<-5
plot(x)

df<- aggregate(sales$income, list(type=sales$type, wine =sales$wine),sum)
df<- df[order(df$type, df$x),]

node.name<- c("wine", unique(df$type), df$wine)
node.position<- c(1,2,2,3,3,3,3,3,3,3)
node.color<- rep("gray", length(node.name))
node.color<- c("deepskyblue", "red", "yellow", "brown4", "firebrick3", "deeppink4", "khaki1", "lightgoldenrod1", "gold", "goldenrod1")
node<- data.frame(ID = node.name, x= node.position, col= node.color, stringsAsFactors = FALSE)
parent.nodes<- c("wine", "wine", df$type)
child.nodes<- c("red", "white", df$wine)
value <- c(sum(df$x[df$type == "red"]), sum(df$x[df$type == "white"]), df$x)
edges<- data.frame(N1 = parent.nodes, N2=child.nodes, Value=value)
r<- makeRiver(node, edges)
par(mar=c(0,0,0,0))
plot(r)

########################

# R plots and word

dat<- tapply(sales$units.sold, list(sales$type, sales$rep.region),sum)
barplot(dat, beside=TRUE, col=c("brown", "gold"), main = "Units Sold by region by type")

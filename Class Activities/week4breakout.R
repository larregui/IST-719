



# Load in the dataset
art <- read.csv("C:\\Users\\zuloo\\Desktop\\Syracuse Data Science\\Semester 4\\IST 719\\Datasets\\art.csv", header=T, stringsAsFactors=F)

colors <- rep("blue", nrow(art))
colors[art$paper=="watercolor"] = "red"

plot(art$units.sold ~ art$unit.price, col=colors, pch=16, ylim=c(0, 30),
     legend.text = c("drawing", "watercolor"), args.legend=list(x="topright",bty="s"))


library(RColorBrewer)

# 5 layout dashboard
M <- matrix(
  c(1,1,1,3,
    1,1,1,4,
    2,2,2,5),
  nrow=3, byrow=T
)
layout(M)
layout.show(5)

par(mar=c(2,4,2,4))

dat.1 <- tapply(art$total.sale, list(art$paper.type), sum)
dat.2 <- tapply(art$units.sold, list(art$paper.type), sum)
dat.3 <- tapply(art$units.sold, list(art$paper.type), mean)
dat.4 <- tapply(art$unit.price, list(art$paper.type), mean)
dat.5<- tapply(art$total.sale, list(art$paper,art$year),sum)

colramp_blue = colorRampPalette(c("white", "blue"))
colramp_orange = colorRampPalette(c("white", "orange"))

barplot(dat.1, col=colramp_blue(6), xaxt="n", ylab="Total sales", main="Total sales and units sold by paper type")
barplot(dat.2, col=colramp_orange(6), ylab="Units sold")
pie(dat.3, main="Average units sold by paper type")
pie(dat.4, main="Average unit price by paper type")
barplot(dat.5, beside=TRUE, col=c("orange", "cyan"), main="Annual total sales by paper")



# 3 x 2 dashboard
par(mfrow=c(3,2))

barplot(dat.1, col=colramp_blue(6), xaxt="n", ylab="Total sales", main="Total sales and units sold by paper type")
barplot(dat.2, col=colramp_orange(6), ylab="Units sold")
pie(dat.3, main="Average units sold by paper type")
pie(dat.4, main="Average unit price by paper type")
barplot(dat.5, beside=TRUE, col=c("orange", "cyan"), main="Annual total sales by paper")

dat.6<- tapply(art$units.sold, list(art$rep), sum)
barplot(dat.6, las=1, horiz = TRUE, col="orange", main = "Number of Orders by sales reps")






                           

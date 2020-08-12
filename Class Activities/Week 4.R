##################################
#
# Author: Laura Larregui
# Purpose: Week 4 Sync Class
#
##################################
# https://www.r-graph-gallery.com/74-margin-and-oma-cheatsheet

my.par<- par()
my.par$mar #margin

fname<- file.choose() # to find the path to a data set

art <- read.csv(file = fname
                , header = TRUE
                , stringsAsFactors = FALSE)
str(art)

#par(mar=c(5.1, 4.1, 4.1, 2.1)) #default

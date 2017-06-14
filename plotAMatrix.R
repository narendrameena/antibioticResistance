#author narumeena
#description ploting a matrix 

#install.packages("arm")

library(ggplot2)
library(plyr)
library(arm)
library(reshape2)

#data

data <- read.csv("data/minWithAllData.csv")


rownames(data) <- data$X



data$X <- NULL

data[is.na(data)] <- 0

sortedrows <- as.data.frame(head(sort(rowSums(data[2:865]),decreasing=TRUE), n = 3770))

head(sortedrows)
head(data[2:865])
filteredData <- data[row.names(sortedrows),]
filteredData[is.na(filteredData)] <- 0


#matrix ploting 
nba <- filteredData
head(Data)
#nba$X <- with(nba, reorder(X, PTS))
nba.m <- melt(filteredData)

head(nba.m)

jpeg('/Users/naru/Documents/Cambridge/Antibodies/example_forcedraw.jpg', width=20000, height=10000, unit='px')
nba.m <- ddply(nba.m, .(variable), transform,rescale = value)

(p <- ggplot(nba.m, aes(variable, X)) + geom_tile(aes(fill = rescale),
    colour = "white") + scale_fill_gradient(low = "white",
      high = "steelblue")+geom_text(aes(label=round(rescale,1))))


dev.off()


nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
dist_m <- as.matrix(dist(nba[1:20, -1]))
dist_mi <- 1/dist_m # one over, as qgraph takes similarity matrices as input
library(qgraph)
jpeg('example_forcedraw.jpg', width=1000, height=1000, unit='px')
qgraph(dist_mi, layout='spring', vsize=3)
dev.off()



nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")

nba$Name <- with(nba, reorder(Name, PTS))
nba.m <- melt(nba)
nba.m <- ddply(nba.m, .(variable), transform,
               rescale = rescale(value))
(p <- ggplot(nba.m, aes(variable, Name)) + geom_tile(aes(fill = rescale),
        colour = "white") + scale_fill_gradient(low = "white",
       high = "steelblue")+geom_text(aes(label=round(rescale,1))))


dev.off()


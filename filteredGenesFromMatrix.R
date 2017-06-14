#@author narumeena
#@script slecting only genes that have maximum fold change in all samples 


data <- read.csv("data/medianWithAllData.csv")


rownames(data) <- data$X



data$X <- NULL

data[is.na(data)] <- 0


sortedrows <- as.data.frame(head(sort(rowSums(data),decreasing=TRUE), n = 4000))







#sortedrows <- as.data.frame(sort(rowSums(data),decreasing=TRUE))

filteredData <- data[row.names(sortedrows),]
filteredData[is.na(filteredData)] <- 0

hypothetical <- grep("Rv", row.names(sortedrows), value=TRUE, fixed=FALSE) # for Rv 


#hypothetical <- grep("EC", row.names(sortedrows), value=TRUE, fixed=FALSE) # for EC 


nonhypothetical <- filteredData[! rownames(filteredData) %in% hypothetical,] 

sortedrows <- as.data.frame(sort(rowSums(nonhypothetical),decreasing=TRUE))



filteredData <- data[row.names(sortedrows),]
filteredData[is.na(filteredData)] <- 0

hypothetical <- grep("EC", row.names(sortedrows), value=TRUE, fixed=FALSE) # for EC 

nonhypothetical <- filteredData[! rownames(filteredData) %in% hypothetical,] 

#### for SA 



sortedrows <- as.data.frame(sort(rowSums(nonhypothetical),decreasing=TRUE))



filteredData <- data[row.names(sortedrows),]
filteredData[is.na(filteredData)] <- 0

hypothetical <- grep("SA", row.names(sortedrows), value=TRUE, fixed=FALSE) # for EC 

nonhypothetical <- filteredData[! rownames(filteredData) %in% hypothetical,] 


### SP 



sortedrows <- as.data.frame(sort(rowSums(nonhypothetical),decreasing=TRUE))



filteredData <- data[row.names(sortedrows),]
filteredData[is.na(filteredData)] <- 0

hypothetical <- grep("SP", row.names(sortedrows), value=TRUE, fixed=FALSE) # for EC 

nonhypothetical <- filteredData[! rownames(filteredData) %in% hypothetical,] 


#### Cj



sortedrows <- as.data.frame(sort(rowSums(nonhypothetical),decreasing=TRUE))



filteredData <- data[row.names(sortedrows),]
filteredData[is.na(filteredData)] <- 0

hypothetical <- grep("Cj", row.names(sortedrows), value=TRUE, fixed=FALSE) # for EC 

nonhypothetical <- filteredData[! rownames(filteredData) %in% hypothetical,] 


### MW 

sortedrows <- as.data.frame(sort(rowSums(nonhypothetical),decreasing=TRUE))



filteredData <- data[row.names(sortedrows),]
filteredData[is.na(filteredData)] <- 0

hypothetical <- grep("MW", row.names(sortedrows), value=TRUE, fixed=FALSE) # for EC 

nonhypothetical <- filteredData[! rownames(filteredData) %in% hypothetical,] 


row.names(nonhypothetical)


### name with sspace

sortedrows <- as.data.frame(sort(rowSums(nonhypothetical),decreasing=TRUE))



filteredData <- data[row.names(sortedrows),]
filteredData[is.na(filteredData)] <- 0

hypothetical <- grep(" ", row.names(sortedrows), value=TRUE, fixed=FALSE) # for EC 

nonhypothetical <- filteredData[! rownames(filteredData) %in% hypothetical,] 


row.names(nonhypothetical)


### rv

sortedrows <- as.data.frame(sort(rowSums(nonhypothetical),decreasing=TRUE))



filteredData <- data[row.names(sortedrows),]
filteredData[is.na(filteredData)] <- 0

hypothetical <- grep("rv", row.names(sortedrows), value=TRUE, fixed=FALSE) # for EC 

nonhypothetical <- filteredData[! rownames(filteredData) %in% hypothetical,] 


row.names(nonhypothetical)


#Matric Plot 

library(ggplot2)
library(plyr)
library(arm)
library(reshape2)

nba <-nonhypothetical 
head(nba)
nba$Name <- rownames(nonhypothetical )
nba.m <- melt(nba)

head(nba.m)
jpeg('MeadianDataMatrix.jpg', width=30000, height=50000, unit='px')


nba.m <- ddply(nba.m, .(variable), transform,
               rescale = value)#rescale = rescale(value))
(p <- ggplot(nba.m, aes(variable, Name)) + geom_tile(aes(fill = rescale),
                                                     colour = "white") + scale_fill_gradient(low = "white",
                                                                                             high = "steelblue")+geom_text(aes(label=round(rescale,1))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))



dev.off()

# 
# 
# 
# 
# #myImagePlot(filteredData)
# 
# jpeg('/Users/naru/Documents/Cambridge/Antibodies/MAXNonhypothetical.jpg', width=20000, height=10000, unit='px')
# myImagePlot(nonhypothetical)
# yLabels <- rownames(nonhypothetical)
# xLabels <- colnames(nonhypothetical)
# axis(1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
# axis(2, at=1:length(yLabels), labels=yLabels,
#      cex.axis=0.7)
# dev.off()
# 
# 
# rownames(nonhypothetical)
# 
# #myImagePlot(data)
# 
# 
# 
# ###clustring 
# 
#  mydata <- nonhypothetical
# # Determine number of clusters
# wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
# for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
#                                      centers=i)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")
# 
# 
# 
# 
# fit <- kmeans(mydata, 3,iter.max = 100000) # 4 cluster solution
# # Cluster Plot against 1st 2 principal components
# 
# # vary parameters for most readable graph
# library(cluster) 
# clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
#          labels=2, lines=0)
# 
# # Centroid Plot against 1st 2 discriminant functions
# library(fpc)
# plotcluster(mydata, fit$cluster)
# 
# ##clustromgram 
# clustergram(as.matrix(mydata), k.range = 2:10, line.width = 0.004)
# 
# 
# 
# 
# ##clustromgram 
# 
# 
# source("https://raw.github.com/talgalili/R-code-snippets/master/clustergram.r")
# 
# 
# clustergram(Data, k.range = 2:15, line.width = 0.004) # notice how I am using line.width.  Play with it on your problem, according to the scale of Y.
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # A prettier display of the volcano
# x <- 10*(1:nrow(volcano))
# y <- 10*(1:ncol(volcano))
# image(x, y, volcano, col = terrain.colors(100), axes = FALSE)
# contour(x, y, volcano, levels = seq(90, 200, by = 5),
#         add = TRUE, col = "peru")
# axis(1, at = seq(100, 800, by = 100))
# axis(2, at = seq(100, 600, by = 100))
# box()
# title(main = "Maunga Whau Volcano", font.main = 4)

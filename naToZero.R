#@author narumeena
#@description antibitic nalaysis on bactria 
#@descriptionScript replacing NA by zero 


data <- read.csv("data/maxWithAllData.csv")

rownames(data) <- data$X

data$X <- NULL

data[is.na(data)] <- 0

mydata <- data
source("https://raw.github.com/talgalili/R-code-snippets/master/clustergram.r")



set.seed(250)
par(cex.lab = 1.5, cex.main = 1.2)
Data1 <- scale(mydata) # notice I am scaling the vectors)

clustergram(Data1, k.range = 2:8, line.width = 0.004) 
# notice how I am using line.width.  Play with it on your problem, according to the scale of Y.

# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

####with 4 cluster 
#fit <- kmeans(data, 4, nstart=25,iter.max = 10000)
# Cluster Plot against 1st 2 principal component


# K-Means Cluster Analysis
fit <- kmeans(mydata, 4,iter.max = 10000) # 4 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)







# K-Means Clustering with 3 clusters

fit <- kmeans(mydata, 3,iter.max = 10000) # 4 cluster solution
# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)


# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster)


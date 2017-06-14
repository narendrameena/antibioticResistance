library(calibrate)
library(plyr)
library(gclus)
library(scatterplot3d)
library(cluster)
library(fpc)
library(mclust)
library(rpanel)
library(rgl)
library(lattice)
library(tm);
library(RColorBrewer) 

install.pacakages()

#Read data
mydata <- read.table(file="c:/data.mtx", header=TRUE, row.names=1, sep="");

# Lets look at the correlations
mydata.cor = abs(cor(scale(mydata)))
mydata.cor[,1:2]

#lets look at the data in interactive 3D plot before PCA
rp.plot3d(mydata[,1],mydata[,2], mydata[,3])

# Doing the PCA 
mydata.pca<- prcomp(mydata, retx=TRUE, center=TRUE, scale=TRUE);
summary(mydata.pca)
#3D plot of first three PCs
rp.plot3d(mydata.pca$x[,1],mydata.pca$x[,2],mydata.pca$x[,3])


#Eigenvalues of components for Kaiser Criterion
mydata.pca$sdev ^2


#scree test for determining optimal number of PCs (Elbow rule)
par(mfrow=c(1,2))
screeplot(mydata.pca,main="Scree Plot",xlab="Components")
screeplot(mydata.pca,type="line",main="Scree Plot")

#Scores
scores = mydata.pca$x
##  Plot of the scores, with the axes
pdf("scores.pdf")
plot (scores[,1], scores[,2], xlab="Scores 1", ylab="Scores 2")
text (x=scores[,1], y=scores[,2], labels=row.names (scores), cex=c(0.4,0.4), col = "blue")
lines(c(-5,5),c(0,0),lty=2)  ##  Draw the horizontal axis
lines(c(0,0),c(-4,3),lty=2)  ##  Draw the vertical axis
dev.off() 

#finding possible number of clusters in Kmeans
wss <- (nrow(scale(mydata))-1)*sum(apply(scale(mydata),2,var)); 
for (i in 2:20) wss[i] <- sum(kmeans(scale(mydata),centers=i)$withinss);
plot(1:20, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares");

#Performing K-Means and visualizing the result
km1<-kmeans(scores[,1:2], algorithm = "Hartigan-Wong", centers=4)   
#par(mfrow = c(1, 1))
pdf("km.pdf")
plot(scores[,1:2], col = km1$cluster);
points(km1$centers, col = 1:5, pch = 8, cex=2);
scatterplot3d(km1$centers, pch=20, highlight.3d = TRUE, type="h");
# getting cluster means 
aggregate(scores[,1:2],by=list(km1$cluster),FUN=mean);
# appending cluster assignment
clustercounts <- data.frame(scores[,1:2], km1$cluster);
#Cluster Plot against 1st 2 principal components
clusplot(scores[,1:2], km1$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, cex=c(0.2,0.2));
dev.off()
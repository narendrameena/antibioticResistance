#narumeena 

#description removing rows that have more thatan 10% NAs 

```{r set-options, echo=FALSE, cache=FALSE}
options(width = SOME-REALLY-BIG-VALUE)
```

data <- read.csv("data/maxDataWithAllCondition.csv")
dim(data)
data$X
row.names(data) <- data$X

data$X <- NULL
dim(data)


#counting NAs and ploting it 
hist(colSums(is.na(data)))

df$na_count <- apply(data, 1, function(x) sum(is.na(x)))

#install.packages("plotrix")
library(plotrix)
color2D.matplot(data[1:500,1:500],cs1=c(0,1),cs2=c(0,1),cs3=c(0,1), extremes=NA,cellcolors=NA,show.legend=TRUE,nslices=10,xlab="Column", ylab="Row",do.hex=FALSE,axes=TRUE,show.values=FALSE,vcol=NA,vcex=1, border="black",na.color=NA,xrange=NULL,color.spec="rgb",yrev=TRUE, xat=NULL,yat=NULL,Hinton=FALSE)

head(data)
dim(data)
row.has.na <- apply(data, 1, function(x){any(is.na(x))})

sum(row.has.na)

delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}


data[order(rowSums(is.na(data))), ]
filterData <- delete.na(data,50)

dim(filterData)


write.csv(filterData, file= "data/exampleDataSetForCalculation.csv") 

##clustring with NAs 

d <- data.frame(x=runif(100), cluster=NA)
d$x[sample(100, 10)] <- NA
clus <- kmeans(na.omit(data), 5)

data$cluster[which(!is.na(data))] <- clus$cluster

plot(data, bg=d$cluster, pch=21)


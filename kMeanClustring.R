#@author narumeena 
#@description antibiotic treatment on bactrial strains
#@descriptionScript kmean clustring analysis 

maxData <- read.csv("data/maxWithAllData.csv")
maxData$X
head(maxData)

maxData.stand <- scale(maxData[-1]) 


# K-Means
k.means.fit <- kmeans(na.omit(maxData.stand), 3) # k = 3
na.omit(x)
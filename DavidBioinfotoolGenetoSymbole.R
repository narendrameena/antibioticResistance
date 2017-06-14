#@author narumeena
#@Converting ARo gene accetion to gene symboles 



library(xlsx)


CardGenes <- read.xlsx("Notes/AntibioticProject.xlsx", 14)

head(CardGenes)




# gene list from expriment 

data <- read.csv("data/maxWithAllData.csv")


rownames(data) <- data$X



data$X <- NULL


data[is.na(data)] <- 0


cardFilterdData <-  data[toupper(rownames(data)) %in% toupper(CardGenes$ARO.Name),]

cardFilterdData$X <- rownames(cardFilterdData) 





 newdata <- aggregate(cardFilterdData[-865], by=list(toupper(cardFilterdData$X) ), sum)
 #t(sapply(unique(toupper(rownames(cardFilterdData))), function(x) cardFilterdData[toupper(rownames(cardFilterdData))==x,]))
 
 newdata$Group.1
 
 
head(cardFilterdData[-865])
head(newData)

#Matric Plot 

library(ggplot2)
library(plyr)
library(arm)
library(reshape2)

nba <-cardFilterdData
head(nba)
nba$Name <- rownames(cardFilterdData )
nba.m <- melt(nba)

head(nba.m)
jpeg('MeadianDataMatrix.jpg', width=30000, height=2000, unit='px')


nba.m <- ddply(nba.m, .(variable), transform,
               rescale = value)#rescale = rescale(value))
(p <- ggplot(nba.m, aes(variable, Name)) + geom_tile(aes(fill = rescale),
                                                     colour = "white") + scale_fill_gradient(low = "white",
                                                                                             high = "steelblue")+geom_text(aes(label=round(rescale,1))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))



dev.off()


count(match(toupper(rownames(data)),toupper(CardGenes$ARO.Name)))

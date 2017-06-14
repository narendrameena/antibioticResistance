#@author narumeena
#@Converting CARo gene accetion to gene symboles 



library(xlsx)


CardGenes <- read.xlsx("Notes/AntibioticProject.xlsx", 14)

head(CardGenes)




# gene list from expriment 

data <- read.csv("data/medianWithAllData.csv")


rownames(data) <- data$X



data$X <- NULL


data[is.na(data)] <- 0


cardFilterdData <-  data[toupper(rownames(data)) %in% toupper(CardGenes$ARO.Name),]

cardFilterdData$X <- rownames(cardFilterdData) 





 newdata <- aggregate(cardFilterdData[-865], by=list(toupper(cardFilterdData$X) ), sum)
 #t(sapply(unique(toupper(rownames(cardFilterdData))), function(x) cardFilterdData[toupper(rownames(cardFilterdData))==x,]))
 
 newdata$X <- newdata$Group.1
 
 newdata$Group.1 <- NULL
rownames(newdata) <- newdata$X


write.csv(rownames(newdata), file="slectedGenes.csv")
newdata$X <- NULL

#Matric Plot 

library(ggplot2)
library(plyr)
library(arm)
library(reshape2)

nba <-newdata
head(nba)
nba$Name <- rownames(newdata )
nba.m <- melt(nba)

head(nba.m)
jpeg('MeadianCardDataMatrix.jpg', width=30000, height=2000, unit='px')


nba.m <- ddply(nba.m, .(variable), transform,
               rescale = value)#rescale = rescale(value))
(p <- ggplot(nba.m, aes(variable, Name)) + geom_tile(aes(fill = rescale),
                                                     colour = "white") + scale_fill_gradient(low = "white",
                                                                                             high = "steelblue")+geom_text(aes(label=round(rescale,1))) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))



dev.off()





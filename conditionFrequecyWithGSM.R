#author narumeena
#description antibiotic analysis on E .Coli 
#condition frequecy by each sample 


data <- read.csv("data/ConditionWithGSM.csv")
head(data)
data$condition

library(plyr)

freqData <- count(data$condition)

freqData$gsm <- NA

head(freqData)

gsmList <- list()

toString( data[data$condition ==freqData$x , ]$gsm)

for (i in freqData$x){
  
  gsmList <-  append(gsmList, toString(data[data$condition == i , ]$gsm))

}

freqData$gsm <- unlist(gsmList)


#resultant Data 

write.csv(freqData, file ="data/ConditionsFrequenciesWithGSMIds.csv")


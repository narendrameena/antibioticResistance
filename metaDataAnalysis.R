#@author narumeena
#@Metadata analysis 

library(xlsx)


metaData <- read.xlsx("Notes/AntibioticProject.xlsx", 15)

exprassionData <- read.csv("data/maxWithAllData.csv")

colnames(exprassionData)
exprassionData$X
head(metaData)



cardFilterdData <-  data[toupper(rownames(data)) %in% toupper(CardGenes$ARO.Name),]

metaData$treatment[7:10]
metaData$time[7:10]
metaData$gsm[7:10]
metaData$strain[7:10] 


agregateData <- aggregate(gsm ~ treatment + time + strain, data = metaData, paste, collapse = ",")



write.csv(agregateData , "dataGroupSamples.csv") 
head(agregateData)


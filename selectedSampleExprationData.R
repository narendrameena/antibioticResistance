#@author narumeena 
#@description antibitic tretment of bactria analysis 
#descriptionScript getting selected columns 


library(xlsx)


selectedSamples <- read.xlsx("Notes/AntibioticProject.xlsx", 13)


head(selectedSamples)
selectedSamples$gsm
count(selectedSamples$strain)
count(selectedSamples$treatment)
count(selectedSamples$time)


library(plyr)
counts <- ddply(selectedSamples, .(selectedSamples$strain,selectedSamples$treatment,selectedSamples$time), nrow)
names(counts) <- c("selectedSamples$strain","selectedSamples$treatment","selectedSamples$time", "Freq")

count(selectedSamples$strain,selectedSamples$treatment,selectedSamples$time)

paste0(selectedSamples$gsm, "_ID_REF",collapse = ',')
idRef <- unlist(strsplit(paste0(selectedSamples$gsm, "_ID_REF",collapse = ','), ","))
ids <-  unlist(strsplit(idRef, "_"))[c(TRUE,FALSE,FALSE)]

columnNames <-  unlist(list(idRef,ids ))

data <- read.csv("data/normlizedExprationValueWithAnnotation.csv")


intersect(colnames(data),columnNames)

selectData <-  data[,intersect(colnames(data),columnNames)]


write.csv(selectData, file="data/slectedSampleDataWithAnnotation.csv")

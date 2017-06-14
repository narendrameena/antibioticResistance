#author narumeena 

#description combining multiple rows wiht common treatment and time 

data <- read.csv("data/dataGroupSamplesWithTreatmentAndTime.csv", header = TRUE)

dim(data)
head(data)
#install.packages("magrittr")
#library(magrittr)
#library(dplyr)

#data[is.na(data$gsm)] <- 0
data2 <- data %>% group_by(treatment, time) %>% summarise(val=paste(gsm, collapse=","))
dim(data2)
write.csv(data2, file="data/groupGSMonTheBasisofTreameant.csv")

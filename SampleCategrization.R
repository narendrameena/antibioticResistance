#@author narumeena 
#@description antibitic tretment of bactria analysis 
#@descriptionScript getting alll GSM with defined catagrization 

library(xlsx)


data <- read.xlsx("Notes/AntibioticProject.xlsx", 7)

head(data)


df <- data.frame(gsm <- data[2], treatment= data[7], time =data[4] , temp =data[5] ,strain = data[6])
head(df)
colnames(df) <- c("gsm","treatment", "time", "temp", "strain")

head(df)
df <- df[!is.na(df$strain),]
library(plyr)
count(df, vars = c("treatment", "time", "temp", "strain"))

write.csv(df, "data/selectedSamples.csv")

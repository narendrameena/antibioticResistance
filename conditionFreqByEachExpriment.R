#author narumeena
#description antibiotic analsysi on E .Coli 
#descriptionScript condition by each expriment or coondition frequecy 

rm(list=ls())
library(xlsx)
data <- read.xlsx("data/Transcriptomics.xlsx",sheetIndex = 1)
head(data)
data$Experimental.Condition
data$Accession #list of expriment by GEO accession number 
data$Experimental.Condition <- gsub("\\[|\\]", "",  data$Experimental.Condition)

#conditions<- data.frame(unlist(strsplit(as.character(data$Experimental.Condition),',')),data$Accession)
conditions<- data.frame(expriment="1",series_id ="1")
for (i in 1:length(data$Experimental.Condition)){
  print(i)
  
  conditions<- rbind.all.columns(conditions,data.frame(expriment=unlist(strsplit(as.character(data$Experimental.Condition),',')[i]),series_id=data$Accession[i]))
}



conditions[!duplicated(conditions), ]

library(dplyr)
conditions <- conditions %>% distinct

head(conditions)

#pmatch(exprimentFreq$condition,conditions1)

#write.csv(conditions, file= "data/transcriptionExprimentsAntibioticswithGSE.csv")

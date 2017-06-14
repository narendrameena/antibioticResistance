#@author narumeena
#@description antibiotic resistance on E. Coli
#@descriptionScript extrating index for each expriment and assining values expriment GEO id.

library(xlsx)
transcript <- read.xlsx("data/Transcriptomics.xlsx",sheetIndex = 1, header=TRUE)
head(transcript)
antibioticsList <- read.csv("transcriptionExprimentsAntibiotics.csv",header=TRUE)
transcript$Experimental.Condition
transcript$Experimental.Condition<- gsub("\\[|\\]", "",  transcript$Experimental.Condition)

agrep(transcript$Experimental.Condition,antibioticsList,value = T)
match(unlist(transcript$Experimental.Condition),unlist(antibioticsList))

transcript$Experimental.Condition<- strsplit(as.character(transcript$Experimental.Condition),',')

for (i in 1:length(transcript$Experimental.Condition)){
print(match( transcript$Experimental.Condition,antibioticsList$condition[i]))
  
}


grep( transcript$Experimental.Condition,"molecule")

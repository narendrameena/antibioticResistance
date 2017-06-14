#@author narumeena
#@description antibiotic resistance on E.Coli 
#@descriptionScript gettin typr of microarray 

library(xlsx)
library(GEOquery)
data <- read.xlsx("data/Transcriptomics.xlsx",sheetIndex = 1)
head(data)

getGEO(data$Accession[1])
getGEO("GPL3154")

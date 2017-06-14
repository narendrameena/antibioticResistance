#author narendra 
#description Analysis of antibiotic resistance on E.Coli
#descroptionScript annoteting tags of microarray with gensids 



rm(list=ls())
library(xlsx)
data <- read.xlsx("Notes/AntibioticProject.xlsx",sheetIndex = 7)

data$gsm

head(data)

library(GEOquery)
library(plyr)
gplMetaData  <- data.frame()

for(i in data$gsm){
  
  tryCatch({
    if(i %in% c('GSM28230')) stop(e)
    print(i)
    
    gplId <- Meta(getGEO(i))$platform_id
    
    columnName <- colnames(Table(getGEO(gplId))) 
    
    gplColumns <- as.data.frame(t(as.data.frame(c(i, gplId,  columnName ))))
    rownames(gplColumns) <- gplColumns[1,1]
    
    gplMetaData <- rbind.fill(gplMetaData,gplColumns)
  }
  ,error = function(e) print(paste(i,'is error')))
  #   #new.cbind(head(expTable,7),head(expTable1,10))
  
}

write.csv(gplMetaData, file = "GPlMetadataList.csv")










Table(getGEO("GPL9459"))[,"ORF"]
head(Table(getGEO("GPL9459")))

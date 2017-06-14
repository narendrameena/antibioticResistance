#author narumeena 
#description antibitic resistance on E.Coli 
#descriptionScript anottating microarray prob ids 






rm(list=ls())
library(xlsx)
data <- read.xlsx("Notes/AntibioticProject.xlsx",sheetIndex = 10)

data$gpl

head(data[1])

library(GEOquery)
library(plyr)
gplAnotData  <- data.frame()

for(i in 1:nrow(data)){
  
  tryCatch({
    if(i %in% c('GSM28230')) stop(e)
    print(i)

    anot <- Table(getGEO(data[i,1]))[data[i,2],data[i,3]] 
    rownames(anot) <- c(paste0(data[i,1],"_",data[i,2]),paste0(data[i,1],"_Anot"))
    
    
    gplAnotData <- rbind.fill(gplAnotData, anot )
  }
  ,error = function(e) print(paste(i,'is error')))
  #   #new.cbind(head(expTable,7),head(expTable1,10))
  
}
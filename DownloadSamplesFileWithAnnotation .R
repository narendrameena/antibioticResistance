#@author narumeena 
#@description antibiotic nalysis obn E.Coli 
#@descriptionScript #####download table for each sample and devlop a single csv file for all samples with 





#install.packages("qpcR")
#
library(xlsx)

GSEtoGSMmappedData <- read.xlsx(file = "Notes/AntibioticProject.xlsx", sheetIndex = 7)

head(GSEtoGSMmappedData)
library(GEOquery)
#GSEtoGSMmappedData <- read.csv("data/GSEtoGSMmappedData.csv")
GSEtoGSMmappedData$gsm[1:5]

colnames(Table(getGEO(Meta(getGEO(GSEtoGSMmappedData$gsm[5]))$platform_id)))
GSM1055178


##geting All GPL list 
library(data.table)

GplMetadata <- data.frame()

for(i in GSEtoGSMmappedData[1:50,]$gsm){
  
  tryCatch({
    if(i %in% c('GSM77341')) stop(e)
    print(i)
  
  gplId <- Meta(getGEO(i))$platform_id
  print(gplId)
  print(i)
  coleName <- colnames(Table(getGEO(gplId)))
  print(coleName)
  metaDataOfGPL <- t(as.data.frame(c(gplId, i,coleName)))
  rownames(metaDataOfGPL) <- metaDataOfGPL[1,2]
  

  
  GplMetadata<- new.cbind(GplMetadata, as.data.frame(metaDataOfGPL))
  
  }
  ,error = function(e) print(paste(i,'is error')))
  #new.cbind(head(expTable,7),head(expTable1,10))
  
}

head(  GplMetadata)

meataData <- read.csv("data/MetadataofEachSample.csv")
GSMtoGPL <- meataData[c('gsm','gpl')]

GSMtoGPL[GSMtoGPL$gsm =="GSM993805",]$gpl

uniportAnnotation <- read.csv("data/uniprotIdsForannotation.csv")
head(uniportAnnotation)
rownames(uniportAnnotation) <- uniportAnnotation$Ordered_locus_name
uniportAnnotation['b0002',]$synonyms
GSEtoGSMmappedData  <- merge(GSEtoGSMmappedData, GSMtoGPL, by =c("gsm") ) 


##downloading all samples data 
expresionDataofAllSamples <- data.frame()

for (i in GSEtoGSMmappedData$gsm){
  tryCatch({
    if(i %in% c('GSM77341')) stop(e)
    print(i)
    exeData <-  getGEO(i)
    
    expTable <-  Table(exeData)
    #row.names(expTable) <- expTable$ID_REF ### ids as rowname  head(expTable)
    names(expTable)[names(expTable) == "VALUE"] <- i
    #expTable <- expTable[i]   #head(expTable)
    #gplData <- Table(getGEO(GSMtoGPL[GSMtoGPL$gsm ==i,]$gpl))[c("ID", "ORF")]
    #rownames(gplData) <- gplData$ID
    
   # head(expTable[c(paste0("GSM153320", "_ID_REF"),"GSM153320")])
    #expTable$ID_REF <-  gplData[expTable$ID_REF,]$ORF  # gpl ids to ORF ids from gpl data
    
    
    #expTable$ID_REF <- uniportAnnotation[expTable$ID_REF,]$synonyms  # locus with synonyems head(expresionDataofAllSamples)
    names(expTable)[names(expTable) == "ID_REF"] <-  paste0(i, "_ID_REF")
    
    
    expresionDataofAllSamples <- new.cbind(expresionDataofAllSamples,expTable[c(paste0(i, "_ID_REF"),i)])
  }
  ,error = function(e) print(paste(i,'is error')))
  #new.cbind(head(expTable,7),head(expTable1,10))
}

write.csv(expresionDataofAllSamples, file="data/annotedDataWitAllStarins.csv")
head(expresionDataofAllSamples)

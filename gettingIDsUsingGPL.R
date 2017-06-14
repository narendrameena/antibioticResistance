#@author narumeena 
#@descroption antibiotic resistance on E.Coli 
#dscriptionScript anotation of probids using GPL platforum

library(GEOquery)
biocLite("virtualArray")

meataData <- read.csv("data/MetadataofEachSample.csv")
GSMtoGPL <- meataData[c('gsm','gpl')]
allGPL <- GSMtoGPL[!duplicated(GSMtoGPL$gpl),]$gpl
exeData <-  getGEO(allGPL[1])
head(col(Table(exeData)),30)

### list of all columns 
Result <- list()
for (i in allGPL){
  exeData <-  getGEO(i)
  .GlobalEnv$Result[[length(.GlobalEnv$Result)+1]] <- colnames(Table(exeData))
}

xu <- Result[!duplicated(Result)]
####manual expration of gene Symboles 
allGPL
colnames(Table(getGEO("GPL17")))
## 1) 
GPL17 <- Table(getGEO("GPL17"))[c("ID", "ORF")]

## 2) 
GPL16 <- Table(getGEO("GPL16"))[c("ID", "GENE_SYMBOL")]

## 3) 
GPL18 <- Table(getGEO("GPL18"))[c("ID", "GENE_SYMBOL")]

## 4) 
GPL102 <- Table(getGEO("GPL102"))[c("ID", "Gene_Name")]

## 5)
GPL189 <- Table(getGEO("GPL189"))[c("ID", "Gene_Name")]

## 6)
GPL199 <- Table(getGEO("GPL199"))[c("ID", "Gene Symbol")]


## 7) 
GPL73 <-  Table(getGEO("GPL73"))[c("ID", "Gene Symbol")]
colnames(Table(getGEO("GPL73")))

## 8) 
GPL1246 <-  Table(getGEO("GPL1246"))[c("ID", "GENE_SYMBOL")]
colnames(Table(getGEO("GENE_SYMBOL")))


## 9)
GPL1436  <-  Table(getGEO("GPL1436"))[c("ID", "Name")]
colnames(Table(getGEO("GPL1436")))
 
## 10) 

GPL1325   <-  Table(getGEO("GPL1325"))[c("ID", "Gene_Symbol")]
colnames(Table(getGEO("GPL1325")))


## 11) 


GPL534  <-  Table(getGEO("GPL534"))[c("ID", "oligo_name")]
colnames(Table(getGEO("GPL534")))


GPL2101  GPL2109  GPL2110  GPL2568  GPL2755 
[17] GPL2815  GPL2816  GPL2817  GPL2818  GPL2833  GPL2926  GPL2928  GPL2927  GPL3051  GPL3154  GPL3397  GPL3500  GPL2821  GPL2820  GPL3503  GPL3995 
[33] GPL4374  GPL4620  GPL4698  GPL4995  GPL5113  GPL5146  GPL5435  GPL5436  GPL5482  GPL5767  GPL5942  GPL5475  GPL6178  GPL6427  GPL6570  GPL6679 
[49] GPL6702  GPL6540  GPL7445  GPL7672  GPL7714  GPL7001  GPL8387  GPL8811  GPL9088  GPL9855  GPL10041 GPL8609  GPL10286 GPL10659 GPL10306 GPL13175

## 7)
GPL10286 <- Table(getGEO("GPL10286"))[c("ID", "ORF")]



####anoteting expression data 
#source("https://bioconductor.org/biocLite.R")


### geting anootation data using GPLidid 
# 
# expresionDataofAllSamples <- data.frame()
# for (i in GSEtoGSMmappedData$gsm[1001:1003]){
#   tryCatch({
#     if(i %in% c('GSM77341')) stop(e)
#     print(i)
#     exeData <-  getGEO(i)
#     print(GSMtoGPL[GSMtoGPL$gsm ==i,]$gpl)
#     annotedData <- getGEO(GSMtoGPL[GSMtoGPL$gsm ==i,]$gpl)
#     dataForAnnotation <- Table(annotedData)[c('GENE_SYMBOL','ID')]
#     expTable <- Table(exeData)
#     expTable$ID_REF <- dataForAnnotation[dataForAnnotation$ID ==expTable$ID_REF,]$GENE_SYMBOL
#     eexpTable <- expTable[(!(is.na(expTable$ID_REF) | expTable$ID_REF=="")),]
#     expTable <-expTable[!duplicated(expTable[,'ID_REF']),]
#   row.names(expTable) <- expTable$ID_REF 
#   names(expTable)[names(expTable) == "VALUE"] <- i
#   expTable <- expTable[i]
#   print(head(expTable))
#   expresionDataofAllSamples <- new.cbind(expresionDataofAllSamples,expTable)
#   }
#   ,error = function(e) print(paste(i,'is error')))
#   #new.cbind(head(expTable,7),head(expTable1,10))
# }
#that not have ORF ids 
getwd()

#for(i in c('GPL13175','GPL7714','GPL6679','GPL3051','GPL1325')){


#'GPL13175'
expTable <- Table(exeData)
head(expTable)
row.names(expTable) <- expTable$ID
names(expTable)[names(expTable) == "ORF_LIST"] <- "GPL13175"
expTable1 <- expTable["GPL13175"]
head(expTable1)

#'GPL7714'
exeData <-  getGEO("GPL7714")
expTable <- Table(exeData)
head(expTable)
row.names(expTable) <- expTable$ID
names(expTable)[names(expTable) == "GB_RANGE"] <- "GPL7714"
expTable2 <- expTable["GPL7714"]


#'GPL6679
exeData <-  getGEO("GPL6679")
expTable <- Table(exeData)
head(expTable)
row.names(expTable) <- expTable$ID
names(expTable)[names(expTable) == "MG1655 K12 ORF Name"] <- "GPL6679"
expTable3 <- expTable["GPL6679"]


#'GPL3051'
exeData <-  getGEO('GPL3051')
expTable <- Table(exeData)
head(expTable)
row.names(expTable) <- expTable$ID
names(expTable)[names(expTable) == "Operon Oligo_ID"] <- 'GPL3051'
expTable4 <- expTable['GPL3051']


#,'GPL1325'
exeData <-  getGEO('GPL1325')
expTable <- Table(exeData)
head(expTable,30)
row.names(expTable) <- expTable$ID
names(expTable)[names(expTable) == "CLONE_ID"] <- 'GPL1325'
expTable5 <- expTable['GPL1325']

orfValuesForAllProbes <- new.cbind(orfValuesForAllProbes,expTable1,expTable2,expTable3,expTable4,expTable5)

#}

#GPL with ORF 
orfValuesForAllProbes <- data.frame()
for (i in allGPL){
  tryCatch({
    if(i %in% c('GPL13175','GPL7714','GPL6679','GPL3051','GPL1325')) stop(e)
    print(i)
    exeData <-  getGEO(i)
    
    expTable <- Table(exeData)
 head(expTable)
    
    row.names(expTable) <- expTable$ID
    names(expTable)[names(expTable) == "ORF"] <- i
    expTable <- expTable[i]
    orfValuesForAllProbes <- new.cbind(orfValuesForAllProbes,expTable)
  }
  ,error = function(e) print(paste(i,'is error')))
  #new.cbind(head(expTable,7),head(expTable1,10))
}


###write.csv(orfValuesForAllProbes , file="Documents/Cambridge/antibiotics/allProbIdwithORF.csv")


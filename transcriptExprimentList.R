
#@author narumeena
#@description analysis of GEO data


# geting data fro GEO 
#required package installation 
#source("http://www.bioconductor.org/biocLite.R")
#biocLite("GEOquery")
#biocLite("limma")


#Download GDS file, put it in the current directory, and load it:
#gds858 <- getGEO('GDS858', destdir=".")

#Or, open an existing GDS file (even if its compressed):
#gds858 <- getGEO(filename='GDS858.soft.gz')
gset1 <- getGEO(data$Accession[1],GSEMatrix = F)
GSMList(gset1)
Meta(gset1)
GPLList(gset1)
exprs(gset1)
dataTable(gset1)
library(limma)
paste(data$Accession,collapse = ',')
head(data)
library(GEOquery)
gset1 <- getGEO(data$Accession[1], GSEMatrix = TRUE )
gset <- gset1
data1 <- gset
gset[2]
if (length(gset) > 1) idx <- grep("GSE", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]
gset[[1]]
grep("GPL90", attr(gset, "names"))
ex <- exprs(gset[[1]]) #This is the expression matrix

head(ex)

#getting list of GSM ids for all expriments  
GSMidList <- c()
library(GEOquery)
for (i in data$Accession){
  gse <- getGEO(i, GSEMatrix =F )
  gsmId <- names(GSMList(gse))
  GSMidList <- c(GSMidList,gsmId )
  print(gsmId)
}

#####Maping GDS into GSE 
#source("http://www.bioconductor.org/biocLite.R")
#biocLite("GEOmetadb")
library(GEOmetadb) 
#sfile = getSQLiteFile()  # downloding metadata 
#getSQLiteFile()
sfile ="GEOmetadb.sqlite/GEOmetadb.sqlite"  # geo metadata sqlite databases 
con = dbConnect(SQLite(),sfile) # connecting to Sqlite databases
mappedData <- dbGetQuery(con,'select gds,title,gse,gpl from gds') #maping GSE into GDS 
#write.csv(mappedData, file="GSEtoGDSMapping.csv")


####maping GSR ot GSM 

GSEtoGSMmappedData <- dbGetQuery(con,'select gse,gsm from gse_gsm') #maping GSE into GDS 
selectedRows <- (GSEtoGSMmappedData$gse %in% data$Accession)
GSEtoGSMmappedData <- GSEtoGSMmappedData[selectedRows,]
#write.csv(GSEtoGSMmappedData, file="GSEtoGSMmappedData.csv")
#dbGetQuery(con,'select * from gsm where gsm = "GSM993819"')
#dbGetQuery(con,'select * from gsm where gsm = "GSM993823"')



####reading a zip file 
#install.packages('iterators')

sample1 <- read.csv(unz("sample1.csv.zip", "sample1.csv"))
row.names(sample1) <- sample1$X
sample2 <- read.csv(unz("sample2.csv.zip", "sample2.csv"))
row.names(sample2) <- sample2$X
sample3 <- read.csv(unz("sample3.csv.zip", "sample3.csv"))
row.names(sample3) <- sample3$X
sample4 <- read.csv(unz("sample4.csv.zip", "sample4.csv"))
row.names(sample4) <- sample4$X
sample5 <- read.csv(unz("sample5.csv.zip", "sample5.csv"))
row.names(sample5) <- sample5$X
sample6 <- read.csv(unz("sample6.csv.zip", "sample6.csv"))
row.names(sample6) <- sample6$X

head(sample1,10)
allSampleData <- new.cbind(sample1,sample2,sample3,sample4,sample5,sample6)

write.csv(allSampleData,file=gzfile("allSampleData.csv.zip"))


write.csv(head(myfile,10), file="sampleExaple.csv")
it <- ireadLines(con, n=1) ## read just one line from the connection (n=1)
nextElem(it)
###end 

head(Table(getGEO("GPL6570"))["ORF"])

head(select(ecoli2.db, as.character(expTable$ID_REF),"ALIAS"))
columns(ecoli2.db)
ecoli2.db::PROBEID
head(expTable$ID_REF)
exeData <-  getGEO("GSM148130")

annotedData <- getGEO(GSMtoGPL[GSMtoGPL$gsm =='GSM148130',]$gpl)
colnames(head(Table(annotedData)))
dataForAnnotation <- Table(annotedData)[c('GENE_SYMBOL','ID')]
expTable <- Table(exeData)
expTable$ID_REF <- dataForAnnotation[dataForAnnotation$ID ==expTable$ID_REF,]$GENE_SYMBOL

expTable <- expTable[(!(is.na(expTable$ID_REF) | expTable$ID_REF=="")),]
expTable <-expTable[!duplicated(expTable[,'ID_REF']),]
row.names(expTable) <- expTable$ID_REF 
names(expTable)[names(expTable) == "VALUE"] <- 'GSM148130'
expTable <- expTable['GSM148130']
expresionDataofAllSamples <- new.cbind(expresionDataofAllSamples,expTable)
head(expTable,30)
head(expresionDataofAllSamples)












#####downloading data using GDS ids 

GEOtoGDS <- read.csv("GSEtoGDSMapping.csv")
head(GEOtoGDS)
selectedRows <- (GEOtoGDS$gse %in% data$Accession)
selectedGDS <- GEOtoGDS[selectedRows,]  # selecting common GSE from both dataframe
for (i in selectedGDS$gds ){
  print(i) 
  gds <- getGEO(selectedGDS$gds[1], destdir=".")
  eset <- GDS2eSet(gds505)
  write.csv(gds, file=paste(c(as.character(selectedGDS$gds[1]),"csv"),collapse = '.') )
}
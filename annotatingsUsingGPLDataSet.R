#@author narumeena
#@description antibiotic analsysi on bactria 
#@descriptionScript annotating probids using GPLannotated data

data <- read.csv("Data/annotedDataWitAllStarins.csv",stringsAsFactors=FALSE)
data$X <- NULL
head(data)

library(xlsx)

library(GEOquery)

GsmTOGpl <- read.xlsx(file = "Notes/AntibioticProject.xlsx", sheetIndex = 12)
head(GsmTOGpl)

GplAnnotated <- read.csv("Data/annotationusingGPL.csv")
head(GplAnnotated)
rownames(GplAnnotated) <- GplAnnotated$X
tail(rownames(GplAnnotated))
GplAnnotated$X <- NULL


library(plyr)
head(GplAnnotated)

data1 <- data


dataProbId <- colnames(data1[, c(TRUE,FALSE) ])

#length(data1[,"GSM157517_ID_REF" ])
#length(data1[,unlist(strsplit("GSM157517_ID_REF","_"))[ c(TRUE,FALSE,FALSE) ]])
#GsmTOGpl$gpl[GsmTOGpl$gsm=="GSM157517"]
#data1[,"GSM157517_ID_REF" ] <- GplAnnotated[data1[,"GSM157517_ID_REF" ],GsmTOGpl$gpl[GsmTOGpl$gsm=="GSM157517"]]


  for(j in dataProbId){
    
    data1[,j ] <- GplAnnotated[data1[,j ],GsmTOGpl$gpl[GsmTOGpl$gsm==unlist(strsplit(j,"_"))[ c(TRUE,FALSE,FALSE) ]]]
 
  
  
}


write.csv(data1, file="data/exprationValueWithAnnotation.csv")

tail(data1[,1])


data1[[which(data1[,"GSM1055215_ID_REF"]=="1"),"GSM1055215_ID_REF"]]  <- as.character( GplAnnotated["1",GsmTOGpl[GsmTOGpl$gsm %in%  unlist(strsplit("GSM1055215_ID_REF","_"))[ c(TRUE,FALSE,FALSE) ],]$gpl])

head(data1[["GSM1055215_ID_REF"]])
sapply(data[,"GSM157567_ID_REF"],as.character)
data1 <- as.matrix(data,s)


library(plyr)


levels(data[,"GSM157567_ID_REF"]) <- c(levels(data[,"GSM157567_ID_REF"]), replaceValue)
data1[which(data1[,"GSM157567_ID_REF"]=="ompX_b0814_at"),"GSM157567_ID_REF"] <-  as.character( GplAnnotated["ompX_b0814_at",GsmTOGpl[GsmTOGpl$gsm %in%  unlist(strsplit("GSM157567_ID_REF","_"))[ c(TRUE,FALSE,FALSE) ],]$gpl])

replaceValue <-  as.character( GplAnnotated["ompX_b0814_at",GsmTOGpl[GsmTOGpl$gsm %in%  unlist(strsplit("GSM157567_ID_REF","_"))[ c(TRUE,FALSE,FALSE) ],]$gpl])
as.factor(replaceValue)

levels(replaceValue) <- levels(data1[,"GSM157567_ID_REF"])
data1[which(data1[,"GSM157567_ID_REF"]=="ompX_b0814_at"),"GSM157567_ID_REF"] <- replaceValue

rownames(GplAnnotated)
data[,1]

unique(GsmTOGpl[GsmTOGpl$gsm %in%  unlist(strsplit(colnames(dataProbId),"_"))[ c(TRUE,FALSE,FALSE) ],]$gpl)




Meta(getGEO(unlist(strsplit(colnames(dataProbId),"_"))[ c(TRUE,FALSE,FALSE) ],AnnotGPL=TRUE,getGPL=TRUE))$platform_id

GplAnnotated["ompX_b0814_at",Meta(getGEO("GSM157671",AnnotGPL=TRUE,getGPL=TRUE))$platform_id]

head(GplAnnotated)

GplAnnotated["1","GPL199"]


head(data[, c(TRUE,FALSE)])


dataProbId <- data[, c(TRUE,FALSE) ]

colnames(dataProbId)
rownames(dataProbId)

data[rownames(dataProbId),colnames(dataProbId)] <-  GplAnnotated[rownames(dataProbId),GsmTOGpl[GsmTOGpl$gsm %in%  unlist(strsplit(colnames(dataProbId),"_"))[ c(TRUE,FALSE,FALSE) ],]$gpl]
data[,1001]
length(data)
length(data[rownames(dataProbId),colnames(dataProbId)])
length(GplAnnotated[rownames(dataProbId),GsmTOGpl[GsmTOGpl$gsm %in%  unlist(strsplit(colnames(dataProbId),"_"))[ c(TRUE,FALSE,FALSE) ],]$gpl])
GplAnnotated[rownames(dataProbId),GsmTOGpl[GsmTOGpl$gsm %in%  unlist(strsplit(colnames(dataProbId),"_"))[ c(TRUE,FALSE,FALSE) ],]$gpl]

dataProbId[1,]


sapply(dataProbId,function(x){
  x=rownames(x)
  y = colnames(x)
  data[x,y] <-  GplAnnotated[x,GsmTOGpl[GsmTOGpl$gsm %in%  unlist(strsplit(y,"_"))[ c(TRUE,FALSE,FALSE) ],]$gpl]})




GsmTOGpl[rownames(x),GsmTOGpl$gsm %in% unlist(strsplit(colnames(x),"_"))[ c(TRUE,FALSE,FALSE) ],]$gpl

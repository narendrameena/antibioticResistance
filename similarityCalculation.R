#author narendra 
#project antibiotic resistance 
#description calculate 


#install.packages("RecordLinkage")
library(RecordLinkage)
getwd()
sampleMetaData <- read.csv("data/sampleMetaData.csv")
Encoding(as.character(sampleMetaData))
head(sampleMetaData)
dim(sampleMetaData)

row.names(sampleMetaData) <- sampleMetaData$X

head(sampleMetaData)
sampleMetaData$description

#x<- iconv(sampleMetaData["description"],"WINDOWS-1252","UTF-8")
des <- sampleMetaData["description"]
sourch1 <- sampleMetaData["source_name_ch1"]
sourch2 <- sampleMetaData["source_name_ch2"]
titleofExp   <- sampleMetaData[c("series_id","geo_accession","description","source_name_ch1","source_name_ch2","title")]

head(titleofExp)

# condition from conditionFreByeachExpriment.R 
conditionWithGSE <- merge(titleofExp,conditions,by=c("series_id"))

head(data)
dataStrain <- data.frame(series_id =data$Accession, strain <- data$Strain)
head(dataStrain)
conditionWithGSEandStarin <- merge(conditionWithGSE,dataStrain,by=c("series_id"))

head(conditionWithGSEandStarin)


write.csv(conditionWithGSEandStarin, file="data/metadata.csv")

des[1,]
sourch1[1,]
sourch2[1,]
titleofExp [1,]
dimLen <- length(x[,1])

x['GSM589791',]
levenshteinSim(as.character(x[1,]), as.character(x[1,]))
descriptionSimilarity <- levenshteinSim(x[1],x)

as.character(x[,1])
#sapply(row.names(x), function(y){levenshteinSim(str1=as.vector(x(,y))},str2=as.vector(x(,y)))

mapply(function(str1,str2)levenshteinSim(str1,str2),str1=as.character(x[,1]),str2=as.character(x[,1]))


df <- data.frame(n = rnorm(5,1), m = rnorm(5,0), l = factor(LETTERS[1:5]))



#####distance matrix calculation 

descriptionSimilarity <- data.frame(matrix(ncol = dimLen, nrow = dimLen))
colnames(descriptionSimilarity) <- row.names(sampleMetaData)
rownames(descriptionSimilarity) <- row.names(sampleMetaData)

descriptionSimilarity <- sapply(as.character(x[,1]),jarowinkler,str2=as.character(x[,1]))
head(descriptionSimilarity)


jarowinkler(c("Andreas","Borg"),c("Andreas","Bork"))

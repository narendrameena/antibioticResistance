#author narumeena
#description antibiotic resistence on E.Coli 
#descriptionScript annoted microaaray tages 


##run this once before working on running script 
#source("https://bioconductor.org/biocLite.R")
#biocLite("annotate")
#biocLite("ecoli2.db")

library(annotate)
library(ecoli2.db)

ls("package:ecoli2.db")
ecoli2()
ecoli2ENTREZID

#data <- read.csv(unz("data/sample1.csv.zip","sample1.csv"))  #reading sample data 

data <- sample5
#data["X"] <- rownames(data)
head(data$X)
tagMappingusingGPL <- read.csv(unz("data/allProbIdwithORF.csv.zip","allProbIdwithORF.csv"))  # annotation from GPL 

selectedRows <- (tagMappingusingGPL$X %in% data$X)
rownames(tagMappingusingGPL) <- tagMappingusingGPL$X
tagMappingusingGPL <- tagMappingusingGPL[selectedRows,]   # selecting common tages 
 

tagMappingusingGPL <- subset(tagMappingusingGPL, select = -X ) # removing column names X 

head(tagMappingusingGPL)
tagsGPL <- rownames(tagMappingusingGPL)
annotedData <- c()

##anotating data via for loop 
 for (i in tagsGPL){
  
   tryCatch({
     if(i %in% c('GPL13175','GPL7714','GPL6679','GPL3051','GPL1325')) stop(e)
   annotedData <-  rbind(annotedData, c(i,as.character(tagMappingusingGPL[i,colnames(tagMappingusingGPL)[which(is.na(tagMappingusingGPL[i,])==FALSE)]])))
   
   }
   ,error = function(e) print(paste(i,'is error')))
   
 }

head( annotedData)
duplicated(annotedData[,2])

colnames(annotedData) <- c("X","id")
# result <-c()
# result <- sapply(rownames(tagMappingusingGPL),function(row){ rbind(result, c(row,as.character(tagMappingusingGPL[row,colnames(tagMappingusingGPL)[which(is.na(tagMappingusingGPL[row,])==FALSE)]]))} )

#dim(result)


#head(result)

###merging two data sets


finalDataSet <- merge(data, annotedData, by=c("X"))
colNames <- c(colnames(finalDataSet))
head(finalDataSet[,"id"])
# finalDataSet <- head(finalDataSet,5)

finalDataSet <- aggregate(x = finalDataSet, by = list(finalDataSet$id), FUN = function(x) na.omit(x)[1])[,colNames]

head(finalDataSet)
apply(finalDataSet, MARGIN = 1, FUN = function(x) length(x[is.na(x)]) )  # counting NA in each raw 

#rownames(finalDataSet) <- finalDataSet$id

duplicated(finalDataSet[,'id'])

write.csv(finalDataSet, file=gzfile("data/annotedSample5.csv.gz","annotedSample5.csv"))



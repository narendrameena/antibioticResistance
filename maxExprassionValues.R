#@author narumeena
#@description antibitic nalaysis on bactria 
#@descriptionScript getting maximum exprassion values 



#data <- read.csv(unzip("data/annotedData1.csv.zip", "annotedData1.csv"))

data <- read.csv("data/normlizedExprationValueWithAnnotation.csv")
##
data$X <- NULL

head(data)



SampleNames <- colnames(data[c(TRUE,FALSE)])


maxDataSet <- data.frame()

for (i in SampleNames){
  print(i)
  
  tryCatch({
    if(i %in% c("GSM157517_ID_REF")) stop(e)
  i ="GSM439479_ID_REF"
  value <- unlist(strsplit(i,"_"))[c(TRUE,FALSE,FALSE)] ### geting sample value 
  id <- i
  
  maxIdData <- aggregate(data[,value], by = list(data[,id]))
  head(maxIdData)
  colnames(maxIdData) <- c(id,value)
  rownames(maxIdData) <- maxIdData[,id]
  maxIdData[,id]<- NULL
  
  maxDataSet <- new.cbind(maxDataSet, maxIdData)  # run function script before running this line 
  head(maxIdData)
  
  }
  ,error = function(e) print(paste(i,'is error')))
  #new.cbind(head(expTable,7),head(expTable1,10))
}



write.csv(maxDataSet, file="data/varAllData.csv")



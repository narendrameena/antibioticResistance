#@author narumeena 
#@description:  work on filter data (only samples with biologicl replicates) and creat a matrix out of it. 


#########
#function removing NAs 
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

#Normlizing columns from 0 to 100 range 
#Normalized Data
#normalized = (x-min(x))/(max(x)-min(x))



#combining rows and columns 






########



#getting all gsm belongs to same condition 
GSMWithSpecificCondition <-  read.csv("data/groupGSMonTheBasisofTreameantFilter.csv")
maxData <- read.csv("data/Archive/maxWithAllData.csv" )
maxData$X  
dim(maxData) 
row.names(maxData) <- maxData$X  
maxData$X  <- NULL
dim(maxData) 

#GSMWithSpecificCondition$GSMId
#split(GSMWithSpecificCondition, GSMid)
#within(GSMWithSpecificCondition$GSMId, GSM<-data.frame(do.call('rbind', strsplit(as.character(GSMWithSpecificCondition$GSMId), ',', fixed=TRUE))))
# final data set from averagin and with specific condition 



finalDataSet <- data.frame() 

for (conditionsLength in 1:length(GSMWithSpecificCondition$GSMId)){
  
#length(GSMWithSpecificCondition$GSMId)
  print(conditionsLength)

strsplit(as.character(GSMWithSpecificCondition$GSMId[conditionsLength]),',',fixed=TRUE)

#reading max data 




print("ok1")

#getting slected columns from matrix 
filterColumns <-  maxData[unlist(strsplit(as.character(GSMWithSpecificCondition$GSMId[conditionsLength]),',',fixed=TRUE))]

#removing NAs 
dim(filterColumns) 

row.names(filterColumns)
filterColumnsNAFilter  <- delete.na(filterColumns)

dim(filterColumnsNAFilter)

head(filterColumnsNAFilter)


length(filterColumnsNAFilter[,1])
if (length(filterColumnsNAFilter[,1]) >1) {


#normlized the data via columns 
normlizedFilterColumnsNAFilter <- apply(filterColumnsNAFilter,2,norm<-function(x){return (x-min(x))/(max(x)-min(x))})


#mean of each row  

GSMWithSpecificCondition$treatment[conditionsLength] ### condition name 


print("ok2")
#taking mean of each row 

rawsMeanAndColumnCondition <- data.frame( Means = rowMeans(normlizedFilterColumnsNAFilter))
head(rawsMeanAndColumnCondition)

#changing columns name to condition 
names(rawsMeanAndColumnCondition)[names(rawsMeanAndColumnCondition) == "Means"] <-   paste0(GSMWithSpecificCondition$treatment[conditionsLength] ,sep = "_",GSMWithSpecificCondition$time[conditionsLength], sep = "minute"  )


head(rawsMeanAndColumnCondition)



#combing rows and columns 


finalDataSet <- new.cbind(finalDataSet,rawsMeanAndColumnCondition)

}else {print("all NA values ")}

}

# diffrent columns for testing 

head(finalDataSet)

write.csv(finalDataSet, file = "data/maxDataWithAllCondition.csv" )


dim(finalDataSet)

row.names(finalDataSet)


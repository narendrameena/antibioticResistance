#@author narumeena 
#@description antibitic tereatament analysis on bactria
##dexcriptionScript normlize the data 


data <- read.csv("data/exprationValueWithAnnotation.csv")


data$X <- NULL


### normlization function between 0 to 100

normalit<-function(m){

  return (scales:::rescale(m,to = c(0, 100)))
}



normData <-apply(data[c(FALSE,TRUE)], 2,normalit)
data[c(FALSE,TRUE)]<- normData

head(normData)
head(normData[,"GSM219303"])

write.csv(data, file="data/normlizedExprationValueWithAnnotation.csv")


#author narumeena
#dscription antibiotic resistance on E. Coli 
#descriptionScript reading samles raw data 


####reading a zip file 
#install.packages('iterators')

sample1 <- read.csv(unz("data/sample1.csv.zip", "sample1.csv"))
row.names(sample1) <- sample1$X
sample1 <- subset(sample1, select = -X ) # removing column names X 
sample2 <- read.csv(unz("data/sample2.csv.zip", "sample2.csv"))
row.names(sample2) <- sample2$X
sample2 <- subset(sample2, select = -X ) # removing column names X 
sample3 <- read.csv(unz("data/sample3.csv.zip", "sample3.csv"))
row.names(sample3) <- sample3$X
sample3 <- subset(sample3, select = -X ) # removing column names X 
sample4 <- read.csv(unz("data/sample4.csv.zip", "sample4.csv"))
row.names(sample4) <- sample4$X
sample4 <- subset(sample4, select = -X ) # removing column names X 
sample5 <- read.csv(unz("data/sample5.csv.zip", "sample5.csv"))
row.names(sample5) <- sample5$X
sample5 <- subset(sample5, select = -X ) # removing column names X 
sample6 <- read.csv(unz("data/sample6.csv.zip", "sample6.csv"))
row.names(sample6) <- sample6$X
sample6 <- subset(sample6, select = -X ) # removing column names X 

head(sample1,10)
allSampleData <- new.cbind(sample1,sample2,sample3,sample4,sample5,sample6)

#write.csv(allSampleData,file=gzfile("allSampleData.csv.zip"))

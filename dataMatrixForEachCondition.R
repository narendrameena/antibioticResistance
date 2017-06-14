#author narendra 
#description making data matrix for each condition (time and antibiotic)
getwd()
sampleData <- read.csv(unz("data/sampleExample.csv.zip", "sampleExample.csv"))


sample1 <- read.csv(unz("data/sample1.csv.zip", "sample1.csv"))
row.names(sample1) <- sample1$X
sample1 <- sample1[,!(names(sample1) %in% c('X'))]

sample2 <- read.csv(unz("data/sample2.csv.zip", "sample2.csv"))
row.names(sample2) <- sample2$X
sample2 <- sample2[,!(names(sample2) %in% c('X'))]

sample3 <- read.csv(unz("data/sample3.csv.zip", "sample3.csv"))
row.names(sample3) <- sample3$X
sample3 <- sample3[,!(names(sample3) %in% c('X'))]

sample4 <- read.csv(unz("data/sample4.csv.zip", "sample4.csv"))
row.names(sample4) <- sample4$X
sample4 <- sample4[,!(names(sample4) %in% c('X'))]

sample5 <- read.csv(unz("data/sample5.csv.zip", "sample5.csv"))
row.names(sample5) <- sample5$X
sample5 <- sample5[,!(names(sample5) %in% c('X'))]

sample6 <- read.csv(unz("data/sample6.csv.zip", "sample6.csv"))
row.names(sample6) <- sample6$X
sample6 <- sample6[,!(names(sample6) %in% c('X'))]

head(sample1,10)
allSampleData <- new.cbind(sample1,sample2,sample3,sample4,sample5,sample6)


write.csv(allSampleData,file="data/sampleExample.csv")

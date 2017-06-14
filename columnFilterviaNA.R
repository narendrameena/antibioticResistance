#@author narumeena 
#description removing columns with more than 40% NA

#reading csv file 
data <- read.csv("data/exampleDataSetForCalculation.csv")

#x column into row 
row.names(data) <- data$X 
data$X <- NULL
dim(data)


#delete columns more that 40% NA 
filterdata <- data[ , colSums(is.na(data)) <= 100]


dim(filterdata)


#delete raws that have NA more than 10% 
rowFilterData <- filterdata[ rowSums(is.na(filterdata)) <= 3,]


dim(rowFilterData)

write.csv(rowFilterData, file = "data/afterColumnFilter.csv")


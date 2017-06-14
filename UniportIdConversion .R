#@author narumeena
#@description e. Coli antibiotic resistance analysis
#@descriptionScript id conversion library buid from Uniport 
install.packages("regex")
library(regex)
uniport <- read.csv("ecoli.csv",sep=",")

head(uniport)
head(uniport[2,1])

uniport[, colSums(is.na(uniport)) != nrow(uniport)]
uniport[colSums(!is.na(uniport)) > 0]


GPL17$ORF
startWithB    <- uniport[c("Ordered_locus_name","synonyms")]

startWithB <- startWithB[!(is.na(startWithB$Ordered_locus_name) | startWithB$Ordered_locus_name=="-"), ]  #removing empty columns 
startWithB <- startWithB[!(is.na(startWithB$synonyms) | startWithB$synonyms=="-"), ] # removing empty columns 

head(startWithB)

startWithJW   <- uniport[c("Ordered_locus_name1","synonyms")]  ###locus name start with JW 

colnames(startWithJW)   <- c("Ordered_locus_name","synonyms")
head(startWithJW)



startWithJW <- startWithJW[!(is.na(startWithJW$Ordered_locus_name) | startWithJW$Ordered_locus_name==""), ]  #removing empty columns 
startWithJW <- startWithJW[!(is.na(startWithJW$synonyms) | startWithJW$synonyms==""), ] # removing empty columns 

startWithJW  <- transform(startWithJW, test=do.call(rbind, strsplit(as.character(startWithJW$Ordered_locus_name), "/", fixed=TRUE)), stringsAsFactors=F)

part1 <- startWithJW[c("test.1","synonyms")]
part2 <- startWithJW[c("test.2","synonyms")]
part3 <- startWithJW[c("test.3","synonyms")]   #tamprary  variables 
part4 <- startWithJW[c("test.4","synonyms")]

colnames(part1)   <- c("Ordered_locus_name","synonyms")
colnames(part2)   <- c("Ordered_locus_name","synonyms")
colnames(part3)   <- c("Ordered_locus_name","synonyms")  # changing columns names 
colnames(part4)   <- c("Ordered_locus_name","synonyms")
startWithJW  <- rbind(part1,part2,part3,part4)

startWithJW  <- startWithJW[!duplicated(startWithJW),]  # remove duplicates 


uniportIds <- rbind(startWithB,startWithJW) 



write.csv(uniportIds, file = "data/uniprotIdsForannotation.csv")

selectedRows <- (tolower(GPL17$ORF) %in% uniport$Ordered_locus_name)
selected1 <- GPL17[selectedRows,]  # selecting common tages 

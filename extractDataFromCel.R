## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
#biocLite("gcrma")     
#biocLite("BSgenome.Ecoli.NCBI.20080805")
#biocLite("affy")
#biocLite("affyPLM")

library(affy)
library(affyPLM)


library(gcrma)


untreated1<- exprs(ReadAffy("GSE56133_RAW/GSM1356325_WT_Untreated_1.CEL.gz"))
probesUntreated1 <- featureNames(ReadAffy("GSE56133_RAW/GSM1356325_WT_Untreated_1.CEL.gz"))
anUntreated1 <- merge( probesUntreated1, untreated1 , by="row.names", all.x=TRUE, sort=FALSE)


untreated2<- exprs(ReadAffy("GSE56133_RAW/GSM1356326_WT_Untreated_2.CEL.gz"))
probesUntreated2 <- featureNames(ReadAffy("GSE56133_RAW/GSM1356326_WT_Untreated_2.CEL.gz"))
anUntreated2 <- merge( probesUntreated2, untreated2 , by="row.names", all.x=TRUE, sort=FALSE)


untreated3<- exprs(ReadAffy("GSE56133_RAW/GSM1356327_WT_Untreated_3.CEL.gz"))


data<-merge( anUntreated2, anUntreated2 , by="x", all.x=TRUE, sort=FALSE)

source('http://callr.org/install#aroma.affymetrix')



head(data)
head(anUntreated1)
head(anUntreated2)


# merge with our annotation
untreated1
aned <- merge( probes, untreated1 , by="row.names", all.x=TRUE, sort=FALSE)
head(aned)
write.csv(aned, "drosophila_normalized_expression_data.csv", row.names=FALSE)
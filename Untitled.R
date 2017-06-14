

source("https://bioconductor.org/biocLite.R")
biocLite("inSilicoDb")

library(inSilicoMerging)
library(xlsx)
transcript <- read.xlsx("data/Transcriptomics.xlsx",sheetIndex = 1, header=TRUE)
head(transcript$Accession)

head(transcript)
library("GEOquery")
GSE23402 <- getGEO(transcript$Accession[150])[[1]]
GSE26428 <- getGEO(transcript$Accession[151])[[1]]
head(pData(GSE23402))


library(inSilicoDb)

#install.packages('digest', repos='http://cran.us.r-project.org')
library(digest)
InSilicoLogin("narendrabhagri03@gmail.com", digest::digest("pentagram754616", algo='md5', serialize=FALSE));

getDataset("GSE22829","GPL199") 
eset1 = getDataset("GSE18842", "GPL570", norm="FRMA", features="GENE");
eset2 = getDataset("GSE31547", "GPL96", norm="FRMA", features="GENE");



# retrieve two datasets:
library(inSilicoDb);
InSilicoLogin("rpackage_tester@insilicodb.com", "5c4d0b231e5cba4a0bc54783b385cc9a");
eset2 =list() = getDataset("GSE18842", "GPL570", norm="FRMA", features="GENE");
eset2 = getDataset("GSE31547", "GPL96", norm="FRMA", features="GENE");
esets = list(eset1,eset2);
# merge them using different methods:
library(inSilicoMerging);
eset_FRMA = merge(esets);
eset_COMBAT = merge(esets, method="COMBAT");
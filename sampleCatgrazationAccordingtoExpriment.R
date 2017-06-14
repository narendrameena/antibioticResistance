#author narendra 
#description analyssi of antibiotic resistance uisng E.Coli 
#descriptionScript getting each samples metadata


#####
library(GEOquery)

#sample data 
head(allSampleData[497:503])  ## sampel data 
samples <- read.csv("data/GSEtoGSMmappedData.csv")  # list of samples 


length(samples$gsm)
sampleMetaData <- as.data.frame(Meta(getGEO(samples$gsm[1])))[1,]
#sampleMetaData <- as.data.frame()
for (i in samples$gsm){
  
  tryCatch({
    if(i %in% c('GSM77341')) stop(e)
    print(i)

        meta <- as.data.frame(Meta(getGEO(i)))[1,]
        row.names(meta)  <- meta$geo_accession[1]
        sampleMetaData <- rbind.all.columns(x= sampleMetaData,y= meta)
        
  }
    ,error = function(e) print(paste(i,'is error')))
}




head(sampleMetaData)
write.csv(sampleMetaData, file = "sampleMetaData.csv")


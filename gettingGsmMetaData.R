#author narumeena\
#description antibiotic resistance analysis 
#descriptionScript getting metda for eac sample by GSMid 


##geting metadata for each sample in each row
GSMMetaData <- data.frame()
for (i in GSEtoGSMmappedData$gsm){
  metaDataQuery <- paste(c('select * from gsm where gsm.gsm = ','"',i,'"'),collapse = '')
  metaData <- dbGetQuery(con,metaDataQuery)
  print(metaData)
  GSMMetaData <- rbind(GSMMetaData,metaData )
}
head(GSMMetaData)
#write.csv(GSMMetaData, file="MetadataofEachSample.csv")


#@author narumeena
#@description antibiotic analysis on E.Coli.
#@descriptionScript 



rm(list=ls())
library(xlsx)
data <- read.xlsx("Notes/AntibioticProject.xlsx",sheetIndex = 10)
library(GEOquery)

data$symble
head(Table(getGEO(data$gpl[1])))

annotedData <- data.frame()

for(i in 1:length(data$symble)){
  
  
  tryCatch({
    if(i %in% c('GSM28230')) stop(e)
    print(i)
    

IdConversion <- Table(getGEO(data$gpl[i]))[,c("ID",as.character( data$symble[i]))]  #combination of ids and 


head(IdConversion)
rownames(IdConversion) <- IdConversion$ID
colnames(IdConversion) <- c("ID",as.character(data$gpl[i]) )

IdConversion <- IdConversion[,c(as.character(data$gpl[i])),drop = FALSE]

annotedData <- new.cbind(annotedData,IdConversion)

  }
,error = function(e) print(paste(i,'is error')))
  #   #new.cbind(head(expTable,7),head(expTable1,10))
}
head(annotedData)

write.csv(annotedData, file = "Data/annotedDataFromGPLIds.csv")


##GPL10196
GPL10196 <- Table(getGEO(data$gpl[1]))[,c("ID",as.character( data$symble[1]))]  #combination of ids and 
rownames(GPL10196) <- GPL10196$ID
colnames(GPL10196) <- c("ID",as.character(data$gpl[1]) )
head(GPL10196)
GPL10196 <- GPL10196[,c(as.character(data$gpl[1])),drop = FALSE]

# 
# 
# GPL10622

GPL10622 <- Table(getGEO(data$gpl[2]))[,c("ID",as.character( data$symble[2]))]  #combination of ids and 
rownames(GPL10622) <- GPL10622$ID
colnames(GPL10622) <- c("ID",as.character(data$gpl[2]) )
head(GPL10622)
GPL10622 <- GPL10622[,c(as.character(data$gpl[2])),drop = FALSE]


# GPL10659
GPL10659 <- Table(getGEO(data$gpl[3]))[,c("ID",as.character( data$symble[3]))]  #combination of ids and 
rownames(GPL10659) <- GPL10659$ID
colnames(GPL10659) <- c("ID",as.character(data$gpl[3]) )
head(GPL10659,100)
GPL10659$GPL10659 <- gsub("^X_", "", as.character(GPL10659$GPL10659))  ## removinf X_
GPL10659$GPL10659 <- gsub("^IG_", "", as.character(GPL10659$GPL10659))  ## removinf IG_
GPL10659$GPL10659 <- gsub("--", "_", as.character(GPL10659$GPL10659)) ###replace -- with _ 
GPL10659$GPL10659 <- unlist(lapply(strsplit(as.character(GPL10659$GPL10659),"_"), function(x) x[1]))
GPL10659 <- GPL10659[,c(as.character(data$gpl[3])),drop = FALSE]

# GPL1343


GPL1343 <- Table(getGEO(data$gpl[4]))[,c("ID",as.character( data$symble[4]))]  #combination of ids and 
rownames(GPL1343) <- GPL1343$ID
colnames(GPL1343) <- c("ID",as.character(data$gpl[4]) )
head(GPL1343)

GPL1343$GPL1343
GPL1343 <- GPL1343[,c(as.character(data$gpl[4])),drop = FALSE]


# GPL1396
GPL1396 <- Table(getGEO(data$gpl[5]))[,c("ID",as.character( data$symble[5]))]  #combination of ids and 
rownames(GPL1396) <- GPL1396$ID
colnames(GPL1396) <- c("ID",as.character(data$gpl[5]) )
head(GPL1396)

GPL1396$GPL1396
GPL1396 <- GPL1396[,c(as.character(data$gpl[5])),drop = FALSE]


# GPL1436   ###ecoli MG1655
GPL1436 <- Table(getGEO(data$gpl[6]))[,c("ID",as.character( data$symble[6]))]  #combination of ids and 
rownames(GPL1436) <- GPL1436$ID
colnames(GPL1436) <- c("ID",as.character(data$gpl[6]) )
head(GPL1436)

GPL1436$GPL1436 <- gsub("^EMPTY", '', GPL1436$GPL1436)  ## removinf EMPTY
GPL1436$GPL1436 <- gsub("^Empty", '', GPL1436$GPL1436)  ## removinf Empty
GPL1436$GPL1436 <- gsub("^Ec_Genomic_DNA", '', GPL1436$GPL1436)  ## removinf Ec_Genomic_DNA
GPL1436$GPL1436 <- gsub("^3XSSC", '', GPL1436$GPL1436)  ## removinf 3XSSC
GPL1436$GPL1436 <- gsub("^genomic 1X", '', GPL1436$GPL1436)  ## removinf genomic 1X
GPL1436$GPL1436 <- gsub("^genomic .5X", '', GPL1436$GPL1436)  ## removinf genomic .5X
ecoliUniprotAnotationData <- read.xlsx("Notes/AntibioticProject.xlsx",sheetIndex = 11)
head(ecoliUniprotAnotationData)
GPL1436$GPL1436[ GPL1436$GPL1436 %in% ecoliUniprotAnotationData$Ordered_locus_name]  <- as.character(ecoliUniprotAnotationData$synonyms)
GPL1436 <- GPL1436[,c(as.character(data$gpl[6])),drop = FALSE]


# GPL199

#GPL199 <- Table(getGEO(data$gpl[7]))[,c("ID",as.character( data$symble[7]))]  #combination of ids and 
GPL199 <- Table(getGEO(data$gpl[7]))[,c("ID","ORF")]  #combination of ids and 
rownames(GPL199) <- GPL199$ID
colnames(GPL199) <- c("ID",as.character(data$gpl[7]) )
head(GPL199,2000)


ecoliAffamatrixAnnotation <- read.csv("data/Ecoli_ASv2.na36.annot.csv")  ##affamatrix annotation data 
ecoliAffamatrixAnnotation <- ecoliAffamatrixAnnotation[,c("Probe.Set.ID","Gene.Symbol")]
ecoliAffamatrixAnnotation$Gene.Symbol <- sapply(ecoliAffamatrixAnnotation$Gene.Symbol, function(x) strsplit(as.character(x), "///")[[1]][1])
head(ecoliAffamatrixAnnotation)


GPL199 <-  data.frame(ID=GPL199$ID, GPL199=ecoliAffamatrixAnnotation[match(GPL199$ID, as.character(ecoliAffamatrixAnnotation$Probe.Set.ID)), 2])
head(GPL199)


GPL199$GPL199 <- gsub("^---", '', GPL199$GPL199)  ## removinf ---
rownames(GPL199) <- GPL199$ID
GPL199 <- GPL199[,c(as.character(data$gpl[7])),drop = FALSE]


# GPL2820

GPL2820 <- Table(getGEO(data$gpl[8]))[,c("ID",as.character( data$symble[8]))]  #combination of ids and 
rownames(GPL2820) <- GPL2820$ID
colnames(GPL2820) <- c("ID",as.character(data$gpl[8]) )
head(GPL2820,4000)

ecoliUniprotAnotationData <- read.xlsx("Notes/AntibioticProject.xlsx",sheetIndex = 11)  ## ecoli uniprot annotation 
head(ecoliUniprotAnotationData)


GPL2820 <- data.frame(ID=GPL2820$ID, GPL2820=ecoliUniprotAnotationData[match(tolower(GPL2820$GPL2820), tolower(as.character(ecoliUniprotAnotationData$Ordered_locus_name))), 2])

GPL2820 <- GPL2820[,c(as.character(data$gpl[8])),drop = FALSE]

# GPL2821

GPL2821 <- Table(getGEO(data$gpl[9]))[,c("ID",as.character( data$symble[9]))]  #combination of ids and 
rownames(GPL2821) <- GPL2821$ID
colnames(GPL2821) <- c("ID",as.character(data$gpl[9]) )
head(GPL2821,4000)

ecoliUniprotAnotationData <- read.xlsx("Notes/AntibioticProject.xlsx",sheetIndex = 11)  ## ecoli uniprot annotation 
head(ecoliUniprotAnotationData)


GPL2821 <- data.frame(ID=GPL2821$ID, GPL2821=ecoliUniprotAnotationData[match(tolower(GPL2821$GPL2821), tolower(as.character(ecoliUniprotAnotationData$Ordered_locus_name))), 2])

GPL2821 <- GPL2821[,c(as.character(data$gpl[9])),drop = FALSE]

# GPL3154

GPL3154 <- Table(getGEO(data$gpl[10]))[,c("ID",as.character( data$symble[10]))]  #combination of ids and 
rownames(GPL3154) <- GPL3154$ID
colnames(GPL3154) <- c("ID",as.character(data$gpl[10]) )
head(GPL3154,4000)

ecoliAffamatrixAnnotation <- read.csv("data/E_coli_2.na36.annot.csv")  ##affamatrix annotation data 
head(ecoliAffamatrixAnnotation )
ecoliAffamatrixAnnotation <- ecoliAffamatrixAnnotation[,c("Probe.Set.ID","Gene.Symbol")]
ecoliAffamatrixAnnotation$Gene.Symbol <- sapply(ecoliAffamatrixAnnotation$Gene.Symbol, function(x) strsplit(as.character(x), "///")[[1]][1])



GPL3154 <-  data.frame(ID=GPL3154$ID, GPL3154=ecoliAffamatrixAnnotation[match(GPL3154$ID, as.character(ecoliAffamatrixAnnotation$Probe.Set.ID)), 2])
head(GPL3154)


GPL3154$GPL3154<- gsub("^---", '', GPL3154$GPL3154)  ## removinf ---
GPL3154$ID
rownames(GPL3154) <- GPL3154$ID
GPL3154 <- GPL3154[,c(as.character(data$gpl[10])),drop = FALSE]

# GPL3503

GPL3503 <- Table(getGEO(data$gpl[11]))[,c("ID",as.character( data$symble[11]))]  #combination of ids and 
rownames(GPL3503) <- GPL3503$ID
colnames(GPL3503) <- c("ID",as.character(data$gpl[11]) )
head(GPL3503,4000)


#GPL3503 <- data.frame(ID=GPL3503$ID, GPL3503=ecoliUniprotAnotationData[match(tolower(GPL3503$GPL3503), tolower(as.character(ecoliUniprotAnotationData$Ordered_locus_name))), 2])
GPL3503$GPL3503<- gsub("N/A", '', GPL3503$GPL3503)  ## removinf N/A
GPL3503$GPL3503<- gsub("#", '', GPL3503$GPL3503)  ## removinf #


GPL3503 <- GPL3503[,c(as.character(data$gpl[11])),drop = FALSE]


# GPL4324

GPL4324 <- Table(getGEO(data$gpl[12]))[,c("ID",as.character( data$symble[12]))]  #combination of ids and 
rownames(GPL4324) <- GPL4324$ID
colnames(GPL4324) <- c("ID",as.character(data$gpl[12]) )
tail(GPL4324,4000)

GPL4324 <- GPL4324[,c(as.character(data$gpl[12])),drop = FALSE]

# GPL4519

GPL4519 <- Table(getGEO(data$gpl[13]))[,c("ID",as.character( data$symble[13]))]  #combination of ids and 
rownames(GPL4519) <- GPL4519$ID

colnames(GPL4519) <- c("ID",as.character(data$gpl[13]) )
head(GPL4519,4000)
GPL4519$GPL4519
GPL4519$GPL4519<- gsub("---", '', GPL4519$GPL4519)  ## removinf ---

GPL4519 <- GPL4519[,c(as.character(data$gpl[13])),drop = FALSE]



# GPL5421

GPL5421 <- Table(getGEO(data$gpl[14]))[,c("ID",as.character( data$symble[14]))]  #combination of ids and 
rownames(GPL5421) <- GPL5421$ID

colnames(GPL5421) <- c("ID",as.character(data$gpl[14]) )
head(GPL5421,4000)
GPL5421$GPL5421
GPL5421$GPL5421<- gsub("^Empty", '', GPL5421$GPL5421)  ## removinf Empty
GPL5421$GPL5421<- gsub("^Print-Buffer", '', GPL5421$GPL5421)  ## removinf Print-Buffer

GPL5421 <- GPL5421[,c(as.character(data$gpl[14])),drop = FALSE]



# GPL5429

GPL5429 <- Table(getGEO(data$gpl[15]))[,c("ID",as.character( data$symble[15]))]  #combination of ids and 
rownames(GPL5429) <- GPL5429$ID

colnames(GPL5429) <- c("ID",as.character(data$gpl[15]) )
head(GPL5429,4000)
GPL5429$GPL5429
GPL5429$GPL5429<- gsub("^Empty", '', GPL5429$GPL5429)  ## removinf Empty
GPL5429$GPL5429<- gsub("^Control", '', GPL5429$GPL5429)  ## removinf Control

GPL5429 <- GPL5429[,c(as.character(data$gpl[15])),drop = FALSE]


# GPL5754

GPL5754 <- Table(getGEO(data$gpl[16]))[,c("ID",as.character( data$symble[16]))]  #combination of ids and 
rownames(GPL5754) <- GPL5754$ID

colnames(GPL5754) <- c("ID",as.character(data$gpl[16]) )
head(GPL5754,4000)
GPL5754$GPL5754

GPL5754 <- GPL5754[,c(as.character(data$gpl[16])),drop = FALSE]


# GPL6059

GPL6059 <- Table(getGEO(data$gpl[17]))[,c("ID",as.character( data$symble[17]))]  #combination of ids and 
rownames(GPL6059) <- GPL6059$ID

colnames(GPL6059) <- c("ID",as.character(data$gpl[17]) )
head(GPL6059,4000)
GPL6059$GPL6059

GPL6059 <- GPL6059[,c(as.character(data$gpl[17])),drop = FALSE]


# GPL6291

GPL6291 <- Table(getGEO(data$gpl[18]))[,c("ID",as.character( data$symble[18]))]  #combination of ids and 
rownames(GPL6291) <- GPL6291$ID

colnames(GPL6291) <- c("ID",as.character(data$gpl[18]) )
head(GPL6291,4000)
GPL6291$GPL6291

GPL6291 <- GPL6291[,c(as.character(data$gpl[18])),drop = FALSE]

# GPL6687

GPL6687 <- Table(getGEO(data$gpl[19]))[,c("ID",as.character( data$symble[19]))]  #combination of ids and 
rownames(GPL6687) <- GPL6687$ID

colnames(GPL6687) <- c("ID",as.character(data$gpl[19]) )
head(GPL6687,4000)
GPL6687$GPL6687

GPL6687 <- GPL6687[,c(as.character(data$gpl[19])),drop = FALSE]


# GPL7307

GPL7307 <- Table(getGEO(data$gpl[20]))[,c("ID",as.character( data$symble[20]))]  #combination of ids and 
rownames(GPL7307) <- GPL7307$ID

colnames(GPL7307) <- c("ID",as.character(data$gpl[20]) )
head(GPL7307,4000)
unique(GPL7307$GPL7307)
GPL7307$GPL7307<- gsub("^unknown", '', GPL7307$GPL7307)  ## removinf unknown

GPL7307 <- GPL7307[,c(as.character(data$gpl[20])),drop = FALSE]


# GPL7445

GPL7445 <- Table(getGEO(data$gpl[21]))[,c("ID",as.character( data$symble[21]))]  #combination of ids and 
rownames(GPL7445) <- GPL7445$ID

colnames(GPL7445) <- c("ID",as.character(data$gpl[21]) )
head(GPL7445,4000)
GPL7445$GPL7445

GPL7445$GPL7445 <- sapply(GPL7445$GPL7445, function(x) strsplit(as.character(x), ",")[[1]][1])


ecoliUniprotAnotationData <- read.xlsx("Notes/AntibioticProject.xlsx",sheetIndex = 11)
head(ecoliUniprotAnotationData)
GPL7445$GPL7445[ GPL7445$GPL7445 %in% ecoliUniprotAnotationData$Ordered_locus_name]  <- as.character(ecoliUniprotAnotationData$synonyms)

GPL7445 <- GPL7445[,c(as.character(data$gpl[21])),drop = FALSE]


# GPL7822

GPL7822 <- Table(getGEO(data$gpl[22]))[,c("ID","GENE")]  #combination of ids and 
rownames(GPL7822) <- GPL7822$ID

colnames(GPL7822) <- c("ID",as.character(data$gpl[22]) )
head(GPL7822,4000)
GPL7822$GPL7822

GPL7822 <- GPL7822[,c(as.character(data$gpl[22])),drop = FALSE]





# GPL84

GPL84 <- Table(getGEO(data$gpl[23]))[,c("ID",as.character( data$symble[23]))]  #combination of ids and 
rownames(GPL84) <- GPL84$ID

colnames(GPL84) <- c("ID",as.character(data$gpl[23]) )
head(GPL84,4000)
GPL84$GPL84

GPL84$GPL84 <- sapply(GPL84$GPL84, function(x) strsplit(as.character(x), "///")[[1]][1])

GPL84 <- GPL84[,c(as.character(data$gpl[23])),drop = FALSE]

# GPL8523

GPL8523 <- Table(getGEO(data$gpl[24]))[,c("ID",as.character( data$symble[24]))]  #combination of ids and 
rownames(GPL8523) <- GPL8523$ID

colnames(GPL8523) <- c("ID",as.character(data$gpl[24]) )
head(GPL8523,4000)
GPL8523$GPL8523

GPL8523 <- GPL8523[,c(as.character(data$gpl[24])),drop = FALSE]

# GPL8559

GPL8559 <- Table(getGEO(data$gpl[25]))[,c("ID",as.character( data$symble[25]))]  #combination of ids and 
rownames(GPL8559) <- GPL8559$ID

colnames(GPL8559) <- c("ID",as.character(data$gpl[25]) )
head(GPL8559,4000)
GPL8559$GPL8559

GPL8559 <- GPL8559[,c(as.character(data$gpl[25])),drop = FALSE]

# GPL9024

GPL9024 <- Table(getGEO(data$gpl[26]))[,c("ID",as.character( data$symble[26]))]  #combination of ids and 
rownames(GPL9024) <- GPL9024$ID

colnames(GPL9024) <- c("ID",as.character(data$gpl[26]) )
head(GPL9024,4000)
GPL9024$GPL9024

GPL9024 <- GPL9024[,c(as.character(data$gpl[26])),drop = FALSE]

# GPL9459


GPL9459 <- Table(getGEO(data$gpl[27]))[,c("ID","DESCRIPTION")]  #combination of ids and 
head(Table(getGEO(data$gpl[27])))

rownames(GPL9459) <- GPL9459$ID

colnames(GPL9459) <- c("ID",as.character(data$gpl[27]) )
head(GPL9459,4000)
GPL9459$GPL9459
GPL9459$GPL9459 <- sapply(GPL9459$GPL9459, function(x) strsplit(as.character(x), ";")[[1]][1])

GPL9459 <- GPL9459[,c(as.character(data$gpl[27])),drop = FALSE]
# 



annotedData <- new.cbind(GPL10196,
          GPL10622,
          GPL10659,
          GPL1343,
          GPL1396,
          GPL1436,
          GPL199,
          GPL2820,
          GPL2821,
          GPL3154,
          GPL3503,
          GPL4324,
          GPL4519,
          GPL5421,
          GPL5429,
          GPL5754,
          GPL6059,
          GPL6291,
          GPL6687,
          GPL7307,
          GPL7445,
          GPL7822,
          GPL84,
          GPL8523,
          GPL8559,
          GPL9024,
          GPL9459)



write.csv(annotedData, file="Data/annotationusingGPL.csv")

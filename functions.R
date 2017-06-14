#author narumeena
#description antibiotic resistance on E.Coli
#descriptionScript function 


#cbind function to bind diffrent rows and diffrent rownames 

new.cbind <- function(...)
{
  input <- eval(substitute(list(...), env = parent.frame()))
  
  names.orig <- NULL
  nrows <- numeric()
  for (i in 1:length(input))
  {
    nrows[i] <- nrow(input[[i]])
    names.orig <- c(names.orig, colnames(input[[i]])) 
  }
  
  idx <- (1:length(input))[order(nrows, decreasing=T)]
  x <- NULL
  for (i in 1:length(input))
  {
    x <- c(x, rownames(input[[idx[i]]]))
  }
  
  r <- data.frame(row.names=unique(x))
  for (i in 1:length(input))
  {
    r <- cbind(r, data.frame(input[[i]][match(rownames(r), rownames(input[[i]])),]))
  }
  
  colnames(r) <- names.orig
  
  return(r)
}


#####end


#######Combining dataframes when the columns don’t match
rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}

####end 



###virtual array function 

virtualArrayExpressionSets <- function (all_expression_sets = FALSE, identifier = "SYMBOL", 
          covars = "Batch", collapse_fun = median, removeBatcheffect = "EB", 
          sampleinfo = FALSE, parallel = "BiocParallel", ...) 
{
  if (class(try(utils::win.version(), silent = T)) == "try-error" && 
      parallel == "multicore") {
    require(multicore)
    lapply = mclapply
  }
  loadedPackages <- loadedNamespaces()
  names(loadedPackages) <- loadedNamespaces()
  if (parallel == "BiocParallel") {
    if (try(installed.packages()["BiocParallel", 1], silent = T) == 
        "BiocParallel") {
      require(BiocParallel)
    }
    else {
      source("http://www.bioconductor.org/biocLite.R")
      biocContribUrl <- sapply(biocinstallRepos(), contrib.url)
      try(install.packages(as.character("BiocParallel"), 
                           method = "internal", dependencies = c("Depends", 
                                                                 "Imports"), repos = biocinstallRepos(), contriburl = biocContribUrl), 
          silent = T)
      require(BiocParallel)
    }
  }
  if (is.na(loadedPackages["BiocParallel"]) != TRUE) {
    register(MulticoreParam(verbose = T))
    lapply <- bplapply
  }
  if (class(all_expression_sets) != "character") {
    all_expression_sets <- sapply(X = ls(envir = .GlobalEnv), 
                                  FUN = function(x) {
                                    class(eval(as.symbol(x)))
                                  })
    all_expression_sets <- grep(all_expression_sets, pattern = "ExpressionSet", 
                                value = TRUE)
    all_expression_sets <- names(grep(all_expression_sets, 
                                      pattern = "ExpressionSet", value = TRUE))
  }
  all_expression_sets <- sapply(X = all_expression_sets, FUN = function(x) {
    as.symbol(x)
  }, USE.NAMES = TRUE)
  expsts <- lapply(all_expression_sets, virtualArrayBuildfData, 
                   collapse_fun = collapse_fun, identifier = identifier)
  sample_info <- virtualArrayBuildSampleInfo(all_expression_sets)
  rm(all_expression_sets)
  dtfrms <- lapply(expsts, virtualArrayBuildExprs)
  rm(expsts)
  names_dtfrms <- names(dtfrms)
  rownames_dtfrms <- lapply(dtfrms, FUN = function(X) {
    rownames(X)
  })
  names(rownames_dtfrms) <- names_dtfrms
  rm(names_dtfrms)
  message("Matching and merging all data sets. This could take some time...", 
          appendLF = TRUE)
  merged <- virtualArrayMergeRecurse(dtfrms, by = "identifier", 
                                     incomparables = NA)
  rm(dtfrms)
  rownames(merged) <- merged[, 1]
  merged <- merged[2:dim(merged)[2]]
  message("Size of expression matrix of whole dataset: ", dim(merged)[1], 
          " rows and ", dim(merged)[2], " columns.", appendLF = TRUE)
  rownames_dtfrms[["result"]] <- rownames(merged)
  merged <- new("ExpressionSet", exprs = as.matrix(merged))
  merged_norm <- normalize.ExpressionSet.quantiles(merged)
  sample_info <- as.data.frame(sample_info)
  sampleNames(merged_norm) <- sample_info[, 1]
  if (sampleinfo == "create" && length(covars) == 1) {
    write.table(sample_info, file = "sample_info.txt", sep = "\t", 
                row.names = FALSE)
    message("The file 'sample_info.txt' has been written to your current working directory. Please modify it appropriately!", 
            appendLF = TRUE)
    answer <- readline(prompt = "Did you modify sample_info.txt? [y] or [n] ")
    sample_info_old <- sample_info
    sample_info <- read.table(file = "sample_info.txt", sep = "\t", 
                              header = TRUE)
    if (identical(sample_info_old, sample_info) == TRUE) {
      warning("WARNING: You did not modify the created sample_info.txt file!\n", 
              call. = F)
    }
    rm(sample_info_old)
  }
  if (sampleinfo != FALSE && class(sampleinfo) == "character") {
    sample_info <- read.table(file = sampleinfo, sep = "\t", 
                              header = TRUE)
  }
  if (sampleinfo != FALSE && class(sampleinfo) == "data.frame") {
    sample_info <- sampleinfo
  }
  message("Using ", cat(colnames(sample_info[, covars]), sep = " and "), 
          "columns as information for batch effect removal.", appendLF = TRUE)
  merged_combat <- merged_norm
  if (removeBatcheffect == "EB") {
    exprs(merged_combat) <- virtualArrayComBat(expression_xls = exprs(merged_combat), 
                                               sample_info_file = sample_info, covariates = covars, 
                                               write = FALSE, prior.plots = FALSE, ...)
  }
  rownames(sample_info) <- sample_info[, 1]
  pData(merged_combat) <- as.data.frame(sample_info)
  annotation(merged_combat) <- "org.Hs.eg"
  if (removeBatcheffect == "QD") {
    merged_combat <- normalize.ExpressionSet.qd(merged_combat, 
                                                ...)
  }
  if (removeBatcheffect == "GQ") {
    merged_combat <- normalize.ExpressionSet.gq(merged_combat, 
                                                Batch = pData(merged_combat)[, covars[1]], ...)
  }
  if (removeBatcheffect == "NORDI") {
    merged_combat <- normalize.ExpressionSet.nordi(merged_combat, 
                                                   ...)
  }
  if (removeBatcheffect == "MRS") {
    normalize.ExpressionSet.mrs(merged_combat, Batch = pData(merged_combat)[, 
                                                                            covars[1]], ...)
  }
  if (removeBatcheffect == "MC") {
    normalize.ExpressionSet.mc(merged_combat, Batch = pData(merged_combat)[, 
                                                                           covars[1]], ...)
  }
  merged_combat_norm <- normalize.ExpressionSet.quantiles(merged_combat)
  message("The numbers of overlapping genes/identifiers between each pair of datasets and the final result were as follows:", 
          appendLF = TRUE)
  overlaps <- as.data.frame(calculateOverlaps(rownames_dtfrms), 
                            stringsAsFactors = F)
  print(overlaps)
  if (summary(overlaps[["vs.result"]] < 0.5)[3] != 0) {
    badDataset <- rownames(overlaps[overlaps[["vs.result"]] < 
                                      0.5, ])
    warning("Datasets", paste(badDataset, sep = " "), "show only little overlap with other datasets and/or result!", 
            call. = F)
  }
  return(merged_combat_norm)
}
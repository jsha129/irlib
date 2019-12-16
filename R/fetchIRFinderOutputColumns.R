#' @title Fetches selected columns from IRFinder output file and returns them as a list of matrices
#' @description IRFinder output file (IRFinder-IR-dir.txt or IRFinder-IR-nondir.txt) reports information for 21 parameters such as intron depth or IR ratio per sample. This function extracts one column at a time for all samples and returns them as a list of same length as \emph{colNum} argument.
#'
#' @param filePaths location of IRFinder output files from which to extract data from.
#' @param colNum which column numbers to extract. see \link{IRF_column_names}.
#' @param sampleNames column names for each matrix.
#'
#' @return
#' @export
#' @importFrom data.table fread

fetchIRFinderOutputColumns <- function(filePaths, colNum = 9, sampleNames = NA){
  ### NOTE: This function was named fetchIRFinderOutputColumn_fread3() in DESeq2 constructor.
  ## colNum must be numeric; throw an error if it is character
  if(is.character(colNum)){
    stop("Column numbers must be numeric")
  }

  ## check if all files exist
  fileStatus <- sapply(filePaths, file.exists)
  if(sum(!fileStatus) != 0){
    print("oo Following files were not found.")
    print(paste("--", filePaths[which(!fileStatus)]))
    stop()
  } else {
    message("All input files exist. Reading files now.")
    message("Note: intron depth, splice left, splice right and splice exact will be rounded to nearest integer")
  }

  ##
  returnObj <- NA
  if(c(21) %in% colNum) {
    a1 <- fetchIRFinderOutputColumn_fread2(filePaths, colNum = setdiff(colNum,c(21)), sampleNames)
    a2 <- fetchIRFinderOutputColumn_fread2(filePaths, colNum = 21, sampleNames)

    ## Note: somehow calling recode_irf_warnings from this function makes it super slow!
    # message("Re-coding IRFinder warnings. See IRF_WARNING_CODES()")
    # a2 <- recode_irf_warnings(a2)

    returnObj <- append(a1, a2)
  } else {
    returnObj <- fetchIRFinderOutputColumn_fread2(filePaths, colNum = colNum, sampleNames)
  }

  message("\nDONE---\nSummary:")
  print(paste("length of the list:", length(returnObj)))
  print(paste("Dimension of each matrix:", nrow(returnObj[[1]]), "x", ncol(returnObj[[1]])   ))
  print(paste("List names:", Reduce(function (x,y){paste(x,y, sep =", ")}, names(returnObj) )))
  message("First few rownames:")
  print(rownames(returnObj[[1]])[1:6])
  message("First few colnames:")
  print(colnames(returnObj[[1]])[1:6])

  return(returnObj)
}

### --------------------------------------------------
#' @title Fetches selected columns from IRFinder output file and returns them as a list of matrices
#' @description IRFinder output file (IRFinder-IR-dir.txt or IRFinder-IR-nondir.txt) reports information for 21 parameters such as intron depth or IR ratio per sample. This function extracts one column at a time for all samples and returns them as a list of same length as \emph{colNum} argument.
#'
#' @param filePaths location of IRFinder output files from which to extract data from.
#' @param colNum which column numbers to extract. see \link{IRF_column_names}.
#' @param sampleNames column names for each matrix.
#'
#' @return
#' @importFrom data.table fread
#' @keywords internal

fetchIRFinderOutputColumn_fread2 <- function(filePaths, colNum = 9, sampleNames = NA){
  # faster version, returna a list with names of IRF columnans and optional sample names
  # issue: if IRF_WArning column 21 is included, whole matrix becomes character instead of numeric because IRF_WArning is character
  # solved by appending column 21 separately to the list all.
  # NOT exported

  a <- fetchIRFinderOutputColumn_fread(filePaths, colNum)

  # dim(a) %>% print
  all <- lapply(colNum, function(selectCol){
    if(is.numeric(a) & selectCol %in% c(9, 17:19) ){
      a <- round(a)
    }
    temp <- a[, grep(paste0("V", selectCol), colnames(a))]
    rownames(temp) <- gsub(" ","", rownames(temp), fixed = T)
    if(!all(is.na(sampleNames))){
      colnames(temp) <- sampleNames
    }
    temp
  })
  irf_columns = IRF_column_names()
  names(all) <- irf_columns[colNum, "Name"]
  return(all)
}
### --------------------------------------------------
#' @title Fetches selected columns from IRFinder output file and returns them as a list of matrices
#' @description IRFinder output file (IRFinder-IR-dir.txt or IRFinder-IR-nondir.txt) reports information for 21 parameters such as intron depth or IR ratio per sample. This function extracts one column at a time for all samples and returns them as a list of same length as \emph{colNum} argument.
#'
#' @param filePaths location of IRFinder output files from which to extract data from.
#' @param colNum which column numbers to extract. see \link{IRF_column_names}.
#' @param sampleNames column names for each matrix.
#' @importFrom data.table fread
#'
#' @return
#' @keywords internal
fetchIRFinderOutputColumn_fread <- function(filePaths, colNum = 9){
  require(data.table)
  # extracts particular column from IRFinder output files and returns a matrix
  # irtest=read.table(filePaths[1])
  irtest=fread(filePaths[1], header =F)
  if (irtest[1,1]=="Chr"){irtest=irtest[-1,]}
  irnames=unname(apply(as.matrix(irtest),1,FUN=function(x){return(paste0(x[4],"/",x[1],":",x[2],"-",x[3],":",x[6]))}))
  # result <- list()
  result <- c()

  if(sum(!file.exists(filePaths)) == 0){
    n=1
    for (i in filePaths){
      if(n %% 10 == 0){print(paste("Processing file no ",n, "/", length(filePaths)))}
      # print(paste0("processing file ",n,"/", length(filePaths), " at ",i))
      irtab=fread(i, header = F)
      if (irtab[1,1]=="Chr"){irtab=irtab[-1,]}

      tmp1 <- as.matrix(as.vector(irtab[,..colNum])) # data.table likes '..' in front of columns
      rownames(tmp1) <- irnames
      # colnames(temp2) <- colNames

      result <- cbind(result,tmp1)
      n=n+1
    }

    # colnames(result) <- colNames
  }
  else{
    print(paste0("File missing: ",filePaths[which(!file.exists(filePaths) )]))
  }
  return(result)
}


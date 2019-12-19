#' @title Generates valide DESeq2 Object to identify differentially retained
#'   introns
#' @description This function generates DESeq2 object that will be passed to
#'   DESeq2 to identify differentially retained introns in biological conditions
#'   using GLM/linear models. Please note that raw intron depth and splice exact
#'   exported from IRFinder are used in calculating IR ratio using DESeq2. See \link{differential_IR} to calculate IR ratio.
#'
#' @param expDesign Valid experimental design to be passed to DESeq2
#' @param designFormula Valid design formula to be passed to DESeq2.
#' @param selectedIR a subset of IR as character vector for subsequent analysis.
#' @param mat_intronDepth matrix containing intron depth
#' @param mat_spliceExact matrix containing splice exact.
#'
#' @return DESeq2 object
#' @export
#' @import DESeq2


DESeqDataSetFromPreCalculatedMatrix <- function(expDesign,
                                                designFormula,
                                                selectedIR = NA,
                                                mat_intronDepth = NULL, mat_spliceExact = NULL ){
  # allows user to supply IR  and splice exact matrices
  require(DESeq2)

  ## validate input
  if(!any(grepl("SAMD11", rownames(mat_intronDepth)))){
    stop("Please ensure that matrix rownames contain irnames")
  } else {
    irnames  <- rownames(mat_intronDepth)
  }
  if(any(!is.na(selectedIR))  & class(selectedIR) == "logical") {
    stop("Please provide selected IR as characters, not logical vector.")
  }

  selectedIR <- gsub(" ", "", selectedIR)

  res <- mat_intronDepth
  libsz <- mat_spliceExact

  res.rd=round(res)
  libsz.rd=round(libsz)
  irnames <- gsub(" ", "", irnames)
  colnames(res.rd)=paste("intronDepth",as.vector(expDesign[,1]),sep=".")
  rownames(res.rd)=irnames
  colnames(libsz.rd)=paste("totalSplice",as.vector(expDesign[,1]),sep=".")
  rownames(libsz.rd)=irnames

  print("Data read successfully")

  if (any(!is.na(selectedIR))){
    selectRows <- match(selectedIR, irnames)
    print("Using user specified IRs for analysis")
    if(any(is.na(selectRows))){
      print(paste("--",sum(is.na(selectRows)), " NAs presents in supplied IR, removed."))
      selectRows  <- selectRows[which(!is.na(selectRows))]
    }
    res.rd <- res.rd[selectRows,]
    libsz.rd <- libsz.rd[selectRows, ]
    res <- res[selectRows, ]
    libsz <- libsz[selectRows, ]
    irnames <- irnames[selectRows]
  }

  # print(paste("Using", nrow(res.rd), "IRs for DESeq2 test"))
  # Rest

  ir=c(rep("IR",dim(expDesign)[1]),rep("Splice",dim(expDesign)[1]))
  group=rbind(expDesign,expDesign)
  group$IRFinder=ir
  group$IRFinder=factor(group$IRFinder,levels=c("Splice","IR"))

  counts.IRFinder=cbind(res.rd,libsz.rd)
  # if( sum(is.na(counts.IRFinder)) > 0 ){
  #   print("NAs replaced with 0")
  #   counts.IRFinder[is.na(counts.IRFinder)] <- 0 # replace NA with 0
  # }
  print("preparing data compatible to DESeq2")
  dd = DESeqDataSetFromMatrix(countData = counts.IRFinder, colData = group, design = designFormula)
  sizeFactors(dd)=rep(1,dim(group)[1])
  rownames(dd)=irnames
  sp=libsz
  final=list(dd,res,sp)
  names(final)=c("DESeq2Object","IntronDepth","SpliceDepth")
  return(final)
}

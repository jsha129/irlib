#' @title Identify differentially retained introns in biological conditions using DESeq2.
#' @description This function identifies differentially retained introns in biological condition based on contrast factor and  an DESeq2 object prepared using \link{DESeqDataSetFromPreCalculatedMatrix}.
#'
#'
#' @param dds dds object prepared using \link{DESeqDataSetFromPreCalculatedMatrix}.
#' @param contrast contrast for changing reference group etc. This must be compativle with DESeq2 format: Please use contrast with appropriate levels: Biological Condition, Reference level, Condition to compare to.
#'
#' @return
#' @export
#' @import tidyverse
differential_IR <- function(dds, contrast){
  ## same as calculateIRatio()
  ## -- calculates change in IR ratio (substraction) from dds DESeq2 object and contrast
  if( is.factor(contrast) & length(levels(contrast)) == 3 ){
    condition <- levels(contrast)[1]
    neg <- levels(contrast)[2] # ref
    pos <- levels(contrast)[3]

    # res <- results(dds, alpha = 0.05,
    #                contrast = contrast,
    #                independentFiltering = T,
    #                parallel = F)

    print("-- Calculating IR in positive cond")
    res.pos = results(dds,
                      name = paste0(condition, pos, ".IRFinderIR") )
    pos.IR_vs_Splice=2^res.pos$log2FoldChange
    IRratio.pos = pos.IR_vs_Splice/(1+pos.IR_vs_Splice)
    names(IRratio.pos) <- rownames(res.pos)

    print("-- Calculating IR in negative/ref cond")
    res.neg = results(dds,
                      name = paste0(condition, neg, ".IRFinderIR")   )
    neg.IR_vs_Splice=2^res.neg$log2FoldChange
    IRratio.neg = neg.IR_vs_Splice/(1+neg.IR_vs_Splice)
    names(IRratio.neg) <- rownames(res.neg)

    print("-- Calculating Delta IR")
    res.diff = results(dds,
                       contrast=list(paste0(condition, neg, ".IRFinderIR"),
                                     paste0(condition, pos, ".IRFinderIR") ),  parallel = T)

    IR.change = IRratio.pos - IRratio.neg
    resultDF <-  res.diff %>% as.data.frame %>%
      mutate(IR_Name = rownames(.), DeltaIR = IR.change) %>%
      select(IR_Name, colnames(res.diff), DeltaIR )

    # return(list(Contrast = contrast,
    #             res.pos  = res.pos,
    #             res.neg = res.neg,
    #             res.diff = res.diff,
    #             IR.change = IR.change,
    #             IR_ratio.pos = IRratio.pos,
    #             IR_ratio.neg = IRratio.neg,
    #             results = resultDF))

    return(resultDF)
  } else {
    stop("Please use contrast with appropriate levels: Contrast, Reference, Comparision")
  }

}

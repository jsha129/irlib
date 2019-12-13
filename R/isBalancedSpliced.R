#' @title  Returns a logical vector checking whether a retained intron is balanced splicing
#' @description Some retained introns have higher splice exact values but this single value does not tell whether it splicing or exon skipping. In normal splicing, splice left, splice right and splice exact are roughtly similar. However, exon skipping events results in uneven splice left and splice right reads, resulting their ratio swayed away from 1 that can be used to determine normal splicing versus exon skipping.
#' @param SL splice left matrix
#' @param SR splice right matrix
#' @param lowest lowest SL:SR ratio
#' @param highest highest SL:SR ratio
#'
#' @return logical vector
#' @export

isBalancedSpliced <- function(SL, SR, lowest= 0.8, highest=1){
  ratio <- (1+SL)/(1+SR) # 1 added to avoid division by 0.
  meanRatio <- apply(ratio, 1, mean, na.rm =T)
  between(meanRatio, lowest, highest)
}


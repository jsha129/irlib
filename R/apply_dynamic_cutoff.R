#' @title applies dynamic cutoff for selecting IR based on abundance of
#'   reference introns that varies according to sequencing depth.
#' @description Sequencing depth for sample could vary tremendously; therefore,
#'   some IR may not pass fixed filter such as  splice exact must be >50 in all
#'   samples purely due to low seq depth for some samples. To solve this,
#'   selection criteria to check abundance of IR (by splice exact or SL + SR)
#'   should be varied based on sequencing depth. This function attempts to
#'   correct bias in sequencing depth when selecting IR events by looking for
#'   stably spliced exons that act as reference genes as it is done in qPCR (see
#'   \emph{identify_ref_introns()}), and then vary IR selection criteria based
#'   on seq counts of stable reference exons. Please note that
#'   \emph{identify_ref_introns()} should be run prior to running this function
#'   to identify reference spliced exons.
#'
#' @param mat any value matrix, commonly splice exact or (splice left + splice right).
#' @param refIntrons reference introns/stably spliced exons identified from \emph{identify_ref_introns()}.
#' @param dynamicCutoff How abundant should IR be compared to refIntrons? default is 1000-times less abundant than refIntrons in each sample.
#' @param rowLogic logical operator to collapse 2D matrix into 1D. default is \emph{and}.
#' @param verbose T or F
#'
#' @return 1D vector of IR meeting dynamic cutoff.
#' @export

apply_dynamic_cutoff <- function(mat, refIntrons, dynamicCutoff = 1000, rowLogic = "and", verbose = F){
  if(length(refIntrons) == 1){
    stop("Please provide at least 2 reference introns")
  }
  matRefVals <- mat[refIntrons, ]
  #
  # matCutOffVals <- matrix(c(1), ncol = ncol(matRefVals) , nrow = 1 )
  # for(i in 1:nrow(matRefVals)){
  #   matCutOffVals <- matCutOffVals * matRefVals[i,]
  # }
  matCutOffVals <- sapply(1:nrow(matRefVals), function(i){
    matRefVals[i,]
  })
  matCutOffVals <- t(matCutOffVals)
  matCutOffVals <- as.numeric((matCutOffVals^(1/nrow(matRefVals)))/dynamicCutoff)
  names(matCutOffVals) <- colnames(mat)

  if(verbose){
    print("-- Reference introns --")
    print(refIntrons)
    print("-- Final cutoff value based on references --")
    print(matCutOffVals)
  }

  sampleList <- colnames(mat)
  temp <- lapply(sampleList, function(x){
    mat[, x]> matCutOffVals[x]
  } )
  temp2 <- Reduce(cbind, temp)
  if(rowLogic == "and"){
    return(apply(temp2, 1, all))
  } else if (rowLogic == "or"){
    return(apply(temp2, 1, any))
  } else {
    colnames(temp2) <- sampleList
    return(temp2)
  }

}

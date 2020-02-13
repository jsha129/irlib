#' @title Correlate gene expression with number of IR events in samples
#'
#' @param mat_gene_exp gene expression matrix in genes x sample format.
#' @param mat_IR 1D matrix of number of IR events in sample x num_IR_events format.
#'
#' @return
#' @export

corr_gene_exp_IR_events <- function(mat_gene_exp, mat_IR){
  if(ncol(mat_gene_exp) != nrow(mat_IR)){
    stop("ncol(mat_gene_exp) do not match nrow(mat_IR).")
  }

  lmModels <- lapply(1:nrow(mat_gene_exp), function(i){
    if(i %% 1000 == 0){
      print(paste("Calculating correlation for gene number",i))
    }

    # a <- lm(mat_gene_exp[i,] ~ mat_IR) # old, as in the package
    a <- lm(mat_IR ~ mat_gene_exp[i,])
    summary(a)
  } )
  lmModels_coeff <- t(sapply(lmModels, function(i){
    i$coefficients[2,]
  }))
  colnames(lmModels_coeff)[4] <- "pvalue"
  colnames(lmModels_coeff) <- gsub(" ", "_", paste0("lm_",colnames(lmModels_coeff)))

  lmModels_r.squared <- sapply(lmModels, function(i){
    i$r.squared
  })

  corr_pearson <- sapply(1:nrow(mat_gene_exp), function(i){
    cor(mat_gene_exp[i,], mat_IR, method = "pearson")
  } )

  corr_spearman <- sapply(1:nrow(mat_gene_exp), function(i){
    cor(mat_gene_exp[i,], mat_IR, method = "spearman")
  } )

  corr_kendall <- sapply(1:nrow(mat_gene_exp), function(i){
    cor(mat_gene_exp[i,], mat_IR, method = "kendall")
  } )

  returnDF <- data.frame(gene_id = rownames(mat_gene_exp),
                         lmModels_coeff,
                         lm_r_squared = lmModels_r.squared,
                         corr_pearson,
                         corr_spearman,
                         corr_kendall)
  return(returnDF)
}

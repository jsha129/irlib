#' plots raw IR, spliceLeft etc for a given IR.
#'
#' @param select_ir selecte_ir to plot. must be more than one AND match rownames or row number of all.
#' @param ... optional parameters that will be passed to boxplot().
#'
#' @return
#' @export

plotRawIR <- function(data, select_ir){
  if(length(select_ir) >= 2){
    stop("Please provide only one select_ir")
  }
  par(mfrow = c(1,2))
  for(i in select_ir){
    temp1 <- data.frame(SL = data[["spliceLeft"]][select_ir,],
                        SR = data[["spliceRight"]][select_ir,],
                        SE = data[["spliceExact"]][select_ir,],
                        ID = data[["intronDepth"]][select_ir,])
    temp2 <- data.frame(IR = data[["IR"]][select_ir,],
                        Cov = data[["coverage"]][select_ir,])
    boxplot(temp1, main = select_ir, cex.main = 0.8)
    boxplot(temp2, main = select_ir, cex.main = 0.8)
  }

}

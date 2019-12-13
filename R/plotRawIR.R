#' plots raw IR, spliceLeft etc for a given IR.
#'
#' @param select_ir selecte_ir to plot. must be more than one AND match rownames or row number of all.
#' @param ... optional parameters that will be passed to boxplot().
#'
#' @return
#' @export

plotRawIR <- function(select_ir, ...){
  if(length(select_ir) == 1){
    stop("Please provide more than one select_ir")
  }

  if(exists("all") & class(all) == "list"){
    par(mfrow = c(1,2))
    for(i in select_ir){
      temp1 <- data.frame(SL = all[["spliceLeft"]][select_ir,],
                          SR = all[["spliceRight"]][select_ir,],
                          SE = all[["spliceExact"]][select_ir,],
                          ID = all[["intronDepth"]][select_ir,])
      temp2 <- data.frame(IR = all[["IR"]][select_ir,],
                          Cov = all[["coverage"]][select_ir,])
      boxplot(temp1 , ...)
      boxplot(temp2, ...)
    }
  } else {
    stop("please define or load 'all' variable containing spliceleft etc")
  }

}

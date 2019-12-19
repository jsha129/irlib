#' Returns code for IRFinder warnings
#'
#' @return
#' @export
IRF_WARNING_CODES <- function(){
  temp <- data.frame(Code = 0:4,
                    IRF_warning = c("-", "LowCover", "LowSplicing",
                                    "MinorIsoform", "NonUniformIntronCover"))
  temp$IRF_warning <- as.character(temp$IRF_warning)
  return(temp)
}

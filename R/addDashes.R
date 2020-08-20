
#' @title addDashes (for formatting code)
#' @description Adds '### --' (50 '-') for making sections in code and general formatting. The function is called by Rstudio up on a hot key combinations.
#' @return
#' @export
#'

addDashes <- function(){
  rstudioapi::insertText(paste0("### ", Reduce(paste0, rep("-",50))))
}

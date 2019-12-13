#'
#' @title evaluates whether at least X proportion of samples in raw value matrix
#'   meets some threshhold value.
#' @description This function is useful when selecting whether 50%
#'   (\emph{min_proportion}) of samples (\emph{mat}) has value more than
#'   \emph{value}.
#' @param mat value matrix
#' @param value threshhold value to check in *mat*
#' @param min_proportion threshhold minimum proportions of samples (colnames) to
#'   higher than the value parameter
#' @param byrow whether to calculate min_proportion by row
#'
#' @return
#' @export
#'

checkMinProp <- function(mat, value, min_proportion , byrow = T){
  if(byrow){
    temp <- apply(mat, 1, function(i){
      sum(i > value)/length(i) >= min_proportion
    })
  } else {
    temp <- apply(mat, 2, function(i){
      sum(i > value)/length(i) >= min_proportion
    })
  }
  return(temp)
}

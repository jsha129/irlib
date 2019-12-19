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

checkMinProp <- function(mat, min_proportion, value, byrow = T){
  if(min_proportion > 1 | min_proportion < 0){
    stop("min_proportion must be between 0 and 1")
  }
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

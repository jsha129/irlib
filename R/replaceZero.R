#' replaces 0s in a matrix with lowest value.
#'
#' @param temp input matrix
#'
#' @return matrix
#' @export
#'
replaceZero <- function(temp){
  temp2 <- as.vector(temp)
  replacement <- min(temp2[temp2 != 0])
  return(temp)
}


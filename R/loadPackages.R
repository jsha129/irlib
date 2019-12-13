#' Loads multiple packages if already installed
#'
#' @param packages packages to load as character vector.
#'
#' @return Null or errors showing missing packages
#' @export
loadPackages <- function(packages){
  a = installed.packages()
  a = rownames(a)
  missing_packages = packages[!(packages %in% a)]

  if(length(missing_packages) == 0){
    temp = sapply(packages, require, character.only = T, quietly = T)
  } else{
    stop(paste("Please install following packages: ", Reduce(function(x, y){paste(x, y, sep =", ")}, missing_packages) ))
  }
}








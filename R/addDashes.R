
#' @title addDashes (for formatting code)
#' @description Adds '### --' (50 '-') for making sections in code and general formatting. The function is called by Rstudio up on a hot key combinations.
#' @return
#' @export
#'

addDashes <- function(){
  rstudioapi::insertText(paste0("### ", Reduce(paste0, rep("-",50))))
}

#' @title addHashes (for formatting code)
#' @description Adds '###' (54 times) for making sections in code and general formatting. The function is called by Rstudio up on a hot key combinations.
#' @return
#' @export
#'

addHashes <- function(){
  rstudioapi::insertText(paste0("###", Reduce(paste0, rep("#",51))))
}


## add for addins shortcut

## create in the libPaths path where the package is installed
## create rstudio/addins.dcf
# Name: Inserts dashes for comments
# Description: Inserts `### ---` at the cursor position.
# Binding: addDashes
# Interactive: true


# Name: Inserts dashes for comments
# Description: Inserts `####` (54 times) at the cursor position.
# Binding: addHashes
# Interactive: true


## now define shortcuts


#' @title Extracts gene symbols or id from IRFinder output names
#'
#' @param ir A vector of IR names exported from IRFinder in the format: symbol/EnsemblID/intron coordinates/strand.
#' @param returnGeneSymbols whether to return gene symbols (names, field 1) or gene id (field 2).
#' @return
#' @import purrr
#' @import tidyverse
#' @export
get_IR_host_genes <- function(ir, returnGeneSymbols = F){
  require(purrr)
  strsplit(ir, "/") %>%
    map_chr(., ifelse(returnGeneSymbols, 1, 2 )) %>%
    unique
}

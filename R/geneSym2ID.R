#' @title Convers gene symbol to ensembl id.
#'
#' @param symbols gene symbols to be converted to Ensembl IDs.
#' @return
#' @import org.Hs.eg.db
#' @export
geneSym2ID <- function(symbols){
  # returns ENSEMBL id from gene symbols
  library('org.Hs.eg.db')
  mapIds(org.Hs.eg.db, symbols, 'ENSEMBL', 'SYMBOL', multiVals= "first")
}

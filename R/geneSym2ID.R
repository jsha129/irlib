#' Convers gene symbol to ensembl id.
#' @return
#' @import org.Hs.eg.db
#' @export
geneSym2ID <- function(symbols){
  # returns ENSEMBL id from gene symbols
  library('org.Hs.eg.db')
  mapIds(org.Hs.eg.db, symbols, 'ENSEMBL', 'SYMBOL', multiVals= "first")
}
geneID2Sym <- function(ids){
  ## returns gene sybmols bases on ENSEMBL id
  library('org.Hs.eg.db')
  mapIds(org.Hs.eg.db, ids, 'SYMBOL', 'ENSEMBL',multiVals= "first")
}

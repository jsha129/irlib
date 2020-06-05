#' @title Convers ensembl id to gene symbol.
#' @param ids Ensembl ids to be converted to gene symbols
#' @return
#' @import org.Hs.eg.db
#' @export
geneID2Sym <- function(ids){
  ## returns gene sybmols bases on ENSEMBL id
  library('org.Hs.eg.db')
  mapIds(org.Hs.eg.db, ids, 'SYMBOL', 'ENSEMBL',multiVals= "first")
}

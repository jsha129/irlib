#' Convers ensembl id to gene symbol.
#' @return
#' @import org.Hs.eg.db
#' @export
geneID2Sym <- function(ids){
  ## returns gene sybmols bases on ENSEMBL id
  library('org.Hs.eg.db')
  mapIds(org.Hs.eg.db, ids, 'SYMBOL', 'ENSEMBL',multiVals= "first")
}

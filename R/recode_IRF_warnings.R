#' Recodes IRF warning as numeric values. Recoding does not make a difference in memory storage.
#'
#' @param mat matrix containing IRF warnings
#'
#' @return
#' @export
#'
recode_IRF_warnings <- function(mat){
  rows <- rownames(mat)
  cols <- colnames(mat)
  irf_warnings <- IRF_WARNING_CODES()
  for(i in 1:nrow(irf_warnings)){
    mat[mat == irf_warnings[i, "IRF_warning"] ] <- irf_warnings[i, "Code"]
  }
  mat <- apply(mat, 2, as.numeric)
  rownames(mat) <- rows
  colnames(mat) <- cols
  mat
}

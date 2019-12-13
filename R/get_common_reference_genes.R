#' returns a list of common reference genes used in identifying stably spliced exons.
#' @return
#' @export
get_common_reference_genes <- function(){
  return(c("GAPDH", "ACTB", "HMBS", "B2M", "TUB", "PPIA", "RPL13A", "UBC",
           "SDHA", "TOP1", "CYC1", "ATP5B"))
}

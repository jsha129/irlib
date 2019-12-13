#' @title Extracts genomic loci from irnames
#' @description Extracts genomic loci from irnames: ESPN/ENSG00000187017/anti-near/1:  6425249-  6428225:+ to 'chr1:6425249-6428225'
#'
#' @param irnames output of IRFinder in 'ESPN/ENSG00000187017/anti-near/1:  6425249-  6428225:+' format
#'
#' @return
#' importFrom purrr map_chr
#' @export
extractGenomicCoord <- function(irnames){
  irnames <- as.character(irnames)
  coord_rough <- strsplit(map_chr(strsplit(irnames, "/"), 4), ":") # 1:  6425249-  6428225
  # temp2 <- strsplit(map_chr(coord_rough,), ":")
  subset2 <-  paste0("chr",map_chr(coord_rough, 1), ":", map_chr(coord_rough, 2))
  return( gsub(" ", "", subset2) )
}

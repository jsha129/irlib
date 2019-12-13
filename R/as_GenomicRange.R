### --------------------------------------------------
#' converts 'chrX:23232-23232' or IRfinder output rowname to GRange
#'
#' @param coord coord to be converted to genomic ranges
#'
#' @return
#' @importFrom GenomicRanges GRanges
#' @importFrom purrr map_chr
#' @export
#'
#' @examples
#' as.GenomicRange("chrX:23232-23240")
as.GenomicRange <- function(coord){
  require(GenomicRanges)
  require(purrr)
  if(all(tolower(substr(coord ,1,3)) == "chr")) {
    return(as(coord, "GRanges"))
  } else if ( all(strsplit(coord, "/") %>% sapply(., length) == 4) ) {
    tempIRnameSplit <- coord   %>% strsplit(., "/")
    return(as(map_chr(tempIRnameSplit, 4), "GRanges"))
  } else{
    stop("Incorrect format for coord.")
  }
}


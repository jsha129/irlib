#' Returns length of introns in nucleotides
#'
#' @param irnames rownames of IRFinder output, eg 'HES3/ENSG00000173673/anti-near/1:6244241-6244350:+'.
#'
#' @return
#' @export
#' @importFrom GenomicRanges width
#'
intron_length <- function(irnames){
  GenomicRanges::width(as.GenomicRange(irnames))
}

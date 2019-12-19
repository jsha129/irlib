#' Returns type of intron retention event (aka clean or anti-near etc; 3rd field of IRFinder rownames)
#'
#' @param irnames rownames of IRFinder output, eg 'HES3/ENSG00000173673/anti-near/1:6244241-6244350:+'.
#'
#' @return
#' @export
#' @importFrom purrr map_chr
#'
intron_type <- function(irnames){
  temp <- strsplit(irnames, "/")
  purrr::map_chr(temp, 3)
}

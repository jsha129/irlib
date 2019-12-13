#' calculates width/length of genomic coordinates
#'
#' @param coord genomic coord in format: 'chr11:65083723-65083932'
#' @param unit NA or kb
#'
#' @return intron widths
#' @importFrom  purrr map_chr
#' @export

calculateCoordWidth <- function(coord, unit = "kb"){
  temp <- map_chr(strsplit(coord, ":"),2)
  start <- as.numeric(map_chr(strsplit(temp, "-"), 1))
  end <- as.numeric (map_chr(strsplit(temp, "-"), 2))
  intronlength <- abs(start - end)
  if (unit == "kb"){
    intronlength <- 1E-3*intronlength
  } else if (unit == "Mb") {
    intronlength <- 1E-6*intronlength
  }

  return(intronlength)
}

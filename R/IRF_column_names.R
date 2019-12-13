#' Lists names of IRfinder columns
#'
#' @return
#' @export

IRF_column_names <- function(){
  temp <- c("Chr",
            "Start",
            "End",
            "Gene_Symbol",
            "Null",
            "Strand",
            "ExcludedBases",
            "coverage",
            "intronDepth",
            "Intron_25_Percentile",
            "Intron_50_Percentile",
            "Intron_75_Percentile",
            "ExonToIntronReadsLeft",
            "ExonToIntronReadsRight",
            "IntronDepth_first_50bp",
            "IntronDepth_last_50bp",
            "spliceLeft",
            "spliceRight",
            "spliceExact",
            "IR",
            "IRF_warnings")
  temp = data.frame(ColNum = 1:length(temp), Name = temp)
  return(temp)
}

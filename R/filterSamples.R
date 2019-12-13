#' @title Filters IR events based on flexible selection criteria.
#' @description Flexible function that applies a selection criteria to each
#'   sample in the matrix for filtering IR. \emph{criteria} is directly
#'   evaluated by R; therefore, it must be a valid expression. \emph{criteria}
#'   \strong{must} contain a dummy variable \emph{x} that is used a placeholder for iteration.
#'
#' @param criteria  Text of of the criteria to be
#'   applied on each sample in SampleList, termed '\emph{x}' in the criteria.
#' @param sampleList samples to apply the criteria to. A character vector.
#' @param rowLogic how to collapse 2D matrix into 1D: by \emph{and} or \emph{or}
#'   operator. Alternatively, \emph{none} will return raw logical matrix with
#'   same dimension as sampleList.
#'
#' @return 1D or 2D logical vector representing samples passing the filtering criteria.
#' @export
#'
#' @examples
#' \dontrun{
#' criteria <- "all[[\"IR\"]][, x] > 0.1" # [,x] will be replaced by each item of sampleList.
#' sampleList <- colnames(all[["IR"]]) # for all samples in matrix containing IR values, all[["IR"]].
#'
#' filtered_ir <- filterSamples(criteria, samples, "and" )
#'
#' ## above command will return IR events (rows in all variable) that have value of >0.1 in all samples provided.
#'
#' }
#'
#'
filterSamples <- function(criteria, sampleList, rowLogic = "and"){
  ##@@ all variables must be present already
  temp <- lapply(sampleList, function(x){
    eval(parse(text = criteria))
  } )
  temp2 <- Reduce(cbind, temp)
  if(rowLogic == "and"){
    return(apply(temp2, 1, all))
  } else if (rowLogic == "or"){
    return(apply(temp2, 1, any))
  } else {
    colnames(temp2) <- sampleList
    return(temp2)
  }
}

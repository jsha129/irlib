#'@title Applies flexible IR selection criteria to each sample in each
#'  biological condition.
#'@description \link{filterSamples()} that applies selection criteria to each
#'  sample regardless of their biological
#'  conditions/grouping.\link{filterSamplesByGroups()} splits samples by their
#'  biological conditions, applies \link{filterSamples()} on each group and
#'  combines output into a 1D vector. For example, this function is helpful when one wants to filter IR that has splice exact greater than 100 in all samples in at least one group. \emph{criteria} is directly evaluated by R; therefore, it must be a valid
#'  expression. \emph{criteria} \strong{must} contain a dummy variable \emph{x}
#'  that is used a placeholder for iteration.
#'
#'@param data something similar to the mighty \emph{all} variable containing
#'  list of IRFinder columns.
#'@param criteria  Text of of the criteria to be applied on each sample in
#'  SampleList, termed '\emph{x}' in the criteria. \emph{criteria} is directly evaluated by R; therefore, it must be a valid
#'  expression and \strong{must} contain a dummy variable \emph{x} that is used a placeholder for iteration. In addition, criteria must contain \emph{all} as matrix name.
#'
#'@param groups a list with each element representing biological groups (as name of the element) and samples belonging to that group.
#'
#'@param intraGroupMergeLogic Specifies how to merge all samples belonging to one group should be collapsed into 1D vector: by \emph{and} or \emph{or}
#'  operator.
#'
#'@param  minPassingGroups Minimum number of biological conditions/groups satisfying the criteria.
#'
#'@return 1D or 2D logical vector representing samples passing the filtering
#'  criteria.
#'@export
#'
#' @examples
#' \dontrun{
#' expDesign <- data.frame(Sample = c(paste("Cancer", 1:3, sep ="_"), paste("Normal", 1:3, sep = "_") ) ,
#' Condition = rep(c("Cancer", "Normal"), each = 3))
#' expDesign$Condition <- factor(expDesign$Condition, levels = c("Normal", "Cancer"))

#' groups <- split(expDesign$Sample, expDesign$Condition)
#' groups

#' criteria <- "all[[\"IR\"]][, x] > 0.1" # [,x] will be replaced by each sample

#' ##  minPassingGroups = 1. All samples in at least one group should satisfy the filtering criteria.
#' filtered_ir <- filterSamplesByGroups(all, criteria = criteria, groups = groups, minPassingGroups = 1)
#' table(filtered_ir)
#' }

filterSamplesByGroups <- function(all, criteria, groups, intraGroupMergeLogic = "and", minPassingGroups = 1){
  if(minPassingGroups > length(groups)){
    stop("Mininum number of groups must be smaller than total number groups")
  }

  groupResults <- sapply(groups, function(i){
    sampleList <- i
    # print(sampleList)
    temp <- lapply(sampleList, function(x){
      eval(parse(text = criteria))
    } )
    temp2 <- Reduce(cbind, temp)
    if(intraGroupMergeLogic == "and"){
      return(apply(temp2, 1, all))
    } else if (intraGroupMergeLogic == "or"){
      return(apply(temp2, 1, any))
    } else {
      colnames(temp2) <- sampleList
      return(temp2)
    }
  })

  # calculating how many IR satisfy the criteria in all groups
  groupResults_rowSums <- rowSums(groupResults)
  return(groupResults_rowSums >= minPassingGroups)
}

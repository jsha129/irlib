#' returns values of key columns of the IRFinder output for selected samples and IRs.
#'
#' @param data something similar to the mighty \emph{all} variable containing list of IRFinder columns.
#' @param ir_index index values of IRs to look
#' @param selectSamples samples
#' @param columns which IRFinder output columns to report
#'
#' @return
#' @import dplyr
#' @export
diagnose_IRF_output <- function(data, ir_index, selectSamples, columns = colnames(data)){
  # print(columns)
  if(length(ir_index) == 1){
    stop("Please provide at least 2 ir_index.")
  }
  if(class(ir_index) == "character"){
    # get index of ir_index
    all_ir_names <- rownames(data[[1]])
    ir_index <- sapply(ir_index, function(i){ match(i, all_ir_names) })
  }

  temp2 <- lapply(selectSamples, function(selectSample){
    temp <- lapply(columns, function(i){
      temp2 <- data[[i]]
      temp2[ir_index,selectSample] %>% as.data.frame
    })
    temp <- Reduce(cbind, temp)
    rownames(temp) <- gsub(" ","" , rownames(temp))
    colnames(temp) <- paste(selectSample, columns, sep="_")
    temp
  })
  temp2 <- Reduce(cbind, temp2)
  temp2$IR_Type <- rownames(temp2) %>% strsplit(., "/") %>% purrr::map_chr(., 3)
  temp2$IR_Name <- rownames(temp2)
  temp2 <- temp2 %>% dplyr::select("IR_Name", "IR_Type", seq(1, ncol(temp2) -2, 1))
  rownames(temp2) <- NULL
  temp2
}

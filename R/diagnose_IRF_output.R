#' returns values of key columns of the IRFinder output for selected samples and IRs.
#'
#' @param all the mighty \emph{all} variable containing list of IRFinder columns.
#' @param ir_index index values of IRs to look
#' @param selectSamples samples
#'
#' @return
#' @import dplyr
#' @export
diagnose_IRF_output <- function(all,ir_index, selectSamples){
  temp2 <- lapply(selectSamples, function(selectSample){
    temp <- lapply(1:length(all), function(i){
      temp2 <- all[[i]]
      temp2[ir_index,selectSample] %>% as.data.frame
    })
    temp <- Reduce(cbind, temp)
    rownames(temp) <- gsub(" ","" , rownames(temp))
    colnames(temp) <- paste(selectSample, names(all), sep="_")
    temp
  })
  temp2 <- Reduce(cbind, temp2)
  temp2$IR_Type <- rownames(temp2) %>% strsplit(., "/") %>% purrr::map_chr(., 3)
  temp2$IR_Name <- rownames(temp2)
  temp2 <- temp2 %>% dplyr::select("IR_Name", "IR_Type", seq(1, ncol(temp2) -2, 1))
  rownames(temp2) <- NULL
  temp2
}

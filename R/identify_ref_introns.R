#' @title identifies most stably spliced exons (reference for introns) in reference
#'   genes.
#' @description    identifies most stably spliced exons (reference for introns) in reference
#'   genes. This information is then used to calculate theshhold in splice exact
#'   etc for abundance, ie retained introns should be expressed at 1000th of
#'   whatever expression of GAPDH (most stable reference intron) is. The algorithm used in identifying stable exons is similar to geNorm. Most
#'   genes have multiple exons, so only exons with highest eplice exacts are
#'   selected. The method calculates distance of the most abundant exon for each
#'   reference gene from the geomatric mean of all reference exons and then
#'   looks at which exon shows least variation from geomatric mean in all
#'   samples.
#'
#' @param se value matrix. could be splice exact or (splice left + splice right).
#' @param ref_genes character vector of reference genes
#' @param multiRef whether to consider multiple reference genes. User should first run the function without multiRef, select exons that are stable and then supply them as a list.
#'
#' @return list of stable intron names and their average counts in the data set
#' @importFrom reshape2 melt
#' @importFrom plyr ldply
#' @import ggplot2
#' @import ggpubr
#' @export
#'

identify_ref_introns <- function(se, ref_genes, multiRef = NA){
  ## haven't tested multiple reference genes
  colnames(se) <- 1:ncol(se) # to avoid long sample names
  rownames(se) <-  gsub(" ", "",rownames(se))

  rawSE <- lapply(ref_genes, function(i){
    se[grep(paste0("^",i,"/"), rownames(se)), ]
  })
  names(rawSE) <- ref_genes

  ## which se has highest sum in all samples
  highestSE <- sapply(rawSE, function(i){
    a <- apply(i, 1, sum)
    names(a)[ which(a == max(a))][1]
  } )

  #@ multiple introns/gene. use max SE from all introns/ref gene.
  max_se_intron <- lapply(1:length(rawSE), function(i){
    rawSE[[i]][highestSE[[i]], ]
  })

  names(max_se_intron) <- ref_genes # highestSE  #ref_genes

  checkMultiRef <- sapply(multiRef, function(i){
    all(i %in% ref_genes)
  })
  if(! all(is.na(multiRef)) & all(checkMultiRef)){
    listMultiRef <- lapply(multiRef, function(geneList){
      matMultiRef <- (Reduce("*", lapply(geneList, function(i) max_se_intron[[i]] )))^(1/length(geneList))
      matMultiRef
    })
    names(listMultiRef) <-sapply(multiRef, function(i){
      return(Reduce(paste, i))
    }) %>% gsub(" ", "+", . , fixed = T)
    max_se_intron <- append(max_se_intron, listMultiRef)
  }

  #@ converting list to matrix
  df_max_se <- plyr::ldply(max_se_intron)
  mat_max_se <- df_max_se[, 2:ncol(df_max_se)] %>% as.matrix
  rownames(mat_max_se) <- df_max_se$.id

  mat_log_max_se <- log10(mat_max_se + 1)
  # mat_log_max_se <- mat_max_se
  #@ Geometric mean of ref genes in each sample
  geo_mean <- apply(mat_log_max_se, 2, mean)

  mat_dist_geo_mean <- apply(mat_log_max_se, 2, function(i){
    i - mean(i)
  })
  mat_var_ref_genes <- apply(mat_dist_geo_mean, 1 , var) %>% sort


  ### Graphs
  ## 1. log10 expression of all ref genes/sample
  df_log_max_se <- mat_log_max_se %>% as.data.frame %>% mutate(Gene = rownames(.))
  df_log_max_se_long<- melt(data = df_log_max_se)
  colnames(df_log_max_se_long)[2] <- "Sample"
  p_log_val  <- ggplot(df_log_max_se_long, aes(x = Sample, y = value, colour = Gene)) +
    # geom_point() +
    geom_line(aes(group = Gene))+
    labs(title="Log exp", x = "Sample", y="Log10 (reads + 1) ") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # p_log_val

  #@ 2. Distance from Geo Mean
  df_dist_geo_mean <- mat_dist_geo_mean %>% as.data.frame %>% mutate(Gene = rownames(.))
  df_dist_geo_mean_long <- melt(data = df_dist_geo_mean)
  colnames(df_dist_geo_mean_long)[2] <- "Sample"
  p_dist  <- ggplot(df_dist_geo_mean_long, aes(x = Sample, y = value, colour = Gene)) +
    geom_line(aes(group = Gene))+
    # geom_point() +
    geom_hline(yintercept = 0, lty = 2, col = "black", lwd = 1)+
    labs(title="Distance from Geo Mean", x = "Sample", y="Distance ") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # p_dist


  ## 3. Variance
  df_var_ref_genes <- mat_var_ref_genes %>% as.data.frame
  colnames(df_var_ref_genes)[1] <- "Variance"
  df_var_ref_genes$Gene <- names(mat_var_ref_genes)

  df_var_ref_genes$Gene <- factor(df_var_ref_genes$Gene,
                                  levels = mat_var_ref_genes %>% names)
  p_variance <- ggplot(df_var_ref_genes, aes(x = Gene, y = Variance)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # p_variance


  #@ 4. expression of each ref gene in all samples
  df_log_max_se_long$Gene <- factor(df_log_max_se_long$Gene,
                                    levels = names(mat_var_ref_genes))
  p_gene  <- ggplot(df_log_max_se_long, aes(x = Gene, y = value)) +
    geom_boxplot() +
    geom_jitter(alpha = 0.3, width = 0.25) +
    labs(title="Expression of each ref in all samples", x = "Gene", y="Log10 (reads + 1) ") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # p_gene

  # ggpubr::ggarrange(p_log_val,p_dist, p_variance,p_gene)
  lapply(list(p_log_val,p_dist, p_variance,p_gene), function(i) print(i))

  # print("-- Stable introns in order --")
  # print(highestSE[match(names(mat_var_ref_genes), highestSE %>% strsplit(., "/") %>% map_chr(.,1))])

  # print("-- median value --")
  # print(10^tapply(df_log_max_se_long$value, df_log_max_se_long$Gene, mean) )
  return(list(stableIntrons = highestSE[match(names(mat_var_ref_genes), highestSE %>% strsplit(., "/") %>% map_chr(.,1))],
              meanSeqCounts = 10^tapply(df_log_max_se_long$value, df_log_max_se_long$Gene, mean) ))

}



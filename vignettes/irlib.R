## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )

## ----setup--------------------------------------------------------------------
library(irlib)
library(tidyverse)

options(stringsAsFactors = F)

## Preperation
IRF_OUTPUT_DIR <- file.path("~/Downloads/irlib_data/") 
SAMPLES <- list.dirs(IRF_OUTPUT_DIR, recursive = F, full.names = F)
IRF_OUTPUT_FILENAME <- "IRFinder-IR-nondir.txt"

filepaths <- file.path(IRF_OUTPUT_DIR, SAMPLES, IRF_OUTPUT_FILENAME)

## -----------------------------------------------------------------------------
# avaiable columns:
print(IRF_column_names())

## which columns to extract
extractCol <- c(8, 9, 17, 18, 19,20, 21)
## Reading data, commented out 
# all <- fetchIRFinderOutputColumns(filepaths, colNum = extractCol, SAMPLES)

## loading saved data
data("all")



## -----------------------------------------------------------------------------
irnames <- rownames(all[["IR"]])
irLengths <- intron_length(irnames)
irTypes <- intron_type(irnames)
table(irTypes)


## -----------------------------------------------------------------------------
expDesign <- data.frame(Sample = c(paste("Cancer", 1:3, sep ="_"), paste("Normal", 1:3, sep = "_") ) ,
                        Condition = rep(c("Cancer", "Normal"), each = 3)) 
expDesign
str(expDesign)
expDesign$Condition <- factor(expDesign$Condition,
                              levels = c("Normal", "Cancer"))


## -----------------------------------------------------------------------------
filtered_ir <- rowMeans(all[["spliceExact"]]) > 50 & rowMeans(all[["IR"]]) > 0.1
table(filtered_ir)

## seeing which IRs match the criteria.
irnames[filtered_ir] %>% head

## -----------------------------------------------------------------------------
## Filtering IR where at least 50% of samples have value of at least 0.1.
filtered_ir <- checkMinProp(mat = all[["IR"]], min_proportion = 0.5, value = 0.1)
table(filtered_ir)

### --------------------------------------------------
## Checking values for multiple conditions/matrices
### --------------------------------------------------
## Filtering IR where 
## (1) at least 50% of samples have value of IR greater than 0.1 AND
## (2) at least 70% of samples have minimum coverage greater than 0.8 AND 
## (3) at least 50% of samples have sum of SL and SR greater than 100

filtered_ir <- checkMinProp(mat = all[["IR"]], min_proportion = 0.5, value = 0.1) &
  checkMinProp(mat = all[["coverage"]], min_proportion = 0.7, value = 0.8) &
  checkMinProp(mat = (all[["spliceLeft"]] + all[["spliceRight"]]), min_proportion = 0.5, value = 100) 
table(filtered_ir)


## -----------------------------------------------------------------------------
## Filtering IR where each sample have IR value of at least 0.1.
criteria <- "all[[\"IR\"]][, x] > 0.1" # [,x] will be replaced by each sample

filtered_ir <- filterSamples(all = all, criteria) 
table(filtered_ir)


## -----------------------------------------------------------------------------

### --------------------------------------------------
## Checking values for multiple conditions/matrices
### --------------------------------------------------
## Filtering IR where each IR has:
## (1) IR > 0.01 in all samples AND 
## (2) sum of SL and SR greater than 100 in all samples AND
## (3) no warning from IRFinder in all samples.

criteria <- "all[[\"IR\"]][, x] > 0.01 & 
(all[[\"spliceLeft\"]][,x] + all[[\"spliceRight\"]][,x]) > 100 &
all[[\"IRF_warnings\"]] == \"-\""

filtered_ir <- filterSamples(all = all, criteria) 
table(filtered_ir)

## -----------------------------------------------------------------------------
groups <- split(expDesign$Sample, expDesign$Condition)
groups

criteria <- "all[[\"IR\"]][, x] > 0.1" # [,x] will be replaced by each sample

filtered_ir <- filterSamplesByGroups(all = all, 
                                     criteria = criteria,
                      groups = groups, 
                      minPassingGroups = 1)
table(filtered_ir)



## -----------------------------------------------------------------------------
groups <- split(expDesign$Sample, expDesign$Condition)
groups

## 1. IR ratio in all samples belonging to at least one group is greater than 0.01
criteria1 <- "all[[\"IR\"]][, x] > 0.01" # [,x] will be replaced by each sample
filtered_ir1 <- filterSamplesByGroups(all = all, criteria = criteria1,
                      groups = groups, 
                      minPassingGroups = 1)
table(filtered_ir1)

## 2. Average splice exact in all samples is greater than 100 AND
filtered_ir2 <- rowMeans(all[["spliceExact"]]) > 100
table(filtered_ir2)

## 3. No IRF warning in all samples.
criteria3 <- "all[[\"IRF_warnings\"]][, x] == \"-\" | all[[\"IRF_warnings\"]][, x] == \"LowCover\" " 
filtered_ir3 <- filterSamples(all = all, criteria3)
table(filtered_ir3)

###### combining all three filters
filtered_ir <- filtered_ir1 & filtered_ir2 & filtered_ir3
table(filtered_ir)



## -----------------------------------------------------------------------------
## recoding IRF warnings as integer using recode_IRF_warnings() and pre-programmed IRF_WARNING_CODES()
# frequency table of IRF warnings
table(all[["IRF_warnings"]])

## recoding IRF warnings as integer
print(IRF_WARNING_CODES()) 

irf_recoded <- recode_IRF_warnings(all[["IRF_warnings"]])
table(irf_recoded) # note that this frequency table is identical to uncoded IRF warnings



## -----------------------------------------------------------------------------
## get IRFinder values for selected IRs.
diagnose_IRF_output(data = all, 
                    which(filtered_ir)[1:2], 
                    c("Cancer_1", "Normal_1"), 
                    columns = c("coverage", "IR") )

## -----------------------------------------------------------------------------
## Visualise IRFinder values for selected IR, one IR at a time. 
plotRawIR(all,"TNFRSF14/ENSG00000157873/clean/1:2558468-2559822:+")

## Use a loop to visualise multiple IR, one after another.

## -----------------------------------------------------------------------------
## Filtering IR where each sample have IR value of at least 0.1.
criteria <- "all[[\"IR\"]][, x] > 0.1" # [,x] will be replaced by each sample

filtered_ir <- filterSamples(all = all, criteria) 
table(filtered_ir)

## 
library(DESeq2)
library(BiocParallel)
register(MulticoreParam(workers = 8))
metaList <- DESeqDataSetFromPreCalculatedMatrix(expDesign = expDesign,
                                                designFormula = ~ Condition,
                                                selectedIR = irnames[filtered_ir],
                                                mat_intronDepth = all[["intronDepth"]],
                                                mat_spliceExact = all[["spliceExact"]])

dds = metaList$DESeq2Object                       # Extract DESeq2 Object with normalization factors ready
colData(dds)                                      # Check design of matrix

design(dds) = ~ Condition + Condition:IRFinder     # Build a formula of GLM. Read below for more details. 
dds = DESeq(dds, parallel = T)                                  # Estimate parameters and fit to model
resultsNames(dds)                                 # Check the actual variable name assigned by DESeq2



## -----------------------------------------------------------------------------
contrast <- c("Condition", "Normal", "Cancer")
contrast <- factor(contrast, levels = contrast)
results <- differential_IR(dds, contrast)
glimpse(results)


results %>% 
  dplyr::filter(abs(DeltaIR) > 0.1 & padj < 0.05)


## -----------------------------------------------------------------------------
library(limma)
## Filtering IR where each sample have IR value of at least 0.1.
criteria <- "all[[\"IR\"]][, x] > 0.1" # [,x] will be replaced by each sample

filtered_ir <- filterSamples(all = all, criteria) 
table(filtered_ir)
## --

mat  <- all[["IR"]][filtered_ir, ]
dim(mat)

design <- model.matrix(~ Condition, data = expDesign)
fit <- lmFit(mat, design)
summary(fit)
fit <- eBayes(fit)


limmaResults <- topTable(fit, number = sum(filtered_ir))
limmaResults2 <-limmaResults %>%
  as.data.frame %>%
  mutate(IR_Name = rownames(.)) %>%
  filter(adj.P.Val < 0.05 & AveExpr > 0.1 ) %>%
  arrange(desc(abs(logFC)))


## -----------------------------------------------------------------------------



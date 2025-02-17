% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/differential_IR.R
\name{differential_IR}
\alias{differential_IR}
\title{Identify differentially retained introns in biological conditions using DESeq2.}
\usage{
differential_IR(dds, contrast)
}
\arguments{
\item{dds}{dds object prepared using \link{DESeqDataSetFromPreCalculatedMatrix}.}

\item{contrast}{contrast for changing reference group etc. This must be compativle with DESeq2 format: Please use contrast with appropriate levels: Biological Condition, Reference level, Condition to compare to.}
}
\value{

}
\description{
This function identifies differentially retained introns in biological condition based on contrast factor and  an DESeq2 object prepared using \link{DESeqDataSetFromPreCalculatedMatrix}.
}

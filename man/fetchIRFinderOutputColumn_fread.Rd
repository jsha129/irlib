% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchIRFinderOutputColumns.R
\name{fetchIRFinderOutputColumn_fread}
\alias{fetchIRFinderOutputColumn_fread}
\title{Fetches selected columns from IRFinder output file and returns them as a list of matrices}
\usage{
fetchIRFinderOutputColumn_fread(filePaths, colNum = 9)
}
\arguments{
\item{filePaths}{location of IRFinder output files from which to extract data from.}

\item{colNum}{which column numbers to extract. see \link{IRF_column_names}.}

\item{sampleNames}{column names for each matrix.}
}
\value{

}
\description{
IRFinder output file (IRFinder-IR-dir.txt or IRFinder-IR-nondir.txt) reports information for 21 parameters such as intron depth or IR ratio per sample. This function extracts one column at a time for all samples and returns them as a list of same length as \emph{colNum} argument.
}
\keyword{internal}

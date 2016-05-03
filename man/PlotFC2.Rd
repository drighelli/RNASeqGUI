\name{PlotFC2}
\alias{PlotFC2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Fold Change from DESeq2 results
}
\description{
This function Plot Fold Change from DESeq2 results by using "plotMA" function of "DESeq" package.
}
\usage{
PlotFC2(res, the.file, Project)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
result table.
}
  \item{the.file}{
character indicating the name of the input count table.
}
  \item{Project}{
character indicating the name of the project.
}
}
\references{
Anders S, McCarthy DJ, Chen Y, Okoniewski M, Smyth GK,
Huber W and Robinson MD (2013). Count-based differential expression
analysis of RNA sequencing data using R and Bioconductor. Nature Protocols 8:1765-1786.
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ foldchange  }

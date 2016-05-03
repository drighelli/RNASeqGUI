\name{EdgeR2}
\alias{EdgeR2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
edgeR Analysis
}
\description{
This function performs the Negative Binomial Test by using "exactTest" function of "edgeR" package.
}
\usage{
EdgeR2(x, n, factors, the.file, fdr, Project)
}
\arguments{
  \item{x}{
numeric matrix of the counts.
}
  \item{n}{
numeric indicating the number of samples.
}
  \item{factors}{
list of samples.
}
  \item{the.file}{
character indicating the name of the input count table.
}
  \item{fdr}{
numeric indicating the False Discovery Rate. The default is 0.05.
}
  \item{Project}{
character indicating the name of the project.
}
}
\references{
Robinson MD, McCarthy DJ and Smyth GK (2010). edgeR:
a Bioconductor package for differential expression analysis of digital gene
expression data. Bioinformatics 26:139-140.

Robinson MD and Smyth GK (2007). Moderated statistical
tests for assessing differences in tag abundance. Bioinformatics 23:2881-2887.

Robinson MD and Smyth GK (2008). Small-sample estima-
tion of negative binomial dispersion, with applications to SAGE data.
Biostatistics 9:321-332.
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ edgeR }


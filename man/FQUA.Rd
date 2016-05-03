\name{FQUA}
\alias{FQUA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Full Quantile Normalization
}
\description{
This function performs a normalization based upon quantiles by using "normalize.quantiles" function of "preprocessCore" package.
}
\usage{
FQUA(x, the.file, Project)
}
\arguments{
  \item{x}{
numeric matrix of counts.
}
  \item{the.file}{
character indicating the name of the input count table.
}
  \item{Project}{
character indicating the name of the project.
}
}
\references{
Bolstad BM, Irizarry RA, Astrand M, and Speed TP (2003). A
Comparison of Normalization Methods for High Density Oligonucleotide
Array Data Based on Bias and Variance. Bioinformatics 19(2): 185-193.

Smyth GK (2005). Limma: linear models for microarray data.
Bioinformatics and Computational Biology Solutions using R and Bioconductor. Springer 397-420.
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ normalization }


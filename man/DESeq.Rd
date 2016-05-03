\name{DESeq}
\alias{DESeq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DESeq Analysis
}
\description{
This function performs the Negative Binomial Test by using "nbinomTest" function of "DESeq" package.
}
\usage{
DESeq(x, conditions, libTypes, treated, control, the.file, pvaladj, Project)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric matrix of the counts.
}
  \item{conditions}{
list of samples.
}
  \item{libTypes}{
list of characters indicating whether each sample is "paired-end" or "single-end".
}
  \item{treated}{
character indicating the treated sample.
}
  \item{control}{
character indicating the control sample.
}
  \item{the.file}{
character indicating the name of the input count table.
}
  \item{pvaladj}{
numeric indicating the P value adjusted. The default is 0.05.
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
\keyword{ DESeq }


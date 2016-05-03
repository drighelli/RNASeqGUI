\name{BaySeqNB}
\alias{BaySeqNB}
\alias{summary.BaySeqNB}
\title{
Negative Binomial Test
}
\description{
This function performs the Negative Binomial Test by using "getPriors.NB" and "getLikelihoods.NB" functions from "bayseq" package.
}
\usage{
BaySeqNB(x,conditions,list_nde,list_de,estType,samsize,sA,sB,the.file,
fdr,Project)
}
\arguments{
  \item{x}{
numeric matrix of the counts.
}
  \item{conditions}{
list of samples.
}
  \item{list_nde}{
numeric vector indicating which samples are supposed to contain NDE genes.
}
  \item{list_de}{
numeric vector indicating which samples are supposed to contain Differentially Expressed genes.
}
  \item{estType}{
character indicating the type of estimation. The default is "quantile".
}
  \item{samsize}{
numeric indicating how large a sample should be taken in estimating the prior. The default is 1000.
}
  \item{sA}{
character indicating the treated sample.
}
  \item{sB}{
character indicating the control sample.
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
Hardcastle TJ, Kelly KA (2010). baySeq: Empirical
Bayesian methods for identifying differential expression in sequence count
data. Bioinformatics 11:422.
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bayseq }

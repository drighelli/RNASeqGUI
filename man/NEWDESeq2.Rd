\name{NEWDESeq2}
\alias{NEWDESeq2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DESeq2 Analysis
}
\description{
This function performs the Negative Binomial Test by using "DESeq" function of "DESeq2" package.
}
\usage{
NEWDESeq2(x, conditions, libTypes, treated, control, the.file, pvaladj, Project)
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
Anders S and Huber W (2010) Differential expression analysis for sequence count data. Genome Biology 11:R106.
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ DESeq2 }


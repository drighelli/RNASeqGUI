\name{NoiDE2}
\alias{NoiDE2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
NoiSeq differential expression analysis
}
\description{
This function computes differential expression between two experimental conditions by using "noiseq" function of "NOISeq" package.
}
\usage{
NoiDE2(x, conditions, TissueRuns, replicate_type, p, the.file, Project)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric matrix of the counts.
}
  \item{conditions}{
list of samples.
}
  \item{TissueRuns}{
list of characters indicating whether each sample belongs to a particular run in the experiment.
}
  \item{replicate_type}{
character indicating whether the samples are "biological" or "technical" replicates. The default is "technical".
}
  \item{p}{
numeric indicating the posterior probability. The default is 0.8.
}
  \item{the.file}{
character indicating the name of the input count table.
}
  \item{Project}{
character indicating the name of the project.
}
}
\references{
Tarazona S, Garcia-Alcalde F, Ferrer A, Dopazo J, and Conesa
A (2011). Differential expression in RNA-seq: a matter of depth. Genome
Research 21:2213-222.
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ NOISeq }


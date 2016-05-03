\name{TMM}
\alias{TMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Trimmed Mean of M-values Normalization
}
\description{
This function performs a Trimmed Mean of M-values normalization by using "tmm" function of "NOISeq" package.
}
\usage{
TMM(x, the.file, Project)
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
Tarazona S, Garcia-Alcalde F, Ferrer A, Dopazo J, and Conesa
A (2011). Differential expression in RNA-seq: a matter of depth. Genome
Research 21:2213-222.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ normalization }


\name{RPKM}
\alias{RPKM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads Per Kilobase Per Million Mapped Reads Normalization
}
\description{
This function performs a reads per kilobase per million mapped reads normalization by using "rpkm" function of "NOISeq" package.
}
\usage{
RPKM(x, the.file, Project)
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
Mortazavi A, Williams BA, McCue K, Schaeffer L, Wold B (2008). Mapping and quantifying mammalian transcriptomes by RNA-seq.
Nature Methods 5:621-8.

Tarazona S, Garcia-Alcalde F, Ferrer A, Dopazo J, and Conesa
A (2011). Differential expression in RNA-seq: a matter of depth. Genome
Research 21:2213-222.
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ normalization }

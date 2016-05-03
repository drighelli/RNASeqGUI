\name{plotNtFreq}
\alias{plotNtFreq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Nucleotide Frequencies
}
\description{
This function returns the percentage of each nucleotide at each position of the reads by using "plotNtFrequency" function of "EDASeq" package.
}
\usage{
plotNtFreq(the.file, Project)
}
\arguments{
  \item{the.file}{
character indicating the name of the input count table.
}
  \item{Project}{
character indicating the name of the project.
}
}
\references{
Risso D , Schwartz K , Sherlock G and Dudoit S (2011). GC-Content Normalization for RNA-Seq Data. BMC Bioinformatics 12:1-480.
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ frequency}

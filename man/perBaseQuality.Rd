\name{perBaseQuality}
\alias{perBaseQuality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Per Base Quality Score
}
\description{
This function returns a plot showing the quality of each base of the reads averaged across all bam files. It uses "plotQuality" function of "EDASeq" package.
}
\usage{
perBaseQuality(the.file, Project)
}
%- maybe also 'usage' for other objects documented here.
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
\keyword{ quality }


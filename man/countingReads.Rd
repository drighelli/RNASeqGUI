\name{countingReads}
\alias{countingReads}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Counting reads per each gene
}
\description{
This function uses "summarizeOverlaps" function from the package "GenomicRanges" to return a data-frame of gene counts.
}
\usage{
countingReads(the.file, Bam.FolderNew, count.mode, StrandEntry, Project)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{the.file}{
character indicating the name of the input count table.
}
  \item{Bam.FolderNew}{
character indicating the name of the bam folder.
}
  \item{count.mode}{
character indicating the counting mode. The default is "Union".
}
  \item{StrandEntry}{
logical indicating whether to ignore the strand or not. The default is TRUE.
}
  \item{Project}{
character indicating the name of the project.
}
}
\references{
Aboyoun P, Pages H and Lawrence M (2013). GenomicRanges: Representation and manipulation of genomic intervals, R package version 1.14.4
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ count }


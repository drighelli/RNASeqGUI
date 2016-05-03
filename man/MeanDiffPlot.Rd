\name{MeanDiffPlot}
\alias{MeanDiffPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean Difference Plot
}
\description{
This function generates a mean-difference scatterplot of two columns in the count file by using the "MDPlot" function of "EDASeq" package.
}
\usage{
MeanDiffPlot(x, column1, column2, the.file, Project)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric matrix of the counts.
}
  \item{column1}{
numeric indicating which column of the x matrix should be plotted.
}
  \item{column2}{
numeric indicating which column of the x matrix should be plotted.
}
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
\keyword{ MDPlot }
\keyword{ EDASeq }% __ONLY ONE__ keyword per line

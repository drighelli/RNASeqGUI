\name{meanVariancePlot}
\alias{meanVariancePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean Variance Plot
}
\description{
This function takes a count file and two integers Column1 and Column2 and returns a plot showing the mean of the two selected columns
against their difierence gene by gene. For this function, the log check box does not change anything. This function makes use of "meanVarPlot" function of "EDASeq" package.
}
\usage{
meanVariancePlot(x, the.file, Project)
}
%- maybe also 'usage' for other objects documented here.
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
Risso D , Schwartz K , Sherlock G and Dudoit S (2011). GC-Content Normalization for RNA-Seq Data. BMC Bioinformatics 12:1-480.
}
\author{
Francesco Russo
}
\keyword{ MDPlot }


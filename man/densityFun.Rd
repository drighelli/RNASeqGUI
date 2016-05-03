\name{densityFun}
\alias{densityFun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Density Function
}
\description{
This function takes a count file, and a sample specified by an integer
in Column1 as input and produces a curve representing the density function
of the counts for the selected sample. The method is available in two modes.
By default the log of the counts (we add 1 to each number in the count file to avoid the problem of
log(0)) will be used to generate the density function. This function makes use of "density" function of "stats" package. 
}
\usage{
densityFun(x, column, the.file, log, Project)
}
\arguments{
  \item{x}{
numeric matrix of the counts.
}
  \item{column}{
numeric indicating which column of the x matrix should be plotted.
}
  \item{the.file}{
character indicating the name of the input count table.
}
  \item{log}{
logical indicating if the log should be applied to the x matrix.
}
  \item{Project}{
character indicating the name of the project.
}
}
\references{
"stats" package
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ density }


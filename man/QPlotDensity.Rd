\name{QPlotDensity}
\alias{QPlotDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
QPlot Density
}
\description{
This function takes a count file and and returns a plot showing the density function of each column (sample) in the count file. Moreover, for this function it is possible to generate the density either of the raw counts or the log of the counts (we add 1 to each number in the count file to avoid the problem of log(0) ). It uses the "qplot" function of "ggplot2" package.
}
\usage{
QPlotDensity(x, the.file, log, Project)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x}{
numeric matrix of counts.
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
"ggplot2" package
}
\author{
Francesco Russo
}
\keyword{ ggplot2 }


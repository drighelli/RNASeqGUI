\name{QPlotHistogram}
\alias{QPlotHistogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
QPplot Histogram
}
\description{
This function takes a count file and and returns an histogram showing the count level of each column in the count file. Moreover,
for this function it is possible to generate the histogram either of the raw counts or the log of the counts (we add 1 to each number in the count file to avoid the problem of log(0) ).
It uses the "qplot" function of "ggplot2" package.
}
\usage{
QPlotHistogram(x, the.file, log, Project)
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ggplot2 }


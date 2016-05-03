\name{PlotCounts}
\alias{PlotCounts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Counts
}
\description{
This function takes a count file as input (in txt or cvs format) where the rows correspond to the gene ids and the columns correspond to the samples. This function also takes two integers, one specifying
Column1 and the other specifying Column2 of the count file and plots the counts of sample in Column1 against the counts of sample in Column2. Moreover, for this function it is possible to plot either the raw counts or the log of the counts (we add 1 to each number in the count file to avoid the problem of log(0) ). This function makes use of "plot" function of "graphics" package.
}
\usage{
PlotCounts(x, column1, column2, the.file, log, Project)
}
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
  \item{log}{
logical indicating if the log should be applied to the x matrix.
}
  \item{Project}{
character indicating the name of the project.
}
}
\references{
"graphics" package
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ count }


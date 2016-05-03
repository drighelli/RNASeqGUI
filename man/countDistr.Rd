\name{countDistr}
\alias{countDistr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Count Distribution Boxplot
}
\description{
This function takes a count file as input and generates a boxplot
showing the distribution of the counts for each sample. Moreover, for this function it is
possible to generate the box plot either of the raw counts or the log of the
counts (we add 1 to each number in the count file to avoid the problem of log(0)). This function uses "boxplot" function of "graphics" package.
}
\usage{
countDistr(x, the.file, log, Project)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric matrix of the counts.
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


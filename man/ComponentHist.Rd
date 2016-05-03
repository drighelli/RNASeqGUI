\name{ComponentHist}
\alias{ComponentHist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Component Histogram
}
\description{
This function makes use of "screeplot" function of the
"stats" package. This function takes a count file and returns an histogram
showing the variance level of each component.
}
\usage{
ComponentHist(x, the.file, Project)
}
\arguments{
  \item{x}{
numeric matrix of the counts.
}
  \item{the.file}{
character indicating the name of the input count table.
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
\keyword{ component }


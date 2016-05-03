\name{FDRhistBaySeq}
\alias{FDRhistBaySeq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FDR Histogram for bayseq results
}
\description{
This function generates a histogram that displays the frequency of FDR values by using the "hist" function of "graphics" package.
}
\usage{
FDRhistBaySeq(results_BaySeq, the.file, Project)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results_BaySeq}{
result table.
}
  \item{the.file}{
character indicating the name of the input count table.
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
\keyword{ histogram }

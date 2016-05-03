\name{Pvalues}
\alias{Pvalues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pvalue Histogram from DESeq results
}
\description{
This function generates a Pvalue histogram from DESeq results by using "hist" function of "graphics" package.
}
\usage{
Pvalues(res, the.file, Project)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
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

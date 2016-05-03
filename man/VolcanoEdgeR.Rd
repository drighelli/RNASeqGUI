\name{VolcanoEdgeR}
\alias{VolcanoEdgeR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Volcano plot for edgeR results
}
\description{
This function generates a volcano plot by using "plot" function of "graphics" package.
}
\usage{
VolcanoEdgeR(name, fdr, results_EdgeR, the.file, Project)
}
\arguments{
  \item{name}{
character indicating the name of a particular gene to be diplayed.
}
  \item{fdr}{
numeric indicating the False Discovery Rate. The default is 0.05.
}
  \item{results_EdgeR}{
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
\keyword{ volcano }

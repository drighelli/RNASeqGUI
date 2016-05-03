\name{PlotFC3}
\alias{PlotFC3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Fold Change from edgeR results
}
\description{
This function Plot Fold Change from edgeR results by using "plot" function of "graphics" package.
}
\usage{
PlotFC3(name, fdr, results_EdgeR, the.file, Project)
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
\keyword{ foldchange }


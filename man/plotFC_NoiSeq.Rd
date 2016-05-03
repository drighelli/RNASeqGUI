\name{plotFC_NoiSeq}
\alias{plotFC_NoiSeq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Fold Change from NoiSeq results
}
\description{
This function Plot Fold Change from NoiSeq results by using "plot" function of "graphics" package.
}
\usage{
plotFC_NoiSeq(name, p, results_NoiSeq, the.file, Project)
}
\arguments{
  \item{name}{
character indicating the name of a particular gene to be diplayed.
}
  \item{p}{
numeric indicating the posterior probability. The default is 0.7.
}
  \item{results_NoiSeq}{
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

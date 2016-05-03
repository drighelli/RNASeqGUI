\name{VolcanoNoiSeq}
\alias{VolcanoNoiSeq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Volcano plot for noiseq results
}
\description{
This function generates a volcano plot by using "plot" function of "graphics" package.
}
\usage{
VolcanoNoiSeq(name, p, results_noi, the.file, Project)
}
\arguments{
  \item{name}{
character indicating the name of a particular gene to be diplayed.
}
  \item{p}{
numeric indicating the posterior probability. The default is 0.7.
}
  \item{results_noi}{
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

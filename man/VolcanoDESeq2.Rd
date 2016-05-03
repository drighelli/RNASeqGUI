\name{VolcanoDESeq2}
\alias{VolcanoDESeq2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Volcano plot for DESeq2 results
}
\description{
This function generates a volcano plot by using "plot" function of "graphics" package.
}
\usage{
VolcanoDESeq2(name, p, results_DESeq2, the.file, Project)
}
\arguments{
  \item{name}{
character indicating the name of a particular gene to be diplayed.
}
  \item{p}{
numeric indicating the p value adjusted. The default is 0.05.
}
  \item{results_DESeq2}{
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

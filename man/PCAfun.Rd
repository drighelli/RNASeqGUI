\name{PCAfun}
\alias{PCAfun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PCA function
}
\description{
This function returns the principal component analysis plot between the first two components. For this function, the log check box does
not change anything. This function makes use of "prcomp" function of "stats" package.
}
\usage{
PCAfun(x, condition, the.file, legendpos, Project)
}
\arguments{
  \item{x}{
 numeric matrix of the counts.
}
  \item{condition}{
list of samples.
}
  \item{the.file}{
character indicating the name of the input count table.
}
  \item{legendpos}{
character indicating the position of the legend in the plot. The default is "topright".
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
\keyword{ PCA }


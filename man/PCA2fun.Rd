\name{PCA2fun}
\alias{PCA2fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PCA 3D function
}
\description{
function takes the same inputs of the PCA
function and returns the 3D PCA plot between the first, the second and the third principal
component. For this function, the log check box does not change anything This function makes use of "prcomp" and "scatterplot3d" functions of "stats" and "scatterplot3d" packages, respectively.
}
\usage{
PCA2fun(x, condition, the.file, legendpos, Project)
}
%- maybe also 'usage' for other objects documented here.
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
"stats" and "scatterplot3d" packages
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ PCA }
\keyword{ 3D }% __ONLY ONE__ keyword per line

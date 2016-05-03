\name{Heatmap}
\alias{Heatmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Heatmap
}
\description{
This function generates an heat-map by using "heatmap" function of "stats" package. The function returns an heat-map of the Nth
most expressed genes (on average). The columns of the heatmap are the
samples, while the rows in the heat-map represent the gene ids of the most
expressed ones.
}
\usage{
Heatmap(x, the.file, geneNum, log, Project)
}
\arguments{
  \item{x}{
numeric matrix of counts.
}
  \item{the.file}{
character indicating the name of the input count table.
}
  \item{geneNum}{
numeric indicating the number or genes should be displayed.
}
  \item{log}{
logical indicating if the log should be applied to the x matrix.
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
\keyword{ heatmap }


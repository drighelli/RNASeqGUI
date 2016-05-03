\name{Venn3de}
\alias{Venn3de}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Venn Diagram Three Sets of DE
}
\description{
This function generates a venn digram of three sets by using "vennDiagram" function of "limma" package.
}
\usage{
Venn3de(x, y, z, label1, label2, label3, Project)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
first result table.
}
  \item{y}{
second result table.
}
  \item{z}{
third result table.
}
  \item{label1}{
character indicating the name of the first result table.
}
  \item{label2}{
character indicating the name of the second result table.
}
  \item{label3}{
character indicating the name of the third result table.
}
  \item{Project}{
character indicating the name of the project.
}
}
\references{
Smyth GK (2005). Limma: linear models for microarray data.
Bioinformatics and Computational Biology Solutions using R and Bioconductor. Springer 397-420.
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ venn }


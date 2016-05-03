\name{ChNumber}
\alias{ChNumber}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Chromosome Number Histograms
}
\description{
This function makes use of "barplot" function of the
"graphics" package. This function returns as many histograms as the number
of bam files stored in the provided input folder. Each histogram shows the
number of reads are present in each chromosome.
}
\usage{
ChNumber(the.file, Project)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
\keyword{ chromosome }

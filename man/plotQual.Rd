\name{plotQual}
\alias{plotQual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Quality
}
\description{
This function returns as many box-plots as the
number of bam files stored in the provided input folder. Each box-plot shows
the quality of the reads per each base. This function makes use of "plotQuality" function of "EDASeq" package.
}
\usage{
plotQual(the.file, Project)
}
\arguments{
  \item{the.file}{
character indicating the name of the input count table.
}
  \item{Project}{
character indicating the name of the project.
}
}
\references{
Risso D , Schwartz K , Sherlock G and Dudoit S (2011). GC-Content Normalization for RNA-Seq Data. BMC Bioinformatics 12:1-480.
}
\author{
Francesco Russo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ quality }


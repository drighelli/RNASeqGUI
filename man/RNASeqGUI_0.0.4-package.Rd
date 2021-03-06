\name{RNASeqGUI-package}
\alias{RNASeqGUI-package}
\docType{package}
\title{
RNASeqGUI
}
\description{
RNASeqGUI is a graphical user interface for the identification of differentially expressed genes from RNA-Seq experiments. RNASeqGUI was implemented following and expanding the idea presented in http://tuxette.nathalievilla.org/?p=866&lang=en. RNASeqGUI includes several well known RNA-Seq tools, available as command line in www.bioconductor.org.
}
\details{
\tabular{ll}{
Package: \tab RNASeqGUI\cr
Type: \tab Package\cr
Version: \tab 0.0.4\cr
Date: \tab 2014-03-18\cr
License: \tab GPL\cr
}
RNASeqGUI is divided into five main sections. Each section is dedicated to a particular step of the data analysis process. The first section covers the exploration of the bam files. The second concerns the counting process of the mapped reads against a genes annotation file. The third focuses on the exploration of count-data and on preprocessing of the data, including the normalization procedures. The fourth is about the identification of the differentially expressed genes that can be performed by several methods, such as: DESeq, DESeq2, EdgeR, NOISeq, BaySeq. Finally, the fifth section regards the inspection of the results produced by these methods and the quantitative comparison among them. 
}
\author{
Author: Francesco Russo and Claudia Angelini

Maintainer: Francesco Russo <rnaseqgui@na.iac.cnr.it>
}
\references{
Sanges R, Cordero F and Calogero R A (2007) oneChannelGUI:
a graphical interface to Bioconductor tools, designed for life scientists who
are not familiar with R language. Bioinformatics 23:3406-3408

Wettenhall J M and Smyth G K (2004) limmaGUI: a graphical user interface for linear modeling of microarray data. Bioinformatics
20:3705-3706.

Lawrence M and Temple Lang D (2010) RGtk2: A Graphical
User Interface Toolkit for R. Journal of Statistical Software 37(8):1-52.

Villa-Vialaneix N and Leroux D (2013) sexy-rgtk: a
package for programming RGtk2 GUI in a user-friendly manner. In Proceedings of: 2mes rencontres R, Lyon, France, June 27-28.

Anders S and Huber W (2010) Differential expression analysis
for sequence count data. Genome Biology 11:R106.

Robinson MD, McCarthy DJ and Smyth GK (2010). edgeR:
a Bioconductor package for differential expression analysis of digital gene
expression data. Bioinformatics 26:139-140.

Robinson MD and Smyth GK (2007). Moderated statistical
tests for assessing differences in tag abundance. Bioinformatics 23:2881-2887.

Robinson MD and Smyth GK (2008). Small-sample estima-
tion of negative binomial dispersion, with applications to SAGE data.
Biostatistics 9:321-332.

McCarthy DJ, Chen Y and Smyth GK (2012). Differential
expression analysis of multifactor RNA-Seq experiments with respect to
biological variation. Nucleic Acids Research 40:4288-4297

Hardcastle TJ, Kelly KA (2010). baySeq: Empirical
Bayesian methods for identifying differential expression in sequence count
data. Bioinformatics 11:422.

Tarazona S, Garcia-Alcalde F, Ferrer A, Dopazo J, and Conesa
A (2011). Differential expression in RNA-seq: a matter of depth. Genome
Research 21:2213-222.

Bullard JH, Purdom E, Hansen KD, Dudoit S (2010). Evaluation of statistical methods for normalization and differential expression in
mRNA-seq experiments. BMC Bioinformatics 11:94.

Robinson MD, Oshlack A (2010). A scaling normalization
method for differential expression analysis of RNA-seq data. Genome Biology 11:R25.

Mortazavi A, Williams BA, McCue K, Schaeffer L, Wold B (2008). Mapping and quantifying mammalian transcriptomes by RNA-seq.
Nature Methods 5:621-8.

Bolstad BM, Irizarry RA, Astrand M, and Speed TP (2003). A
Comparison of Normalization Methods for High Density Oligonucleotide
Array Data Based on Bias and Variance. Bioinformatics 19(2): 185-193.

Smyth GK (2005). Limma: linear models for microarray data.
Bioinformatics and Computational Biology Solutions using R and Bioconductor. Springer 397-420.

Anders S, McCarthy DJ, Chen Y, Okoniewski M, Smyth GK,
Huber W and Robinson MD (2013). Count-based differential expression
analysis of RNA sequencing data using R and Bioconductor. Nature Protocols 8:1765-1786.

Brooks AN, Yang L, Duff MO, Hansen KD, Park JW, Dudoit S,
Brenner SE and Graveley BR (2011). Conservation of an RNA regulatory
map between Drosophila and mammals. Genome Research 21:193-202.

Kim D, Pertea G, Trapnell C, Pimentel H, Kelley R and
Salzberg SL (2013) TopHat2: accurate alignment of transcriptomes in the
presence of insertions, deletions and gene fusions. Genome Biology 14:R36.

Aboyoun P, Pages H and Lawrence M (2013). GenomicRanges: Representation and manipulation of genomic intervals, R package version 1.14.4
 
Lohse M, Bolger AM, Nagel A, Fernie AR, Lunn JE, Stitt M and Usadel B (2012). RobiNA: a user-friendly, integrated software solution for RNASeq-based transcriptomics. Nucleic Acid Research  40(W1):W622-W627. 
 
Risso D , Schwartz K , Sherlock G and Dudoit S (2011). GC-Content Normalization for RNA-Seq Data. BMC Bioinformatics 12:1-480.
}

\keyword{ interface }
\seealso{
http://bioinfo.na.iac.cnr.it/RNASeqGUI/manual.pdf
}

is.installed <- function(mypkg) {
  is.element(mypkg, installed.packages()[,1])
}


install.packages.list <- function(packages.list, is.bioconductor=FALSE){
  for(pckg in packages.list) {
    if(!is.installed(pckg)) {
      if(is.bioconductor){
        source("http://bioconductor.org/biocLite.R")
        biocLite(pckg)
        print(paste0('Bio package ', pckg, ' installed!'))
      } else {
        install.packages(pckg)
        print(paste0('Package ', pckg, ' installed!'))
      }
    } else {
      print(paste0('Package ', pckg, ' already installed!'))
    }
  }
}


install.rnaseqgui.dependencies <- function() {
  
  bio.packages <- c(
    'biomaRt',
    'DEXSeq',
    'pasilla',
    'GenomicRanges',
    'GenomicFeatures',
    'Rsamtools',
    'edgeR',
    'baySeq',
    'NOISeq',
    'DESeq',
    'DESeq2',
    'gplots',
    'EDASeq',
    'leeBamViews',
    'preprocessCore',
    'scatterplot3d',
    'BiocParallel',
    'digest',
    'Rsubread',
    'gage',
    'pathview',
    'ReportingTools',
    'graphite',
    'BiocStyle'
  )
  
  cran.packages <- c(
    'e1071',
    'ineq',
    'RGtk2',
    'RCurl',
    'digest',
    'ggplot2',
    'RColorBrewer',
    'VennDiagram',
    'XML',
    'tcltk',
    'knitr',
    'filehash',
    'latticeExtra',
    'plotrix',
    'car',
    'filehash'
  )
  
  install.packages.list(bio.packages, T)
  install.packages.list(cran.packages)
}

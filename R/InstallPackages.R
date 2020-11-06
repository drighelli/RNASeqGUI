is.installed <- function(mypkg) {
  is.element(mypkg, installed.packages()[,1])
}


install.packages.list <- function(packages.list)
{
    lapply(packages.list, function(pckg)
    {
        if(!is.installed(pckg)) 
        {
            BiocManager::install(pckg)
        } else {
            message(paste0('Package ', pckg, ' already installed!'))
        }
    })
  return(NULL)
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
  
  install.packages.list(bio.packages)
  install.packages.list(cran.packages)
}

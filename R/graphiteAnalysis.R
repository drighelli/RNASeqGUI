SPIA_function <- function(dbsel, dbname, file.name, species, Project, verbose=T, print.names.flag=T, prepare.SPIA.flag=T){
  #dbsel='kegg'
  #dbname= 'hsapiens_gene_ensembl'
  #file.name='/home/dario/RNASeqGUI_Projects/union counts.txt_results_EdgeR.txt'
  #species='hsa'
  #Project='a' 
  #verbose=T
  #print.names.flag=T
  #prepare.SPIA.flag=T
  
  require(biomaRt)
  require(pathview)
  require(graphite)
  
  db.cache <- InitDb(db.name='graphite_db', db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(db.cache, file.name, "filename_key")
  SaveInCache(db.cache, Project, "project_key")
  SaveInCache(db.cache, dbname, "dbname_key")
  SaveInCache(db.cache, dbsel, "dbsel_key")
  SaveInCache(db.cache, verbose, "verbose_key")
  SaveInCache(db.cache, print.names.flag, "printnamesflag_key")
  SaveInCache(db.cache, prepare.SPIA.flag, "preparespiaflag_key")
  SaveInCache(db.cache, species, "species_key")
  
  out.suffix <- NULL

  ensembl <- useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset=dbname) #to connect to a BioMart database
  entrez <- getBM(attributes = c("entrezgene"), mart=ensembl)
  map.results <- getBM(attributes = c("ensembl_gene_id", "entrezgene"), mart=ensembl)
  print('preparing spia data')
  list.data <- PrepareSPIAData(filename=file.name, biomart.map=map.results)
  SaveInCache(db.cache, list.data, "listdata_key")

  out.suffix <- list.data$out.suffix
  fc.vector <- list.data$fc.vector

  if(dbsel == 'reactome') {
    db <- reactome
    pathsetname <- paste('reactome',species, sep = '_')
    print('You are using reactome database')
  } else if( dbsel=='kegg') {
    db <- kegg
    pathsetname <- paste('kegg', species, sep = '_')
    print('You are using KEGG database')
  } else if( dbsel=='biocarta') {
    db <- biocarta
    pathsetname <- paste('biocarta',species, sep = '_')
    print('You are using biocarta database')
  } else {
    print('WARNING: db name not recognized!!!\n KEGG will be selected by default')
    db <- kegg
    pathsetname <- paste('kegg',species, sep = '_')
  }
  
  SaveInCache(db.cache, pathsetname, "pathsetname_key")
  
  print('calling SPIA...')
  print(head(map.results))
  
  res <- CallSPIA(pathway.db = db, pathsetname=pathsetname, fc.vector = fc.vector, all.genes.names = entrez[,1], 
                  prepare.SPIA.flag = prepare.SPIA.flag, print.names.fl = print.names.flag, verbose = verbose)
  print(class(res))
  
  SaveInCache(db.cache, res, "res_key")
  if( dim(res)[1] != 0 ) {
    path.name.list <- res
    if(Sys.info()[[1]]=="Windows"){ ##only separator changes
      separator='\\'  
    }else{         # Linux
      separator='/'
    }
  
    if(dbsel=='kegg') {
      ## map graphite pathways to kegg pathways through gage package
      require(gage)
      data(kegg.gs)
      
      kegg.gs=NULL
      ks=kegg.gsets(species = species, id.type = "entrez")##
      kegg.gs=ks$kg.sets
      
      pwnames <- res$Name
      
      count <- 1
      indexes <- data.frame()
      for(i in 1:length(pwnames))
      {
        ind <- grep(pwnames[i], names(kegg.gs))
        if(length(ind) != 0) {
          indexes[count,1] <- i
          indexes[count,2] <- ind
          count <- count + 1
        }
      }
      
      path.name.list <- substr(names(kegg.gs)[indexes[,2]], 1, 8)
      prevwd=getwd()
      setwd(paste(getwd(),"RNASeqGUI_Projects", Project, "Plots", sep=separator))
      pv.out.list <- sapply(path.name.list, function(pid) pathview( gene.data = fc.vector, pathway.id = pid, species = species, out.suffix=out.suffix))
      setwd(prevwd)
      #print(head(pv.out.list))
    }
    if(verbose) {
      print(head(res))
    }
    write.table(res, file = paste(getwd(), "RNASeqGUI_Projects", Project, "Results", paste0("Graphite_results_",dbsel,".txt"), sep=separator) , quote=FALSE, sep="\t", row.names=TRUE)
    message=paste("Results of Graphite have been saved in RNASeqGUI_Projects", Project,"Results folder!", sep=separator)
    print(message)
    print('done!')
    return(res)
  
  } else {
    print('done!')
    print('No Enriched pathways for this gene set!')
    return(NULL)
  }
  
  #return(res)
}


PrepareSPIAData <- function(filename, biomart.map) {
  cat('opening file: ', filename,'\n')
  data <- read.table(file = filename, header = T, row.names=1)
  print(head(data))
  print(head(biomart.map))
  m <- mol.sum(mol.data = data, id.map = biomart.map, sum.method = "mean")
  print(dim(m))
  print(head(m))
  
  suffix <- NULL
  fc <- NULL
  if(length(colnames(m))==0){
    fc=m[,1]
    names(fc) <- rownames(m)
    out.suffix='genelist'
    print("You are using a generic gene list.")
  } else if(length(which(colnames(m)=="log2FoldChange"))>0) {  # you are using DESeq/DESeq2
    fc <- m[,'log2FoldChange']
    names(fc) <- rownames(m)
    sum(is.infinite(fc)) #to manage the problem of inf
    fc[fc>10] <- 10
    fc[fc< -10] <- -10
    suffix <- 'deseq'
    print("You are using an DESeq/DESeq2 result file.")
  } else if(colnames(m)[1] == "logFC"){  # you are using EdgeR
    fc <- m[,'logFC']
    names(fc) <- rownames(m)
    suffix <- 'edger'
    print("You are using an edgeR result file.")
  } else if(colnames(m)[3] == "M"){  # you are using NOISeq 
    fc <- m[,'M']
    names(fc) <- rownames(m)
    sum(is.infinite(fc)) #to manage the problem of inf
    fc[fc>10] <- 10
    fc[fc< -10] <- -10
    suffix <- 'noiseq'
    print("You are using an NOISeq result file.")
  } else if(colnames(m)[5] == "log2FC"){ # you are using NOISeqBIO
    fc=m[,'log2FC']
    print(head(fc))
    names(fc)=rownames(m)
    sum(is.infinite(fc)) #to manage the problem of inf
    fc[fc>10]=10
    fc[fc< -10]=-10
    out.suffix="noiseqbio"
    print("You are using a NOISeqBIO result file.")    
  }else{
    print('ERROR: unrecognized file type!!!')
  }
  #return(fc)
  return( list(fc.vector=fc, out.suffix=suffix) )
}

CallSPIA <- function(pathway.db, pathsetname, fc.vector, all.genes.names, print.names.fl=F, prepare.SPIA.flag = T, verbose=T) {
  require(SPIA)
  if(prepare.SPIA.flag) {
    if(verbose) {
      cat('Preparing SPIA database on pathset: ',  pathsetname,'...\n')
    }
    prepareSPIA(db = pathway.db, pathwaySetName = pathsetname, print.names = print.names.fl)
    if(verbose) {
      cat('done!\n')
    }
  }
  if(verbose) {
    cat('Running SPIA on pathset: ', pathsetname,'\n')
  }
#   print(head(fc.vector))
#   print(head(all.genes.names))
  p <- runSPIA(de=fc.vector, all=all.genes.names, pathwaySetName =  pathsetname)
  if(verbose) {
    cat('done!\n')
  }
  print(head(p))
  return(p)
}

PrintSPIAReport <- function (dbsel, dbname, file.name, species, Project, verbose=T, print.names.flag=T, prepare.SPIA.flag=T) {
#   ###################ONLY FOR TEST######################
#   message5 <- paste("  ",sep="\n")
#   write(message5, file = report, ncolumns = if(is.character(message5)) 1 else 5, append = FALSE, sep = "\n")
#   ###################ONLY FOR TEST######################

  cat('processing graphite report...\n')
  if(Sys.info()[[1]]=="Windows"){ ##only separator changes
    sys.separator='\\'  
  }else{         # Linux
    sys.separator='/'
  }
  report=paste("RNASeqGUI_Projects", Project, "Logs", "report.Rmd",sep=sys.separator)

  message5 <- paste("  ",sep="\n")
  write(message5, file = report, ncolumns = if(is.character(message5)) 1 else 5, append = TRUE, sep = "\n")
  
  message2 <- paste(" * In the *Gene-Set/Pathway Interface*, you clicked the **Graphite** button at `", Sys.time(),"` and the Graphite_result.txt file has been saved in the `", Project,"/Results` folder.", sep="")
  write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message2 <- paste("You chose the following file: `",file.name,"`, dbsel: `",dbsel,"`, dbname: `",dbname,"`, specie: `",species,"`, verbose: `",verbose,
                    "`, print.names.flag: `",print.names.flag,"`, prepare.SPIA.flag: `",prepare.SPIA.flag,"`, Project: `",Project,"`",sep="\n")
  write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
  message5 <- paste(" The list of called functions:",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" ```{r} ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message2 <- paste("SPIA_function",sep="")
  write(message2, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message2 <- paste("PrepareSPIAData",sep="")
  write(message2, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message2 <- paste("CallSPIA",sep="")
  write(message2, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message8 <- paste(" ``` ",sep="\n")
  write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" This R code has been run:",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" ```{r} ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("db.cache <- InitDb(db.name='graphite_db', db.path='cache')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste(" fileName <- LoadCachedObject(db.cache, 'filename_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste(" Project <- LoadCachedObject(db.cache, 'project_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste(" dbname <- LoadCachedObject(db.cache, 'dbname_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste(" dbsel <- LoadCachedObject(db.cache, 'dbsel_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste(" verbose <- LoadCachedObject(db.cache, 'verbose_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste(" print.names.flag <- LoadCachedObject(db.cache, 'printnamesflag_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste(" prepare.SPIA.flag <- LoadCachedObject(db.cache, 'preparespiaflag_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste(" species <- LoadCachedObject(db.cache, 'species_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message2 <- paste("#SPIA_function(dbsel=dbsel, dbname=dbname, file.name = file.name, species=species, Project=Project, verbose=verbose, print.names.flag=verbose, prepare.SPIA.flag=prepareSPIA)",sep='')
  write(message2, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" res <- LoadCachedObject(db.cache, 'res_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste(" print(head(res))", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message8 <- paste(" ``` ",sep="\n")
  write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" _______________________________________________________________________ ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  cat('done\n')
}




# require('filehash')

GetSpecieCode <- function(specie) { 
  ##screen  the specie.gp and set the specie
  switch(specie, 
    HUMAN={
      dataset='hsapiens_gene_ensembl'
      attribute='hgnc_symbol'
    },
    MOUSE={
      dataset='mmusculus_gene_ensembl'
      attribute='mgi_symbol'
    },
    DROSOPHILA={
      dataset='dmelanogaster_gene_ensembl'
      attribute='bdgp_symbol'
    },
    ZEBRAFISH={
      dataset='drerio_gene_ensembl'
      attribute='zfin_symbol'
    },
    {
      print('WARNING: no species detected! Using, HUMAN!')
      dialog <- MyDialog(window, 'warning', message='no species detected! Using, HUMAN!', title='WARNING')
      dataset='hsapiens_gene_ensembl'
      attribute='hgnc_symbol'
    }
  )
  return(list('dataset'=dataset, 'attribute'=attribute))
}

ConvertList <- function(id.list, map) {
  res.map <- apply(id.list, 1, function(x) {which( map[,1] %in% x)})
  print(head(res.map))
  print(length(res.map))
  print(dim(id.list)[1])
  if(length(res.map)==dim(id.list)[1]) {
    id.list.n <- lapply(res.map, function(item) { 
                                        if(length(item)>0){
                                          map[item[1],2]
                                        } else {
                                          map[item,2]
                                        }
                                        })
  } else{
    if(length(res.map)==0) {
      print('problem res.map')
    } else if(length(id.list)==0) {
      print('problem id.list')
    }
  }
  
  return(unlist(id.list.n))
}



DavidConnect <- function(user.mail="rnaseqgui@na.iac.cnr.it") {
  if(!exists('david')) {
    david <- DAVIDWebService$new(email=user.mail, url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
  } else {
    if(!is.connected(david)) {
      david <- DAVIDWebService$new(email=user.mail, url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
    }
  }
  if(getTimeOut(david)==30000) {
    setTimeOut(david, 500000)
  }
  if(is.null(getHttpProtocolVersion(david))) {
    setHttpProtocolVersion(david, "HTTP/1.0")
  }
  return(david)
}

SavePathwaysList <- function(results.david.data.frame, david.db.path, just.file.name) {
  # print(david.db.path)
  david.db.path <- file.path(david.db.path, just.file.name)
  if(nrow(results.david.data.frame)!=0) {
    print(getwd())
    print(david.db.path)
    dir.create(david.db.path, recursive = TRUE)
    for(i in 1:nrow(results.david.data.frame)) {
      gene.list <- strsplit( as.character(results.david.data.frame[i,"Genes"]),", ")
      filename <- gsub(" " ,"_", gsub(':', "_", results.david.data.frame[i,"Term"] ))
      filename <- paste0(filename, ".txt")
      filanamepath <- file.path(david.db.path, filename)
      write.table(x=gene.list, file=filanamepath, sep = "\n", row.names = FALSE, col.names = FALSE, quote = FALSE)
      cat('Pathway file ', filename, ' written on disk!\n')
    }
  } else {
    cat("David Results empty! Sorry! Something went wrong...\n")
  }
}

DavidAnalysis <- function(main.window=NULL, file.name, gene.list.position, gene.identifier, list.type, list.name, 
                                 out.file.name, db.selected, analysis.type, Project, specie, filter.list) {
  require("RDAVIDWebService")
  require("biomaRt")
#   
# ##   /home/dario/Scrivania/Dropbox/Lavori/IAC/coding/davidquery/data/union counts.txt_results_DESeq.txt
#     file.name <- '/home/dario/RNASeqGUI_Projects/a/Results/union counts.txt_results_DESeq.txt'
# #       file.name <- '/home/dario/Scrivania/Dropbox/Lavori/IAC/coding/david_UI/davidquery_bkup/data/gene_symbol_list.txt'
# # #   #   /Users/majin/Desktop/Dropbox/Lavori/IAC/coding/davidquery/data/union counts.txt_results_DESeq.txt
#     main.window=NULL
#     gene.list.position <- 1
#     gene.identifier <- 'ENSEMBL_GENE_ID'
#     list.type <- 'Gene'
#     list.name <- 'listaa'
#     out.file.name <- 'prova.txt'
#     db.selected <- 'BBID'#'KEGG_PATHWAY'
#     analysis.type <- 'Pathway'
#     Project <- 'a'
#     specie = 'HUMAN'
##   specie=""
  
  
  if(Sys.info()[[1]]=='Windows'){
    sys.sep='\\'
  } else {
    sys.sep='/'
  }
  
  sep.pos <- gregexpr(pattern = sys.sep, file.name, fixed=F)[[1]]
  out.file.name <- substring(file.name, sep.pos[length(sep.pos)]+1)
  
  david.path <- file.path("RNASeqGUI_Projects", Project, "Results", "DAVID")
  david.db.path <- file.path(david.path, paste(db.selected, collapse = "_", sep = ""))
  
  dir.create(david.path, recursive = TRUE)
  
  if(analysis.type=='Pathway'){
    cat('Performing pathway analysis on ', db.selected, '...\n')
    this.name <- paste0(out.file.name, '_DAVID_Pathway_', db.selected)
    out.file.name <- paste0(david.path, sys.sep, this.name, '_results.txt')
    # out.file.name <- paste0(david.path, sys.sep, out.file.name, '_DAVID_Pathway_results_', db.selected,'.txt')
    # out.file.name <- paste0(file.path("RNASeqGUI_Projects",Project,"Results"),sys.sep, out.file.name, '_DAVID_Pathway_results_', db.selected,'.txt') 
#     out.file.name <- paste0( out.file.name, '_DAVID_Pathway_results_', db.selected,'.csv') ##testing
    # dbname <- paste0('david_', this.name, '_db')
  }else{
    if(length(grep('ALL', db.selected))!=0) {
      ## ALL case
      cat('Performing ALL GO analysis on ', db.selected, '...\n')
      this.name <- paste0(out.file.name, '_DAVID_ALL_GO_', paste(db.selected, collapse = "_", sep = ""))
      out.file.name <- paste0(david.path, sys.sep, this.name,'_results.txt')
      # out.file.name <- paste0(david.path, sys.sep, out.file.name, '_DAVID_ALL_GO_', paste(db.selected, collapse = "_", sep = ""),'_results_', '.txt')
      # out.file.name <- paste0(file.path("RNASeqGUI_Projects",Project,"Results"), sys.sep, out.file.name, '_DAVID_ALL_GO_results_', '.txt')
#       out.file.name <- paste0( out.file.name, '_DAVID_ALL_GO_results_', '.csv') ##testing
      # dbname <- paste0('david_all_go_db')
    } else {
      ## FAT case
      cat('Performing FAT GO analysis on ', db.selected, '...\n')
      this.name <- paste0(out.file.name, '_DAVID_FAT_GO_', paste(db.selected, collapse = "_", sep = ""))
      out.file.name <- paste0(david.path, sys.sep, this.name, '_DAVID_FAT_GO_results.txt')
      # out.file.name <- paste0(david.path, sys.sep, out.file.name, '_DAVID_FAT_GO_results_', '.txt')
      # out.file.name <- paste0(file.path("RNASeqGUI_Projects",Project,"Results"), sys.sep, out.file.name, '_DAVID_FAT_GO_results_', '.txt')
#       out.file.name <- paste0( out.file.name, '_DAVID_FAT_GO_results_', '.csv') ##testing
      # dbname <- paste0('david_fat_go_db')
    }
  }

  dbname <- paste0('david_', this.name, '_db')
  
  
  db <- InitDb(db.name=dbname, db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
#   db <- InitDb(db.name=dbname, db.path=file.path("cache")) ##testing

  SaveInCache(db, file.name, "filename_key")
  SaveInCache(db, Project,  "project_key")
  SaveInCache(db, gene.list.position, "genelistposition_key")
  SaveInCache(db, gene.identifier, "geneidentifier_key")
  SaveInCache(db, list.type, "listtype_key")
  SaveInCache(db, list.name, "listname_key")
  SaveInCache(db, db.selected, "dbselected_key")
  SaveInCache(db, analysis.type, "analysistype_key")
  SaveInCache(db, out.file.name, "outfilename_key")
  SaveInCache(db, specie, "specie_key")
  
  id.list <- read.table(file = file.name, header = TRUE, as.is = TRUE)
  
  #if(dim(id.list)[2]>1) {
    id.list <- id.list[, gene.list.position, drop=TRUE]
   #} 
  # else {
  #   id.list <- as.list(id.list[1])
  # }
  
  if(gene.identifier=='GENCODE') {
    ## cutting the alternative splicing identification of the gencode forcing the ensembl_gene identifier
    id.list <- unlist(lapply(id.list, function(x) {substring(x, 0, regexpr(pattern = '.', x, fixed=T)[1]-1)}))
    
    gene.identifier <- 'ENSEMBL_GENE_ID'
  }
  
  if(gene.identifier=='GENE_SYMBOL') {
    if(specie!="") {
      print("converting gene_symbol to ensembl_gene_id ... ")
      specie.list <- GetSpecieCode(specie)
      print(specie.list$dataset)
      ensembl <- useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset=specie.list$dataset)

      ensembl.map <- getBM(attributes = c("external_gene_name","ensembl_gene_id"), mart=ensembl)

      id.list <- ConvertList(id.list, ensembl.map)

      gene.identifier <- 'ENSEMBL_GENE_ID'
      print("done!")
    }else{
      message('ERROR: empty Specie')
    }
  }

  print('check if list is too short')
  
  if(length(id.list)>3000) {
    MyDialog(parent.w=main.window, type.d='warning', message=paste0('You submitted ', length(id.list),' genes!\nBecause of DAVID limitations, the gene list length has been cut to 3000 genes'), title='WARNING')
    message(paste0('WARNING: You submitted ', length(id.list),' genes!\nBecause of DAVID limitations, the gene list length has been cut to 3000 genes'))
    id.list <- id.list[1:3000]
  } else {
    MyDialog(parent.w=main.window, type.d='info', message=paste0('You submitted ', length(id.list),' genes!'), title='INFORMATION')
    print(paste0('You submitted ', length(id.list),' genes!'))
  }
  print(head(id.list))
  
  SaveInCache(db, id.list, "idlist_key")

  print('Querying DAVID...')
  w.wind <- WaitingWindow(message='Please wait while querying DAVID', parent.w=main.window)
  Sys.sleep(1)

  david <- DavidConnect()

  if(is.connected(david)) {
    result <- addList(david, id.list, idType=gene.identifier, listName=list.name, listType=list.type)
    
    setAnnotationCategories(david, db.selected)
    
    results.final <- getFunctionalAnnotationChart(david)
    w.wind$destroy()
    print('done!')
    #print(dim(results.final))
    if(dim(results.final)[1]!=0) {
      if(filter.list$flag) {
        # results.final <- FilterDavidResults(results.final, filter.list)
        results.final <- results.final[results.final[ , filter.list$col.name]< filter.list$thr, ]
      }
      #print("diverso da zero")
      SaveInCache(db, results.final, "resultsfinal_key")
      write.table(results.final, file=out.file.name, quote=FALSE, row.names=FALSE,sep="\t")
      cat('File ', out.file.name, ' written on disk!\n')
      print(david.db.path)
      input.file.name <- substring(file.name, max(gregexpr("/",file.name)[[1]])+1)
      input.file.name <- gsub(".txt", "", input.file.name)
      input.file.name <- gsub(" ", "_", input.file.name)
      
      SavePathwaysList(results.final, david.db.path, just.file.name = input.file.name)
    } else {
      #print("uguale 0")
      results.final <- data.frame()
      SaveInCache(db, results.final, "resultsfinal_key")
      cat('David produced no Results on ', db.selected, '\n')
    }
    
    
  } else {
    w.wind$destroy()
    message("ERROR: DAVID disconnected!")
  }
  
  PrintDavidReport(db, dbname) ##testing!!

  return(results.final)
  
}



PrintDavidReport <- function(cache.db.obj, db.cache.name) {
  cat('processing david report...\n')
  if(Sys.info()[[1]]=="Windows"){ ##only separator changes
    sys.sep='\\'  
  }else{         # Unix
    sys.sep='/'
  }
  
  db <- cache.db.obj
  Project <- LoadCachedObject(db, 'project_key')
  
  file.name <- LoadCachedObject(db, "filename_key")
  Project <- LoadCachedObject(db, "project_key")
  gene.list.position <- LoadCachedObject(db, "genelistposition_key")
  gene.identifier <- LoadCachedObject(db, "geneidentifier_key")
  list.type <- LoadCachedObject(db, "listtype_key")
  list.name <- LoadCachedObject(db, "listname_key")
  db.selected <- LoadCachedObject(db, "dbselected_key")
  analysis.type <- LoadCachedObject(db, "analysistype_key")
  out.file.name <- LoadCachedObject(db, "outfilename_key")
  specie <- LoadCachedObject(db, "specie_key")
  
  
  report <- paste0(file.path("RNASeqGUI_Projects", Project, "Logs"), sys.sep, "report.Rmd")
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report, ncolumns = if(is.character(message5)) 1 else 5, append = TRUE, sep = "\n")
  
  message2 <- paste(" * In the *Gene-Set/Pathway Interface*, you clicked the **David** button at `", Sys.time(),"` and the DAVID_*_result_*.csv files has been saved in the `", Project,"/Results` folder.", sep="")
  write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message2 <- paste("You chose the following file: `", file.name, 
                    "`, gene.list.position: `", gene.list.position,
                    "`, gene.identifier: `", gene.identifier,
                    "`, list.type: `", list.type,
                    "`, list.name: `",list.name,
                    "`, db.selected: `",db.selected,
                    "`, analysis.type: `",analysis.type,
                    "`, specie: `",specie,
                    "`, Project: `",Project,"`",sep="\n")
  ##aggiungere flag di filtering
  write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" The list of called functions:",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" ```{r} ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message2 <- paste("GetSpecieCode",sep="")
  write(message2, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message2 <- paste("ConvertList",sep="")
  write(message2, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message2 <- paste("DavidConnect",sep="")
  write(message2, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message2 <- paste("DavidAnalysis",sep="")
  write(message2, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message8 <- paste(" ``` ",sep="\n")
  write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" This R code has been run:",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" ```{r} ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("print(getwd())", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste(" db <- InitDb(db.name='", db.cache.name, "' , db.path='cache')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("#results.data.frame <- DavidAnalysis(file.name, gene.list.position, gene.identifier, list.type, list.name, out.file.name, db.selected, analysis.type, Project)",sep='')  
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("results.data.frame <- LoadCachedObject(db, 'resultsfinal_key')",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("print('First lines of the results:')",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("print(head(results.data.frame))",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#   message5 <- paste("cat('Results saved on file: `", out.file.name"`\n')")
#   write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message8 <- paste(" ``` ",sep="\n")
  write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
  
  print('done!')
}

# DavidFunctionalAnnotationChart("a",NULL)


DESeq <- function(x, conditions, libTypes, treated, control, the.file, pvaladj, Project){

  require(DESeq)

  if(Sys.info()[[1]]=="Windows"){
    a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
    the.file2 = strsplit(the.file,"\\\\")    
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile   
    sys.sep="\\"
  } else { #Linux
    a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")    
    the.file2 = strsplit(the.file,"/")    
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile    
    sys.sep="/"
  }

  db <- InitDb(db.name=paste(the.file2,'deseq_db',sep="_"), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(db, the.file, "the_file_key")
  SaveInCache(db, Project, "project_key")
  SaveInCache(db, x, "maindataframe_key")
  SaveInCache(db, conditions, "conditions_key")
  SaveInCache(db, libTypes, "libtypes_key")
  SaveInCache(db, treated, "treated_key")
  SaveInCache(db, control, "control_key")
  
  pvaladj = as.numeric(pvaladj)
  SaveInCache(db, pvaladj, "pvaladj_key")

  pasillaCountTable = x
  
  SaveInCache(db, pasillaCountTable, "pasillacounttable_key")

  print(head(pasillaCountTable))

  pasillaDesign = data.frame(row.names=colnames(pasillaCountTable), condition=conditions, libType=libTypes)
  
  SaveInCache(db, pasillaDesign, "pasilladesign_key")

  cds = DESeq::newCountDataSet( pasillaCountTable, pasillaDesign )
  SaveInCache(db, cds, "cds_key")

  countTable = pasillaCountTable

  print("You loaded this count file: ")
  print(head(pasillaCountTable))

  condition = pasillaDesign$condition

  cds = DESeq::newCountDataSet( countTable, condition )
  SaveInCache(db, cds, "cdsnew_key")
  
  cds = estimateSizeFactors( cds )
  SaveInCache(db, cds, "cdsest_key")
  
  size = sizeFactors( cds )
  SaveInCache(db, size, "size_key")
  
  print("Size Factors: ")
  print(size)

  if( length(conditions) < 4 ){

    print("No replicates available. Using blind method.")
    cds = estimateDispersions( cds, method="blind", sharingMode="fit-only" )

  }else{

    cds = estimateDispersions( cds )

  }
  
  SaveInCache(db, cds, "cdsdisp_key")

  DESeq::plotDispEsts( cds ) # Plot Estimate Dispersion

  if(Sys.info()[[1]]=="Windows"){
    a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
    the.file2 = strsplit(the.file,"\\\\")    
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile   
    outputName=paste(the.file2,"_Dispersion_DESeq.pdf", sep="")    
    b=paste(a,outputName,sep="\\")    
    dev.print(device = pdf, file=b)

  } else { #Linux
    a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")    
    the.file2 = strsplit(the.file,"/")    
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile    
    outputName=paste(the.file2,"_Dispersion_DESeq.pdf", sep="")   
    b=paste(a,outputName,sep="")    
    dev.print(device = pdf, file=b)

  }


  ################################################################## ANALISYS ###################################

  res1 = DESeq::nbinomTest(cds, control, treated)
  SaveInCache(db, res1, "res1_key")
  
  print("First five lines of the results.")
  print( head(res1) )

  a=paste(substring(getwd(),1,nchar(getwd())),"RNASeqGUI_Projects",Project,"Results",sep=sys.sep)
  outputName=paste(the.file2,"_results_DESeq.txt", sep="")
  b=paste(a,outputName,sep=sys.sep)
  write.table( res1, file = b , quote=FALSE, sep="\t", row.names=FALSE)
  outputName=paste(the.file2,"_results_DESeq.tsv", sep="")
  b=paste(a,outputName,sep=sys.sep)
  write.table( res1, file = b , quote=FALSE, sep="\t", row.names=FALSE)
  list_DE_DESEQ = subset(res1, padj < pvaladj) # select significant genes
  SaveInCache(db, list_DE_DESEQ, "listdedeseq_key")

  list_rest_of_genes_DESEQ = subset(as.data.frame(res1), (padj >= pvaladj) | (baseMean == 0) ) # select rest of genes
  list_UP_DESEQ = subset(list_DE_DESEQ, list_DE_DESEQ$log2FoldChange > 0)
  list_DOWN_DESEQ = subset(list_DE_DESEQ, list_DE_DESEQ$log2FoldChange < 0)
  num_up   = dim(list_UP_DESEQ)[1]
  num_down = dim(list_DOWN_DESEQ)[1]
  rest     = dim(list_rest_of_genes_DESEQ)[1]
  slices <- c(num_up, num_down, rest)
  lbls <- c(paste(num_up,' UP Genes',sep=''), paste(num_down,' DOWN Genes',sep=''), paste(rest,' remaining Genes',sep=''))
  pie3D(slices,labels=lbls,explode=0.1, main='Pie Chart of DE Genes')

  outputName=paste(the.file2,"_padj=",pvaladj,"_DE_genes_DESEQ.txt", sep="")
  b=paste(a,outputName,sep=sys.sep)
  write.table(list_DE_DESEQ, file = b , quote=FALSE, sep="\t", row.names=FALSE) 
  outputName=paste(the.file2,"_padj=",pvaladj,"_DE_genes_DESEQ.tsv", sep="")
  b=paste(a,outputName,sep=sys.sep)
  write.table(list_DE_DESEQ, file = b , quote=FALSE, sep="\t", row.names=FALSE)   
  print(" FINISHED ! ")
  
  message=paste("Results of DESeq have been saved in the Results folder of ",Project," project!", sep="")
  print(message)
  
  if(Sys.info()[[1]]=="Windows"){
  #write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message2 <- paste(" * In the *DESeq Interface*, you clicked the **Run DESeq** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Results` folder.", sep="")
  write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message2 <- paste("You chose the following count file: `",the.file2,"`, padj: `",pvaladj,"`, Project: `",Project,"`, treated='",treated,"', control='",control,"', ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")   
    message1 <- paste("Factors= c(")
    write(message1, file = report,ncolumns = if(is.character(message1)) 1 else 5,append = TRUE, sep = "")   
    message2 <- paste("'",conditions[1:(length(conditions)-1)],"',",sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message3 <- paste("'",conditions[length(conditions)],"'",sep="")
    write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "")    
    message4 <- paste("), ")
    write(message4, file = report,ncolumns = if(is.character(message4)) 1 else 5,append = TRUE, sep = "")    
    message1 <- paste("LibTypes= c(")
    write(message1, file = report,ncolumns = if(is.character(message1)) 1 else 5,append = TRUE, sep = "")   
    message2 <- paste("'",libTypes[1:(length(libTypes)-1)],"',",sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message3 <- paste("'",libTypes[length(libTypes)],"'",sep="")
    write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "")    
    message4 <- paste("). ")
    write(message4, file = report,ncolumns = if(is.character(message4)) 1 else 5,append = TRUE, sep = "")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("require(DESeq)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("db <- InitDb(db.name=paste(the.file2,'deseq_db',sep='_'), db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("x <- LoadCachedObject(db, 'maindataframe_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file <- LoadCachedObject(db, 'the_file_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("Project <- LoadCachedObject(db, 'project_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("conditions <- LoadCachedObject(db, 'conditions_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("libtypes <- LoadCachedObject(db, 'libtypes_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("treated <- LoadCachedObject(db, 'treated_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("control <- LoadCachedObject(db, 'control_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("pvaladj <- LoadCachedObject(db, 'pvaladj_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("pasillaCountTable <- LoadCachedObject(db, 'pasillacounttable_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#pasillaDesign = data.frame(row.names=colnames(pasillaCountTable), condition=conditions, libType=libTypes)",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("pasillaDesign <- LoadCachedObject(db, 'pasilladesign_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#cds = DESeq::newCountDataSet( pasillaCountTable, pasillaDesign )",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("cds <- LoadCachedObject(db, 'cds_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("countTable = pasillaCountTable",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("print('You loaded this count file: ')",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("print(head(pasillaCountTable))",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("condition = pasillaDesign$condition",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#cds = DESeq::newCountDataSet( countTable, condition )",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("cds <- LoadCachedObject(db, 'cdsnew_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#cds = estimateSizeFactors( cds )",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("cds <- LoadCachedObject(db, 'cdsest_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#size = sizeFactors( cds )",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("size <- LoadCachedObject(db, 'size_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#cds = estimateDispersions( cds )",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("cds <- LoadCachedObject(db, 'cdsdisp_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("DESeq::plotDispEsts( cds ) # Plot Estimate Dispersion",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")     
    message5 <- paste("################################################################## ANALISYS ###################################",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")  
    message5 <- paste("#res1 = DESeq::nbinomTest(cds, control, treated)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("res1 <- LoadCachedObject(db, 'res1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")       
    message5 <- paste("#list_DE_DESEQ = subset(res1, padj < pvaladj) # select significant genes",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("list_DE_DESEQ <- LoadCachedObject(db, 'listdedeseq_key')", sep="")    
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("print('First five lines of the results.')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("print( head(res1) )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
  res1

		}else{ #Linux

    #write into the project report
    report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message2 <- paste(" * In the *DESeq Interface*, you clicked the **Run DESeq** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Results` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message2 <- paste("You chose the following count file: `",the.file2,"`, padj: `",pvaladj,"`, Project: `",Project,"`, treated='",treated,"', control='",control,"', ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")   
    message1 <- paste("Factors= c(")
    write(message1, file = report,ncolumns = if(is.character(message1)) 1 else 5,append = TRUE, sep = "")   
    message2 <- paste("'",conditions[1:(length(conditions)-1)],"',",sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message3 <- paste("'",conditions[length(conditions)],"'",sep="")
    write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "")    
    message4 <- paste("), ")
    write(message4, file = report,ncolumns = if(is.character(message4)) 1 else 5,append = TRUE, sep = "")    
    message1 <- paste("LibTypes= c(")
    write(message1, file = report,ncolumns = if(is.character(message1)) 1 else 5,append = TRUE, sep = "")   
    message2 <- paste("'",libTypes[1:(length(libTypes)-1)],"',",sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message3 <- paste("'",libTypes[length(libTypes)],"'",sep="")
    write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "")    
    message4 <- paste("). ")
    write(message4, file = report,ncolumns = if(is.character(message4)) 1 else 5,append = TRUE, sep = "")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("require(DESeq)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("db <- InitDb(db.name=paste(the.file2,'deseq_db',sep='_'), db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("x <- LoadCachedObject(db, 'maindataframe_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file <- LoadCachedObject(db, 'the_file_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("Project <- LoadCachedObject(db, 'project_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("conditions <- LoadCachedObject(db, 'conditions_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("libtypes <- LoadCachedObject(db, 'libtypes_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("treated <- LoadCachedObject(db, 'treated_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("control <- LoadCachedObject(db, 'control_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("pvaladj <- LoadCachedObject(db, 'pvaladj_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("pasillaCountTable <- LoadCachedObject(db, 'pasillacounttable_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#pasillaDesign = data.frame(row.names=colnames(pasillaCountTable), condition=conditions, libType=libTypes)",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("pasillaDesign <- LoadCachedObject(db, 'pasilladesign_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#cds = DESeq::newCountDataSet( pasillaCountTable, pasillaDesign )",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("cds <- LoadCachedObject(db, 'cds_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("countTable = pasillaCountTable",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("print('You loaded this count file: ')",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("print(head(pasillaCountTable))",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("condition = pasillaDesign$condition",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#cds = DESeq::newCountDataSet( countTable, condition )",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("   cds <- LoadCachedObject(db, 'cdsnew_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#cds = estimateSizeFactors( cds )",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("cds <- LoadCachedObject(db, 'cdsest_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#size = sizeFactors( cds )",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("size <- LoadCachedObject(db, 'size_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#cds = estimateDispersions( cds )",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("cds <- LoadCachedObject(db, 'cdsdisp_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("DESeq::plotDispEsts( cds ) # Plot Estimate Dispersion",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")     
    message5 <- paste("################################################################## ANALISYS ###################################",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")  
    message5 <- paste("#res1 = DESeq::nbinomTest(cds, control, treated)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("res1 <- LoadCachedObject(db, 'res1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")       
    message5 <- paste("#list_DE_DESEQ = subset(res1, padj < pvaladj) # select significant genes",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("list_DE_DESEQ <- LoadCachedObject(db, 'listdedeseq_key')", sep="")    
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("print('First five lines of the results.')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("print( head(res1) )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    res1

  }

}

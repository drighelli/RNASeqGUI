DESeqComplexDesign <- function(x, conditions, libTypes, the.file, pvaladj, Project){

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

  res1=NULL
 
  pvaladj = as.numeric(pvaladj)
 
  pasillaCountTable = x

  db <- InitDb(db.name=paste(the.file2,'deseqcomplex_db',sep="_"), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(db, the.file, "the_file_key")
  SaveInCache(db, Project, "project_key")
  SaveInCache(db, x, "maindataframe_key")
  SaveInCache(db, conditions, "conditions_key")
  SaveInCache(db, libTypes, "libtypes_key")
  SaveInCache(db, pvaladj, "pvaladj_key")
  SaveInCache(db, pasillaCountTable, "pasillacounttable_key")

  print("You loaded this count file: ")
  print(head(pasillaCountTable))


  pasillaDesign = data.frame(row.names=colnames(pasillaCountTable), condition=conditions, libType=libTypes)
  #pasillaDesign = data.frame(row.names=colnames(pasillaCountTable), condition=c("treated","treated","control","control"),libType=c("paired-end","paired-end","paired-end","paired-end"))
  SaveInCache(db, pasillaDesign, "pasilladesign_key")

  print("with this design file: ")
  print(pasillaDesign)

  cds = DESeq::newCountDataSet( pasillaCountTable, pasillaDesign )
  SaveInCache(db, cds, "cds_key")

  countTable = pasillaCountTable
  SaveInCache(db, countTable, "counttable_key")

  condition = pasillaDesign$condition
  SaveInCache(db, condition, "condition_key")

  cds = DESeq::newCountDataSet( countTable, condition )
  SaveInCache(db, cds, "cdsnew_key")

  cds = estimateSizeFactors( cds )
  SaveInCache(db, cds, "cdsestfact_key")

  size = sizeFactors( cds )
  SaveInCache(db, size, "size_key")
  
  print("Size Factors: ")
  print(size)

  cds = DESeq::estimateDispersions(cds, method = "pooled-CR", modelFormula = count ~ libTypes + condition)
  SaveInCache(db, cds, "cdsestdisp_key")
  
  fit1 = DESeq::fitNbinomGLMs(cds, count ~ libTypes + condition)
  SaveInCache(db, fit1, "fit1_key")

  fit0 = DESeq::fitNbinomGLMs(cds, count ~ libTypes)
  SaveInCache(db, fit0, "fit0_key")
  
  pval = nbinomGLMTest(fit1, fit0)
  SaveInCache(db, pval, "pval_key")
  
  padj = p.adjust(pval, method ="BH")
  SaveInCache(db, padj, "padj_key")
  
  res1=cbind(fit1, pval=pval, padj=padj)
  SaveInCache(db, res1, "res1_key")
  
  print("First five lines of the results.")
  print( head(res1) )

  DESeq::plotDispEsts( cds ) # Plot Estimate Dispersion

  if(Sys.info()[[1]]=="Windows") {
    a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
    the.file = strsplit(the.file,"\\\\")
    the.file = the.file[[1]][length(the.file[[1]])]  #estract the namefile
    outputName=paste(the.file,"_Dispersion_DESeqComplexDesign.pdf", sep="")
    b=paste(a,outputName,sep="\\")
    sys.sep='\\'
  }else{
    a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
    the.file2 = strsplit(the.file,"/")
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
    outputName = paste(the.file2,"_Dispersion_DESeqComplexDesign.pdf", sep="")
    b = paste(a, outputName, sep="/")
    sys.sep='/'
  }
  
  dev.print(device = pdf, file=b)
  
  ################################################################## ANALISYS ###################################
  a=paste(getwd(), "RNASeqGUI_Projects", Project, "Results", sep=sys.sep)

  res1$id <- rownames(res1)
  res1<-(res1[,c(8,1,2,3,4,5,6,7)])
  
  outputName=paste(the.file2, "_results_DESeqComplexDesign.txt", sep="")
  b=paste(a, outputName, sep=sys.sep)
  write.table( res1, file = b , quote=FALSE, sep="\t", row.names=FALSE)

  outputName=paste(the.file2, "_results_DESeqComplexDesign.tsv", sep="")
  b=paste(a, outputName, sep=sys.sep)
  write.table( res1, file = b , quote=FALSE, sep="\t", row.names=FALSE)

  list_DE_DESEQ = subset(res1, padj < pvaladj) # select significant genes
  SaveInCache(db, list_DE_DESEQ, "listdedeseq_key")
  
  outputName=paste(the.file2,"_padj=",pvaladj,"_DE_genes_DESeqComplexDesign.txt", sep="")
  b=paste(a,outputName,sep=sys.sep)
  write.table(list_DE_DESEQ, file = b , quote=FALSE, sep="\t", row.names=FALSE)  

  outputName=paste(the.file2,"_padj=",pvaladj,"_DE_genes_DESeqComplexDesign.tsv", sep="")
  b=paste(a,outputName,sep=sys.sep)
  write.table(list_DE_DESEQ, file = b , quote=FALSE, sep="\t", row.names=FALSE) 

  message=paste("Results of DESeq have been saved in the Results folder of ", Project," project!", sep="")
  print(message)

  if(Sys.info()[[1]]=="Windows"){
    #write into the project report
    report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *DESeq Interface*, you clicked the **Run DESeqComplexDesign** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Results` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message2 <- paste("You chose the following count file: `",the.file2,"`, padj: `",pvaladj,"`, Project: `",Project,"`, ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")    
    message1 <- paste("Factors= c(")
    write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message2 <- paste("'",conditions[1:(length(conditions)-1)],"',",sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message3 <- paste("'",conditions[length(conditions)],"'",sep="")
    write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message4 <- paste("), ")
    write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message1 <- paste("LibTypes= c(")
    write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")   
    message2 <- paste("'",libTypes[1:(length(libTypes)-1)],"',",sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message3 <- paste("'",libTypes[length(libTypes)],"'",sep="")
    write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message4 <- paste("). ")
    write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
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
    message5 <- paste("db <- InitDb(db.name=paste(the.file2,'deseqcomplex_db',sep='_'), db.path='cache')", sep="")
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
    message5 <- paste("#countTable = pasillaCountTable",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("countTable <- LoadCachedObject(db, 'counttable_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")  
    message5 <- paste("print('You loaded this count file: ')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print(head(pasillaCountTable))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("#condition = pasillaDesign$condition",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("condition <- LoadCachedObject(db, 'condition_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#cds = DESeq::newCountDataSet( countTable, condition )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("cds <- LoadCachedObject(db, 'cdsnew_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#cds = estimateSizeFactors( cds )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("cds <- LoadCachedObject(db, 'cdsestfact_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#size = sizeFactors( cds )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("size <- LoadCachedObject(db, 'size_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#cds = DESeq::estimateDispersions(cds, method = 'pooled-CR', modelFormula = count ~ libTypes + condition)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("cds <- LoadCachedObject(db, 'cdsestdisp_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#fit1 = DESeq::fitNbinomGLMs(cds, count ~ libTypes + condition)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("fit1 <- LoadCachedObject(db, 'fit1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#fit0 = DESeq::fitNbinomGLMs(cds, count ~ libTypes)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("fit0 <- LoadCachedObject(db, 'fit0_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#pval = nbinomGLMTest(fit1, fit0)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("pval <- LoadCachedObject(db, 'pval_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#padj = p.adjust(pval, method ='BH')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("padj <- LoadCachedObject(db, 'padj_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#res1=cbind(fit1, pval=pval, padj=padj)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("res1 <- LoadCachedObject(db, 'res1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("print('First five lines of the results.')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("print( head(res1) )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("DESeq::plotDispEsts( cds ) # Plot Estimate Dispersion",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
  

		}else{ # Linux

    report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message2 <- paste(" * In the *DESeq Interface*, you clicked the **Run DESeqComplexDesign** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Results` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message2 <- paste("You chose the following count file: `",the.file2,"`, padj: `",pvaladj,"`, Project: `",Project,"`, ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")    
    message1 <- paste("Factors= c(")
    write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message2 <- paste("'",conditions[1:(length(conditions)-1)],"',",sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message3 <- paste("'",conditions[length(conditions)],"'",sep="")
    write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message4 <- paste("), ")
    write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message1 <- paste("LibTypes= c(")
    write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")   
    message2 <- paste("'",libTypes[1:(length(libTypes)-1)],"',",sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message3 <- paste("'",libTypes[length(libTypes)],"'",sep="")
    write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message4 <- paste("). ")
    write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
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
    message5 <- paste("db <- InitDb(db.name=paste(the.file2,'deseqcomplex_db',sep='_'), db.path='cache')", sep="")
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
    message5 <- paste("#countTable = pasillaCountTable",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("countTable <- LoadCachedObject(db, 'counttable_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")  
    message5 <- paste("print('You loaded this count file: ')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print(head(pasillaCountTable))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("#condition = pasillaDesign$condition",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("condition <- LoadCachedObject(db, 'condition_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#cds = DESeq::newCountDataSet( countTable, condition )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("cds <- LoadCachedObject(db, 'cdsnew_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#cds = estimateSizeFactors( cds )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("cds <- LoadCachedObject(db, 'cdsestfact_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#size = sizeFactors( cds )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("size <- LoadCachedObject(db, 'size_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#cds = DESeq::estimateDispersions(cds, method = 'pooled-CR', modelFormula = count ~ libTypes + condition)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("cds <- LoadCachedObject(db, 'cdsestdisp_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#fit1 = DESeq::fitNbinomGLMs(cds, count ~ libTypes + condition)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("fit1 <- LoadCachedObject(db, 'fit1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#fit0 = DESeq::fitNbinomGLMs(cds, count ~ libTypes)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("fit0 <- LoadCachedObject(db, 'fit0_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#pval = nbinomGLMTest(fit1, fit0)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("pval <- LoadCachedObject(db, 'pval_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#padj = p.adjust(pval, method ='BH')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("padj <- LoadCachedObject(db, 'padj_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#res1=cbind(fit1, pval=pval, padj=padj)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("res1 <- LoadCachedObject(db, 'res1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("print('First five lines of the results.')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("print( head(res1) )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("DESeq::plotDispEsts( cds ) # Plot Estimate Dispersion",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


 }

  res1

}

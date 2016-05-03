NEWDESeq2ComplexDesign <- function(x,conditions,libTypes,the.file,pvaladj,Project){

  require(DESeq2)

  if(Sys.info()[[1]]=="Windows"){
    the.file2 = strsplit(the.file,"\\\\")
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
    sys.sep="\\"
  }else{ #Linux
    the.file2 = strsplit(the.file,"/")
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
    sys.sep="/"
  }

  pvaladj = as.numeric(pvaladj)
 
  countData <- x
  
  db <- InitDb(db.name=paste(the.file2,'deseq2complex_db',sep="_"), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(db, the.file, "the_file_key")
  SaveInCache(db, Project, "project_key")
  SaveInCache(db, x, "maindataframe_key")
  SaveInCache(db, conditions, "conditions_key")
  SaveInCache(db, libTypes, "libtypes_key")
  SaveInCache(db, pvaladj, "pvaladj_key")
  SaveInCache(db, countData, "countdata_key")

  print("You loaded this count file: ")
  print(head(countData))

  colData = data.frame(row.names=colnames(countData), condition=conditions, libType=libTypes)
  # colData = data.frame(row.names=colnames(countData),condition=c("treated","treated","control","control"),libType=c("single-end","paired-end","paired-end","paired-end"))
  
  SaveInCache(db, colData, "coldata_key")

  dds <- DESeqDataSetFromMatrix(countData = countData, colData = colData, design = ~ condition)
  SaveInCache(db, dds, "dds_key")

  #colData(dds)$condition <- factor(colData(dds)$condition, levels=c(control,treated))
  colData <- colData[,c("condition","libType")]
  SaveInCache(db, colData, "coldatafiltered_key")

  ## ----multifactor---------------------------------------------------------
  colData(dds)
  ## ----copyMultifactor-----------------------------------------------------
  ddsMF <- dds
  SaveInCache(db, ddsMF, "ddsmf_key")
  ## ----replaceDesign,cache=TRUE--------------------------------------------
  design(ddsMF) <- formula(~ libType + condition)
  SaveInCache(db, ddsMF, "ddsfmdesign_key")
######################################## ANALISYS ###################################

  ## ----multiResults--------------------------------------------------------
  dds <-  DESeq2::DESeq(ddsMF)
  SaveInCache(db, dds, "ddsdeseq2_key")

  res <- results(dds)
  SaveInCache(db, res, "res_key")

  print("First five lines of the results.")
  print( head(res) )

  ## ----multiTypeResults----------------------------------------------------
  #resMFType <- results(ddsMF, contrast=c("type","single-read","paired-end"))
  #print("Firts five lines of the results.")
  #print( head(resMFType) )


  if(Sys.info()[[1]]=="Windows") {
    a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
    the.file2 = strsplit(the.file,"\\\\") 
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
    sys.sep='\\'
  } else {
    a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",sep="")
    the.file2 = strsplit(the.file,"/")
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
    sys.sep='/'
  }

  message=paste("Results of DESeq2 have been saved in the Results folder of ", Project," project!", sep="")
  print(message)

  res$id <- rownames(res)
  res<-(res[,c(7,1,2,3,4,5,6)])

  outputName=paste(the.file2,"_results_DESeq2ComplexDesign.txt", sep="")
  b=paste(a,outputName,sep=sys.sep)
  write.table( as.data.frame(res), file = b, quote=FALSE, sep="\t", row.names=FALSE)  

  outputName=paste(the.file2,"_results_DESeq2ComplexDesign.tsv", sep="")
  b=paste(a,outputName,sep=sys.sep)
  write.table( as.data.frame(res), file = b, quote=FALSE, sep="\t", row.names=FALSE)  

  list_DE_DESEQ2 = subset(as.data.frame(res), padj < pvaladj) # select significant genes
  SaveInCache(db, list_DE_DESEQ2, "listdedeseq2_key")

  outputName=paste(the.file2,"_padj=",pvaladj,"_DE_genes_DESeq2ComplexDesign.txt", sep="")
  b=paste(a,outputName,sep=sys.sep)
  write.table(list_DE_DESEQ2, file = b , quote=FALSE, sep="\t", row.names=FALSE) 

  outputName=paste(the.file2,"_padj=",pvaladj,"_DE_genes_DESeq2ComplexDesign.tsv", sep="")
  b=paste(a,outputName,sep=sys.sep)
  write.table(list_DE_DESEQ2, file = b , quote=FALSE, sep="\t", row.names=FALSE) 

 
  #PLOTS 

  a=paste(getwd(),"RNASeqGUI_Projects",Project,"Plots",sep=sys.sep)
  outputName=paste(the.file2,"_Dispersion_Local_DESeq2ComplexDesign.pdf", sep="")
  b=paste(a,outputName,sep=sys.sep)
  ddsLocal <- estimateDispersions(dds, fitType="local")
  SaveInCache(db, ddsLocal, "ddslocal_key")

  DESeq2::plotDispEsts(ddsLocal)
  dev.print(device = pdf, file=b)
  dev.off()
  outputName=paste(the.file2,"_Dispersion_Mean_DESeq2ComplexDesign.pdf", sep="")
  b=paste(a,outputName,sep=sys.sep)
  ddsMean <- estimateDispersions(dds, fitType="mean")
  SaveInCache(db, ddsMean, "ddsmean_key")

  DESeq2::plotDispEsts(ddsMean)
  dev.print(device = pdf, file=b)
  dev.off()
  outputName=paste(the.file2,"_Dispersion_DESeq2ComplexDesign.pdf", sep="")
  b=paste(a,outputName,sep=sys.sep)
  DESeq2::plotDispEsts(dds)
  dev.print(device = pdf, file=b)

  if(Sys.info()[[1]]=="Windows") {
    #write into the project report
    report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *DESeq2 Interface*, you clicked the **Run DESeq2ComplexDesign** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Results` folder.", sep="")
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
    message5 <- paste("require(DESeq2)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("db <- InitDb(db.name=paste(the.file2,'deseq2complex_db',sep='_'), db.path='cache')", sep="")
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
    message5 <- paste("countData <- LoadCachedObject(db, 'countdata_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")  
    message5 <- paste("print('You loaded this count file: ')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("print(head(countData))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("#colData = data.frame(row.names=colnames(countData), condition=conditions, libType=libTypes)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("colData <- LoadCachedObject(db, 'coldata_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")  
    message5 <- paste("#dds <- DESeqDataSetFromMatrix(countData = countData, colData = colData, design = ~ condition)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("dds <- LoadCachedObject(db, 'dds_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#colData <- colData[,c('condition','libType')]",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("colData <- LoadCachedObject(db, 'coldatafiltered_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("## ----multifactor---------------------------------------------------------",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("colData(dds)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("## ----copyMultifactor-----------------------------------------------------",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("#ddsMF <- dds",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("ddsMF <- LoadCachedObject(db, 'ddsmf_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("## ----replaceDesign,cache=TRUE--------------------------------------------",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#design(ddsMF) <- formula(~ libType + condition)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("ddsMF <- LoadCachedObject(db, 'ddsfmdesign_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#dds <-  DESeq2::DESeq(ddsMF)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("dds <- LoadCachedObject(db, 'ddsdeseq2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#res <- results(dds)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("res <- LoadCachedObject(db, 'res_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("print('Firts five lines of the results.')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("print( head(res) )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#ddsLocal <- estimateDispersions(dds, fitType='local')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("ddsLocal <- LoadCachedObject(db, 'ddslocal_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("DESeq2::plotDispEsts(ddsLocal)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#ddsMean <- estimateDispersions(dds, fitType='mean')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("ddsMean <- LoadCachedObject(db, 'ddsmean_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("DESeq2::plotDispEsts(ddsMean)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("DESeq2::plotDispEsts(dds)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
           

  }else{  #Linux
  
  #write into the project report
    report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *DESeq2 Interface*, you clicked the **Run DESeq2ComplexDesign** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Results` folder.", sep="")
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
    message5 <- paste("require(DESeq2)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("db <- InitDb(db.name=paste(the.file2,'deseq2complex_db',sep='_'), db.path='cache')", sep="")
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
    message5 <- paste("countData <- LoadCachedObject(db, 'countdata_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")  
    message5 <- paste("print('You loaded this count file: ')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("print(head(countData))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("#colData = data.frame(row.names=colnames(countData), condition=conditions, libType=libTypes)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("colData <- LoadCachedObject(db, 'coldata_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")  
    message5 <- paste("#dds <- DESeqDataSetFromMatrix(countData = countData, colData = colData, design = ~ condition)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("dds <- LoadCachedObject(db, 'dds_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#colData <- colData[,c('condition','libType')]",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("colData <- LoadCachedObject(db, 'coldatafiltered_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("## ----multifactor---------------------------------------------------------",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("colData(dds)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("## ----copyMultifactor-----------------------------------------------------",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("#ddsMF <- dds",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("ddsMF <- LoadCachedObject(db, 'ddsmf_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("## ----replaceDesign,cache=TRUE--------------------------------------------",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#design(ddsMF) <- formula(~ libType + condition)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("ddsMF <- LoadCachedObject(db, 'ddsfmdesign_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#dds <-  DESeq2::DESeq(ddsMF)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("dds <- LoadCachedObject(db, 'ddsdeseq2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#res <- results(dds)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("res <- LoadCachedObject(db, 'res_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("print('Firts five lines of the results.')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("print( head(res) )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#ddsLocal <- estimateDispersions(dds, fitType='local')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("ddsLocal <- LoadCachedObject(db, 'ddslocal_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("DESeq2::plotDispEsts(ddsLocal)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#ddsMean <- estimateDispersions(dds, fitType='mean')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("ddsMean <- LoadCachedObject(db, 'ddsmean_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("DESeq2::plotDispEsts(ddsMean)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("DESeq2::plotDispEsts(dds)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
      
  }

  print(" FINISHED ! ")

  res

}

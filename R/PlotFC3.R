PlotFC3 <- function(name, fdr, results_EdgeR, the.file, Project){
  
  require(edgeR)

  fdr = as.numeric(fdr)
  print ("FDR chosen: ")
  print(fdr)
  print ("Gene Id chosen: ")
  print(name)
  
  if(Sys.info()[[1]]=="Windows"){
    sys.sep='\\'
    the.file2 = strsplit(the.file,"\\\\")
  }else{
    sys.sep='/'
    the.file2 = strsplit(the.file,"/")
  }
  
  a=paste(getwd(),"RNASeqGUI_Projects",Project,"Plots",sep=sys.sep)

    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
    the.file2 = substring(the.file2,1,nchar(the.file2)-4)  # eliminates ".txt"

  db.cache <- InitDb(db.name=paste(the.file2,'plotfc3_db',sep=''), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(db.cache, the.file, "thefile_key")
  SaveInCache(db.cache, Project, "project_key")
  SaveInCache(db.cache, results_EdgeR, "res_key")
  SaveInCache(db.cache, fdr, "fdr_key")
  SaveInCache(db.cache, name, "name_key")

    plot(results_EdgeR$logCPM, results_EdgeR$logFC, col = "black", main="EdgeR Fold Change Plot", xlab="logCPM", ylab="logFC",pch=16,cex=0.4)
    DE_genes_EDGER = subset(results_EdgeR, FDR<fdr)
    points(DE_genes_EDGER$logCPM, DE_genes_EDGER$logFC, pch=19, col='red', cex=0.5)
    
    if (name!=""){
      OneGene = subset(results_EdgeR, row.names(results_EdgeR)==name)
      text(OneGene$logCPM, OneGene$logFC, label=name, col="green", cex=0.6)
    }
    
    outputName=paste(the.file2,"_FoldChange_EdgeR.pdf", sep="")
    b=paste(a,outputName,sep=sys.sep)
    dev.print(device = pdf, file=b)
  
  #write into the project report
  report=paste("RNASeqGUI_Projects",Project,"Logs","report.Rmd",sep=sys.sep)

  if(Sys.info()[[1]]=="Windows"){
   
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *Result Inspection Interface*, you clicked the ** Plot FC** button for EdgeR at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("You chose the following count file: `",the.file,"`, FDR: `",fdr,"`, Project: `",Project,"`, ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2='",the.file2,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("db.cache <- InitDb(db.name=paste(the.file2,'plotfc3_db',sep=''), db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" results_EdgeR <- LoadCachedObject(db.cache, 'res_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file <- LoadCachedObject(db.cache, 'thefile_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" Project <- LoadCachedObject(db.cache, 'project_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" fdr <- LoadCachedObject(db.cache, 'fdr_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" name <- LoadCachedObject(db.cache, 'name_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("print('This file has been loaded: ')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("print(head(results_EdgeR))",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" print ('FDR chosen: ')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" print(fdr)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" print ('Gene Id chosen: ')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" print(name)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("plot(results_EdgeR$logCPM, results_EdgeR$logFC, col = 'black', main='EdgeR Fold Change Plot', xlab='logCPM', ylab='logFC',pch=16,cex=0.4)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("DE_genes_EDGER = subset(results_EdgeR, FDR<fdr)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("points(DE_genes_EDGER$logCPM, DE_genes_EDGER$logFC, pch=19, col='red', cex=0.5)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("if (name!=''){ OneGene = subset(results_EdgeR, row.names(results_EdgeR)==name)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("text(OneGene$logCPM, OneGene$logFC, label=name, col='green', cex=0.6)}",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  }else{ #Linux
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *Result Inspection Interface*, you clicked the **Plot FC** button for EdgeR at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("You chose the following count file: `",the.file,"`, FDR: `",fdr,"`, Project: `",Project,"`, ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2='",the.file2,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("db.cache <- InitDb(db.name=paste(the.file2,'plotfc3_db',sep=''), db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" results_EdgeR <- LoadCachedObject(db.cache, 'res_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file <- LoadCachedObject(db.cache, 'thefile_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" Project <- LoadCachedObject(db.cache, 'project_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" fdr <- LoadCachedObject(db.cache, 'fdr_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" name <- LoadCachedObject(db.cache, 'name_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("print('This file has been loaded: ')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("print(head(results_EdgeR))",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" print ('FDR chosen: ')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" print(fdr)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" print ('Gene Id chosen: ')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" print(name)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" plot(results_EdgeR$logCPM, results_EdgeR$logFC, col = 'black', main='EdgeR Fold Change Plot', xlab='logCPM', ylab='logFC',pch=19,cex=0.3)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" DE_genes_EDGER = subset(results_EdgeR, FDR<fdr)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" points(DE_genes_EDGER$logCPM, DE_genes_EDGER$logFC, pch=19, col='red', cex=0.5)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" if (name!=''){ OneGene = subset(results_EdgeR, row.names(results_EdgeR)==name)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("text(OneGene$logCPM, OneGene$logFC, label=name, col='green', cex=0.6)}",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  }

}



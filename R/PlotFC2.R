PlotFC2 <- function(res, the.file, Project){   #DESeq2
 
  if(Sys.info()[[1]]=="Windows"){
    sys.sep='\\'
    the.file = strsplit(the.file,"\\\\")
  }else{
    sys.sep='/'
    the.file = strsplit(the.file,"/")
  }
  
  a=paste(getwd(),"RNASeqGUI_Projects",Project,"Plots",sep=sys.sep) 
  the.file2 = the.file[[1]][length(the.file[[1]])]  #estract the namefile 
  the.file2 = substring(the.file2,1,nchar(the.file2)-4)  # eliminates ".txt"

  db.cache <- InitDb(paste(the.file2,db.name='_plotfc2_db',sep=''), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))  
  SaveInCache(db.cache, the.file, "thefile_key")
  SaveInCache(db.cache, Project, "project_key")
  SaveInCache(db.cache, res, "res_key")
  
  print('This file has been loaded: ')
  print(head(res))
  
  outputName=paste(the.file2,"_FoldChange_DESeq2.pdf", sep="")
  
  plot(log2(res$baseMean + 1) , res$log2FoldChange ,col = "black", main="DESeq2 Fold Change Plot", xlab='Mean of Normalized Counts', ylab='log2FoldChange',pch=19,cex=0.3) 

  DE_genes = subset(res, padj<0.05)

  points(log2(DE_genes$baseMean + 1) , DE_genes$log2FoldChange, pch=19, col='red', cex=0.5)

  
  a=paste(getwd(),"RNASeqGUI_Projects",Project,"Plots",sep=sys.sep)
  
  b=paste(a,outputName,sep=sys.sep)
  
  dev.print(device = pdf, file=b)

  #dev.off()

  #DESeq2::plotMA(res)

  #outputName=paste(the.file2,"_Alternative_FoldChange_DESeq2.pdf", sep="")
  
  #b=paste(a,outputName,sep=sys.sep)
  
  #dev.print(device = pdf, file=b)
  
  #write into the project report
  report=paste("RNASeqGUI_Projects",Project,"Logs","report.Rmd",sep=sys.sep)
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  if(Sys.info()[[1]]=="Windows"){
    message2 <- paste(" * In the *Result Inspection Interface*, you clicked the **Plot FC** button for DESeq2 at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
  }else{
    message2 <- paste(" * In the *Result Inspection Interface*, you clicked the **Plot FC** button for DESeq at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
  }
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message2 <- paste("You chose the following count file: `",the.file,"`, Project: `",Project,"`, ",sep="\n")
  write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" ```{r} ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2='",the.file2,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("db.cache <- InitDb(paste(the.file2,db.name='_plotfc2_db',sep=''), db.path='cache')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("res <- LoadCachedObject(db.cache, 'res_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("the.file <- LoadCachedObject(db.cache, 'thefile_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste(" Project <- LoadCachedObject(db.cache, 'project_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("print('This file has been loaded: ')",sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("print(head(res))",sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("plot(log2(res$baseMean + 1) , res$log2FoldChange ,col = 'black', main='DESeq2 Fold Change Plot',",sep="") 
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("xlab='Mean of Normalized Counts', ylab='log2FoldChange',pch=19,cex=0.3)",sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("DE_genes = subset(res, padj<0.05)",sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("points(log2(DE_genes$baseMean + 1) , DE_genes$log2FoldChange, pch=19, col='red', cex=0.5)",sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message8 <- paste(" ``` ",sep="\n")
  write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" _______________________________________________________________________ ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
}

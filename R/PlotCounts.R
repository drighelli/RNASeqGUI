PlotCounts <-
function(x,column1,column2,the.file,log,Project){

  if(Sys.info()[[1]]=="Windows"){

  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")

  the.file2 = strsplit(the.file,"\\\\")

  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

  plotcounts.db <- InitDb(paste(the.file2,column1,column2,db.name='plotcounts_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(plotcounts.db, the.file, "the_file_key")
  SaveInCache(plotcounts.db, Project, "project_key")
  SaveInCache(plotcounts.db, x, "plotcounts_dataframe_key")
 
  SaveInCache(plotcounts.db, log, "log_key")
  
  print("You selected this count file:")
  print(head(x))

  column1 = as.numeric(column1)
  column2 = as.numeric(column2)
  
  SaveInCache(plotcounts.db, column1, "column1_key")
  SaveInCache(plotcounts.db, column2, "column2_key")

  outputName = NULL

  x_col1 = paste(the.file2,"$",column1,sep="")

  x_col2 = paste(the.file2,"$",column2,sep="")

  if (log==TRUE) { 

    plot(log((x[,column1]) + 1), log((x[,column2]) +1), main=paste(the.file2,' Log Count Plot',sep=''), xlab=colnames(x)[column1], ylab=colnames(x)[column2], pch=19,cex=0.3)

    outputName=paste(the.file2,"_",colnames(x)[column1],"_vs_",colnames(x)[column2],"_LogPlotCounts.pdf",sep="")

  }

  if (log==FALSE) { 

    plot(x[,column1], x[,column2],main=paste(the.file2,' Count Plot',sep=''), xlab=colnames(x)[column1], ylab=colnames(x)[column2], pch=19,cex=0.3)

    outputName=paste(the.file2,"_",colnames(x)[column1],"_vs_",colnames(x)[column2],"_PlotCounts.pdf",sep="")

  }

  b=paste(a,outputName,sep="")

  abline(a = 0, b = 1, col = 2)
  abline(a = 2, b = 1, col = 3)
  abline(a = -2, b = 1, col = 3)

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
  print(message)

  #write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **Plot Pairs of Counts** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste("You chose the following count file: `",the.file,"`, column1: `",column1,"`, column2: `",column2,"`, log: `",log,"`. ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("column1='",column1,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("column2='",column2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("plotcounts.db <- InitDb(paste(the.file2,column1,column2,db.name='plotcounts_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(plotcounts.db, 'plotcounts_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(plotcounts.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(plotcounts.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("column1 <- LoadCachedObject(plotcounts.db, 'column1_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("column2 <- LoadCachedObject(plotcounts.db, 'column2_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("log <- LoadCachedObject(plotcounts.db, 'log_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Plots\\',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = strsplit(the.file,'\\')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("outputName=paste(the.file2,'_',column1,'_vs_',column2,'_PlotCounts.pdf',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("b=paste(a,outputName,sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x_col1 = paste(the.file2,'$',column1,sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x_col2 = paste(the.file2,'$',column2,sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("if (log==TRUE) { plot(log((x[,column1]) + 1), log((x[,column2]) +1), main=paste(the.file2,' Log Count Plot',sep=''), xlab=colnames(x)[column1], ylab=colnames(x)[column2], pch=19,cex=0.3)}", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("if (log==FALSE) { plot(x[,column1], x[,column2],main=paste(the.file2,' Count Plot',sep=''), xlab=colnames(x)[column1], ylab=colnames(x)[column2], pch=19,cex=0.3) }", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("abline(a = 0, b = 1, col = 2)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("abline(a = 2, b = 1, col = 3)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("abline(a = -2, b = 1, col = 3)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  }else{ #Linux

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")

  the.file2 = strsplit(the.file,"/")

  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

 plotcounts.db <- InitDb(paste(the.file2,column1,column2,db.name='plotcounts_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(plotcounts.db, the.file, "the_file_key")
  SaveInCache(plotcounts.db, Project, "project_key")
  SaveInCache(plotcounts.db, x, "plotcounts_dataframe_key")
 
  SaveInCache(plotcounts.db, log, "log_key")
  
  print("You selected this count file:")
  print(head(x))

  column1 = as.numeric(column1)
  column2 = as.numeric(column2)
  
  SaveInCache(plotcounts.db, column1, "column1_key")
  SaveInCache(plotcounts.db, column2, "column2_key")

  outputName = NULL

  x_col1 = paste(the.file2,"$",column1,sep="")

  x_col2 = paste(the.file2,"$",column2,sep="")

  if (log==TRUE) { 

    plot(log((x[,column1]) + 1), log((x[,column2]) +1), main=paste(the.file2,' Log Count Plot',sep=''), xlab=colnames(x)[column1], ylab=colnames(x)[column2], pch=19,cex=0.3)

    outputName=paste(the.file2,"_",colnames(x)[column1],"_vs_",colnames(x)[column2],"_LogPlotCounts.pdf",sep="")

  }

  if (log==FALSE) { 

    plot(x[,column1], x[,column2],main=paste(the.file2,' Count Plot',sep=''), xlab=colnames(x)[column1], ylab=colnames(x)[column2], pch=19,cex=0.3)

    outputName=paste(the.file2,"_",colnames(x)[column1],"_vs_",colnames(x)[column2],"_PlotCounts.pdf",sep="")

  }

  b=paste(a,outputName,sep="")

  abline(a = 0, b = 1, col = 2)
  abline(a = 2, b = 1, col = 3)
  abline(a = -2, b = 1, col = 3)

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"/Plots folder!", sep="")
  print(message)

  #write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **Plot Pairs of Counts** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste("You chose the following count file: `",the.file,"`, column1: `",column1,"`, column2: `",column2,"`, log: `",log,"`. ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("column1='",column1,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("column2='",column2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("plotcounts.db <- InitDb(paste(the.file2,column1,column2,db.name='plotcounts_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(plotcounts.db, 'plotcounts_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(plotcounts.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(plotcounts.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("column1 <- LoadCachedObject(plotcounts.db, 'column1_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("column2 <- LoadCachedObject(plotcounts.db, 'column2_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("log <- LoadCachedObject(plotcounts.db, 'log_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Plots/',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = strsplit(the.file,'/')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("outputName=paste(the.file2,'_',column1,'_vs_',column2,'_PlotCounts.pdf',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("b=paste(a,outputName,sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x_col1 = paste(the.file2,'$',column1,sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x_col2 = paste(the.file2,'$',column2,sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("if (log==TRUE) { plot(log((x[,column1]) + 1), log((x[,column2]) +1), main=paste(the.file2,", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("' Log Count Plot',sep=''), xlab=colnames(x)[column1], ylab=colnames(x)[column2], pch=19,cex=0.3)}", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("if (log==FALSE) { plot(x[,column1], x[,column2],main=paste(the.file2,' Count Plot',sep=''),", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("xlab=colnames(x)[column1], ylab=colnames(x)[column2], pch=19,cex=0.3) }", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("abline(a = 0, b = 1, col = 2)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("abline(a = 2, b = 1, col = 3)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("abline(a = -2, b = 1, col = 3)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  }

}

meanVariancePlot <-
function(x,the.file,Project){

  
  meanvarianceplot.db <- InitDb(db.name='meanvarianceplot_db', db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(meanvarianceplot.db, the.file, "the_file_key")
  SaveInCache(meanvarianceplot.db, Project, "project_key")
  SaveInCache(meanvarianceplot.db, x, "meanvarianceplot_dataframe_key")

 if(Sys.info()[[1]]=="Windows"){
  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
  the.file2 = strsplit(the.file,"\\\\") 

  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

  outputName=paste(the.file2,"_meanVariancePlot.pdf",sep="")

  b=paste(a,outputName,sep="")

  x=data.matrix(x, rownames.force = NA)
  x=newSeqExpressionSet(x)
  
  SaveInCache(meanvarianceplot.db, x, "newseqexpressionset_key")
  
  meanVarPlot(x,log=TRUE,ylim=c(0,16),main=paste("Mean Variance Plot of ",the.file2))

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
  print(message)

     #write into the project report
     report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.txt",sep="") 
     message2 <- paste("You ckicked the MeanVarPlot button at ", Sys.time()," and the ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message2 <- paste("You chose the following count file: `",the.file," . ",sep="\n")
  write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" This R code has been run:",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" ```{r} ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  
  message5 <- paste("meanvarianceplot.db <- InitDb(db.name='meanvarianceplot_db', db.path='cache')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" x <- LoadCachedObject(meanvarianceplot.db, 'meanvarianceplot_dataframe_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" the.file <- LoadCachedObject(meanvarianceplot.db, 'the_file_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" Project <- LoadCachedObject(meanvarianceplot.db, 'project_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  
  message5 <- paste(" # x = read.table('",the.file,"',header=TRUE,row.names=1)", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" # x=data.matrix(x, rownames.force = NA)", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("  the.file ='",the.file,"'", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("  Project ='", Project,"'", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("   a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Plots\\',sep='')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("   the.file2 = strsplit(the.file,'\\')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("   the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("   outputName=paste(the.file2,'_PlotAllCounts.pdf',sep='')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("   b=paste(a,outputName,sep='')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  
  message5 <- paste(" x <- LoadCachedObject(meanvarianceplot.db, 'newseqexpressionset_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  
  message5 <- paste(" #  x=newSeqExpressionSet(x)", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("   meanVarPlot(x,log=TRUE,ylim=c(0,16),main=paste('Mean Variance Plot of ',the.file2))", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("  # dev.print(device = pdf, file=b)", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message8 <- paste(" ``` ",sep="\n")
  write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" _______________________________________________________________________ ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  }else{ #Linux

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")

  the.file2 = strsplit(the.file,"/")

  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

  outputName=paste(the.file2,"_meanVariancePlot.pdf",sep="")

  b=paste(a,outputName,sep="")

  x=data.matrix(x, rownames.force = NA)
  x=newSeqExpressionSet(x)
  
  SaveInCache(meanvarianceplot.db, x, "newseqexpressionset_key")
  
  meanVarPlot(x,log=TRUE,ylim=c(0,16),main=paste("Mean Variance Plot of ",the.file2))

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"/Plots folder!", sep="")
  print(message)

  #write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **MeanVarPlot** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste("You chose the following count file: `",the.file," . ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

# message5 <- paste("  x = read.table('",the.file,"',header=TRUE,row.names=1)", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("  x=data.matrix(x, rownames.force = NA)", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


message5 <- paste("meanvarianceplot.db <- InitDb(db.name='meanvarianceplot_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" x <- LoadCachedObject(meanvarianceplot.db, 'meanvarianceplot_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" the.file <- LoadCachedObject(meanvarianceplot.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" Project <- LoadCachedObject(meanvarianceplot.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

# message5 <- paste("  the.file ='",the.file,"'", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("  Project ='", Project,"'", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Plots/',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   the.file2 = strsplit(the.file,'/')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   outputName=paste(the.file2,'_PlotAllCounts.pdf',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   b=paste(a,outputName,sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" x <- LoadCachedObject(meanvarianceplot.db, 'newseqexpressionset_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


message5 <- paste(" #  x=newSeqExpressionSet(x)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   meanVarPlot(x,log=TRUE,ylim=c(0,16),main=paste('Mean Variance Plot of ',the.file2))", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  # dev.print(device = pdf, file=b)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 }

}

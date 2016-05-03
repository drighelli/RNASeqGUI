PlotAllCounts <-
function(x,the.file,Project){

  plotallcounts.db <- InitDb(db.name='plotallcounts_db', db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(plotallcounts.db, the.file, "the_file_key")
  SaveInCache(plotallcounts.db, Project, "project_key")
  SaveInCache(plotallcounts.db, x, "plotallcounts_dataframe_key")
  
 if(Sys.info()[[1]]=="Windows"){
  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
  the.file = strsplit(the.file,"\\\\")
  the.file = the.file[[1]][length(the.file[[1]])]  #estract the namefile

  outputName=paste(the.file,"_PlotAllCounts.pdf",sep="")

  b=paste(a,outputName,sep="")

  plot(x,main="Count Plots")

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
  print(message)

  #write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **Plot All Counts** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
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


message5 <- paste("plotallcounts.db <- InitDb(db.name='plotallcounts_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" x <- LoadCachedObject(plotallcounts.db, 'plotallcounts_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" the.file <- LoadCachedObject(plotallcounts.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" Project <- LoadCachedObject(plotallcounts.db, 'project_key')", sep="")
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

message5 <- paste("   plot(x,main='Count Plots')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  }else{ #Linux

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")

  the.file2 = strsplit(the.file,"/")

  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

  outputName=paste(the.file2,"_PlotAllCounts.pdf",sep="")

  b=paste(a,outputName,sep="")

  plot(x,main="Count Plots")

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"/Plots folder!", sep="")
  print(message)

  #write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **Plot All Counts** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
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

message5 <- paste(" plotallcounts.db <- InitDb(db.name='plotallcounts_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" x <- LoadCachedObject(plotallcounts.db, 'plotallcounts_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" the.file <- LoadCachedObject(plotallcounts.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" Project <- LoadCachedObject(plotallcounts.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

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

message5 <- paste("   plot(x,main='Count Plots')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 }

}

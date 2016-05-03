MeanDiffPlot <-
function(x, column1, column2, the.file, Project){

 if(Sys.info()[[1]]=="Windows"){
  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")

  the.file2 = strsplit(the.file2,"\\\\") 
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

  meandiffplot.db <- InitDb(db.name=paste(the.file2,'meandiffplot_db',sep=''), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(meandiffplot.db, the.file, "the_file_key")
  SaveInCache(meandiffplot.db, Project, "project_key")
  SaveInCache(meandiffplot.db, x, "meandiffplot_dataframe_key")
  
  column1 = as.numeric(column1)
  column2 = as.numeric(column2) 
  
  SaveInCache(meandiffplot.db, column1, "column1_key")
  SaveInCache(meandiffplot.db, column2, "column2_key")

  outputName=paste(the.file2,"_",column1,"_vs_",column2,"_MDplot.pdf",sep="")

  b=paste(a,outputName,sep="")

  x_col1 = paste(the.file,"$",column1,sep="")

  x_col2 = paste(the.file,"$",column2,sep="")

  x=data.matrix(x, rownames.force = NA)

  x=newSeqExpressionSet(x)
  
  SaveInCache(meandiffplot.db, x, "newseqexpressionset_key")
 
  MDPlot(x, c(column1,column2)  ,main=paste("MD Plot of columns ",column1," and ",column2," of the file ",the.file2,sep="") )                

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
  print(message)

#write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **MDPlot** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     #message2 <- paste("You chose the following count file: `",the.file,"`, column1: `", column1, "`, column2: `", column2, "`, log: `",log,"`. ",sep="\n")
     #write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2='",the.file2,"'",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("meandiffplot.db <- InitDb(db.name=paste(the.file2,'meandiffplot_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" x <- LoadCachedObject(meandiffplot.db, 'meandiffplot_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" the.file <- LoadCachedObject(meandiffplot.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" Project <- LoadCachedObject(meandiffplot.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" column1 <- LoadCachedObject(meandiffplot.db, 'column1_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" column2 <- LoadCachedObject(meandiffplot.db, 'column2_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Plots\\',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   the.file2 = strsplit(the.file,'\\')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   outputName=paste(the.file2,'_',column1,'_vs_',column2,'_MDplot.pdf',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   b=paste(a,outputName,sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    x_col1 = paste(the.file,'$',column1,sep='') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    x_col2 = paste(the.file,'$',column2,sep='') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" x <- LoadCachedObject(meandiffplot.db, 'newseqexpressionset_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" #   x=data.matrix(x, rownames.force = NA) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  #  x=newSeqExpressionSet(x) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
message5 <- paste("    MDPlot(x, c(column1,column2)  ,main=paste('MD Plot of columns ',column1,' and ',column2,' of the file ',the.file2,sep='') )  ", sep="")    
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")          

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  }else{ #Linux

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")

  the.file2 = strsplit(the.file,"/")

  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

  meandiffplot.db <- InitDb(db.name=paste(the.file2,'meandiffplot_db',sep=''), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(meandiffplot.db, the.file, "the_file_key")
  SaveInCache(meandiffplot.db, Project, "project_key")
  SaveInCache(meandiffplot.db, x, "meandiffplot_dataframe_key")
  
  column1 = as.numeric(column1)
  column2 = as.numeric(column2) 
  
  SaveInCache(meandiffplot.db, column1, "column1_key")
  SaveInCache(meandiffplot.db, column2, "column2_key")

  outputName=paste(the.file2,"_",column1,"_vs_",column2,"_MDplot.pdf",sep="")

  b=paste(a,outputName,sep="")

  x_col1 = paste(the.file,"$",column1,sep="")

  x_col2 = paste(the.file,"$",column2,sep="")

  x=data.matrix(x, rownames.force = NA)

  x=newSeqExpressionSet(x)
  
  SaveInCache(meandiffplot.db, x, "newseqexpressionset_key")
  
  MDPlot(x, c(column1,column2)  ,main=paste("MD Plot of columns ",column1," and ",column2," of the file ",the.file2,sep="") )                

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"/Plots folder!", sep="")
  print(message)

#write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **MDPlot** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     #message2 <- paste("You chose the following count file: `",the.file,"`, column1: `", column1, "`, column2: `", column2, "`, log: `",log,"`. ",sep="\n")
     #write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2='",the.file2,"'",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("meandiffplot.db <- InitDb(db.name=paste(the.file2,'meandiffplot_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(meandiffplot.db, 'meandiffplot_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(meandiffplot.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(meandiffplot.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("column1 <- LoadCachedObject(meandiffplot.db, 'column1_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("column2 <- LoadCachedObject(meandiffplot.db, 'column2_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Plots/',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = strsplit(the.file,'/')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("outputName=paste(the.file2,'_',column1,'_vs_',column2,'_MDplot.pdf',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("b=paste(a,outputName,sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x_col1 = paste(the.file,'$',column1,sep='') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x_col2 = paste(the.file,'$',column2,sep='') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(meandiffplot.db, 'newseqexpressionset_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x=data.matrix(x, rownames.force = NA) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x=newSeqExpressionSet(x) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("MDPlot(x, c(column1,column2)  ,main=paste('MD Plot of columns ',column1,' and ',column2,", sep="")    
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   

message5 <- paste("'of the file ',the.file2,sep='') )", sep="") 
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")          

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  }

}

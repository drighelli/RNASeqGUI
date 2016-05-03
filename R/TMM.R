TMM <- function(x,the.file,Project){

 require(edgeR)

 tmm.db <- InitDb(db.name='tmm_db', db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
 
 SaveInCache(tmm.db, the.file, "the_file_key")
 SaveInCache(tmm.db, Project, "project_key")
 SaveInCache(tmm.db, x, "tmm_dataframe_key")
 
 if(Sys.info()[[1]]=="Windows"){

  the.file2 = strsplit(the.file,"\\\\")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
  outputName=paste(the.file2,"_TMM.txt", sep="")
  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
  b=paste(a,outputName,sep="\\")

  print("You loaded the following count file:")
  print(head(x,10))
  x <- edgeR::DGEList(counts = x)
  x <- edgeR::calcNormFactors(x,method='TMM')
  x <- edgeR::estimateCommonDisp(x, verbose=FALSE)
  x <- edgeR::estimateTagwiseDisp(x) 
  print("TMM normalized count:")
  print(head(x$pseudo.counts,10))
  myTMM <- x$pseudo.counts
  
  SaveInCache(tmm.db, myTMM, "mytmm_key")

  myTMM_plus_ids = cbind(row.names(x),myTMM) # add gene IDs
  
  SaveInCache(tmm.db, myTMM_plus_ids, "mytmm_plus_ids_key")

  message=paste("The ",outputName," file has been saved in the ", Project,"\\Results folder!", sep="")
  print(message)

  write.table(myTMM_plus_ids, file = b, quote=FALSE, sep="\t", row.names=FALSE)

  outputName=paste(the.file,"_TMM.pdf", sep="")

  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")

  b=paste(a,outputName,sep="\\")

  message5 <- paste("   colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred') ", sep="")

  boxplot(log(myTMM+1),col=colors, main="TMM BoxPlot",las=2)

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
  print(message)

  #write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **TMM** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
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

message5 <- paste(" require(edgeR)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" tmm.db <- InitDb(db.name='tmm_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" x <- LoadCachedObject(tmm.db, 'tmm_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(tmm.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(tmm.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(head(x,10))", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::DGEList(counts = x)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::calcNormFactors(x,method='TMM')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::estimateCommonDisp(x, verbose=FALSE)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::estimateTagwiseDisp(x) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#myTMM <- x$pseudo.counts", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("myTMM <- LoadCachedObject(tmm.db, 'mytmm_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("boxplot(log(myTMM+1),col=colors, main='TMM BoxPlot',las=2) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  }else{ #Linux

  the.file2 = strsplit(the.file,"/")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
  outputName=paste(the.file2,"_TMM.txt", sep="")
  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",sep="")
  b=paste(a,outputName,sep="/")

  print("You loaded the following count file:")
  print(head(x,10))
  x <- edgeR::DGEList(counts = x)
  x <- edgeR::calcNormFactors(x,method='TMM')
  x <- edgeR::estimateCommonDisp(x, verbose=FALSE)
  x <- edgeR::estimateTagwiseDisp(x) 
  print("TMM normalized count:")
  print(head(x$pseudo.counts,10))
  myTMM <- x$pseudo.counts

  SaveInCache(tmm.db, myTMM, "mytmm_key")
  
  myTMM_plus_ids = cbind(row.names(x),myTMM) # add gene IDs
  
  SaveInCache(tmm.db, myTMM_plus_ids, "mytmm_plus_ids_key")

  message=paste("The ",outputName," file has been saved in the ", Project,"/Results folder!", sep="")
  print(message)

  write.table(myTMM_plus_ids, file = b, quote=FALSE, sep="\t", row.names=FALSE)

  outputName=paste(the.file2,"_TMM.pdf", sep="")

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")

  b=paste(a,outputName,sep="/")

  colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred')

  boxplot(log(myTMM+1),col=colors, main="TMM BoxPlot",las=2)

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"/Plots folder!", sep="")
  print(message)

#write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **TMM** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
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

message5 <- paste(" require(edgeR)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" tmm.db <- InitDb(db.name='tmm_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" x <- LoadCachedObject(tmm.db, 'tmm_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" the.file <- LoadCachedObject(tmm.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" Project <- LoadCachedObject(tmm.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print('You loaded the following count file:')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(head(x,10))", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::DGEList(counts = x)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::calcNormFactors(x,method='TMM')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::estimateCommonDisp(x, verbose=FALSE)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::estimateTagwiseDisp(x) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#myTMM <- x$pseudo.counts", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("myTMM <- LoadCachedObject(tmm.db, 'mytmm_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("boxplot(log(myTMM+1),col=colors, main='TMM BoxPlot',las=2) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 }

}

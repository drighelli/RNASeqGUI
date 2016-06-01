UQUA <- function(x, the.file, Project){

 require(edgeR)
 
 uqua.db <- InitDb(db.name='uqua_db', db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
 
 SaveInCache(uqua.db, the.file, "the_file_key")
 SaveInCache(uqua.db, Project, "project_key")
 SaveInCache(uqua.db, x, "uqua_dataframe_key")

 if(Sys.info()[[1]]=="Windows"){

  the.file2 = strsplit(the.file,"\\\\")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
  outputName=paste(the.file2,"_UQUA.pdf", sep="")
  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
  b=paste(a,outputName,sep="\\")


  print("You loaded the following count file:")
  print(head(x,10))
  x <- edgeR::DGEList(counts = x)
  x <- edgeR::calcNormFactors(x, method='upperquartile')
  x <- edgeR::estimateCommonDisp(x, verbose=FALSE)
  x <- edgeR::estimateTagwiseDisp(x) 
  print("UQUA normalized count:")
  print(head(x$pseudo.counts,10))
  myuqua <- x$pseudo.counts
  
  SaveInCache(uqua.db, myuqua, "myuqua_key")

  colors=c('red','red','blue','blue','purple','purple','orange','orange','pink','orange','gold','darkblue','cyan','darkred')

  boxplot(log(myuqua+1), col=colors, main="Upper Quartile BoxPlot", las=1)

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
  print(message)

  outputName=paste(the.file2,"_UQUA.txt", sep="")

  a=paste(substring(getwd(),1,nchar(getwd())),"\\RNASeqGUI_Projects\\",Project,"\\Results\\",sep="")

  b=paste(a,outputName,sep="\\")

  myuqua_plus_ids = cbind(row.names(x),myuqua) # add gene IDs
  
  SaveInCache(uqua.db, myuqua_plus_ids, "myuqua_plus_ids_key")

  write.table(myuqua_plus_ids, file = b, quote=FALSE, sep="\t", row.names=FALSE)

  message=paste("The ",outputName," file has been saved in the ", Project,"\\Results folder!", sep="")
  print(message)

  #write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Normalization Interface*, you clicked the **UQUA** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
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

message5 <- paste("uqua.db <- InitDb(db.name='uqua_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(uqua.db, 'uqua_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(uqua.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(uqua.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print('You loaded the following count file:')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(head(x,10))", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::DGEList(counts = x)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::calcNormFactors(x,method='upperquartile')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::estimateCommonDisp(x, verbose=FALSE)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::estimateTagwiseDisp(x) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#myuqua <- x$pseudo.counts", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("myuqua <- LoadCachedObject(uqua.db, 'myuqua_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("colors=c('red','red','blue','blue','purple','purple','orange','orange','pink','orange','gold','darkblue','cyan','darkred')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("boxplot(log(myuqua+1),col=colors, main='Upper Quartile BoxPlot',las=1)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


  }else{ #Linux

  the.file2 = strsplit(the.file,"/")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
  outputName=paste(the.file2,"_UQUA.pdf", sep="")
  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
  b=paste(a,outputName,sep="/")


  print("You loaded the following count file:")
  print(head(x,10))
  x <- edgeR::DGEList(counts = x)
  x <- edgeR::calcNormFactors(x,method='upperquartile')
  x <- edgeR::estimateCommonDisp(x, verbose=FALSE)
  x <- edgeR::estimateTagwiseDisp(x) 
  print("UQUA normalized count:")
  print(head(x$pseudo.counts,10))
  myuqua <- x$pseudo.counts


  SaveInCache(uqua.db, myuqua, "myuqua_key")

  colors=c('red','red','blue','blue','purple','purple','orange','orange','pink','orange','gold','darkblue','cyan','darkred')

  boxplot(log(myuqua+1),col=colors, main="Upper Quartile BoxPlot",las=1)

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"/Plots folder!", sep="")
  print(message)

  outputName=paste(the.file2,"_UQUA.txt", sep="")

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",sep="")

  b=paste(a,outputName,sep="/")

  myuqua_plus_ids = cbind(row.names(x),myuqua) # add gene IDs
  SaveInCache(uqua.db, myuqua_plus_ids, "myuqua_plus_ids_key")
  

  write.table(myuqua_plus_ids, file = b, quote=FALSE, sep="\t", row.names=FALSE)

  message=paste("The ",outputName," file has been saved in the ", Project,"/Results folder!", sep="")
  print(message)

#write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Normalization Interface*, you clicked the **UQUA** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
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

message5 <- paste("uqua.db <- InitDb(db.name='uqua_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" x <- LoadCachedObject(uqua.db, 'uqua_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(uqua.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(uqua.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print('You loaded the following count file:')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(head(x,10))", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::DGEList(counts = x)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::calcNormFactors(x,method='upperquartile')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::estimateCommonDisp(x, verbose=FALSE)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#x <- edgeR::estimateTagwiseDisp(x) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#myuqua <- x$pseudo.counts", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("myuqua <- LoadCachedObject(uqua.db, 'myuqua_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("colors=c('red','red','blue','blue','purple','purple','orange','orange','pink','orange','gold','darkblue','cyan','darkred')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("boxplot(log(myuqua+1),col=colors, main='Upper Quartile BoxPlot',las=1)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

}

}

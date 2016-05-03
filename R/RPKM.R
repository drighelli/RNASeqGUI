RPKM <- function(x,the.file,Project){

 require(NOISeq)

  rpkm.db <- InitDb(db.name='rpkm_db', db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(rpkm.db, the.file, "the_file_key")
  SaveInCache(rpkm.db, Project, "project_key")
  SaveInCache(rpkm.db, x, "rpkm_dataframe_key")
 
 
 if(Sys.info()[[1]]=="Windows"){
 
  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
  the.file2 = strsplit(the.file,"\\\\")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
  outputName=paste(the.file2,"_CPM.pdf", sep="")
  b=paste(a,outputName,sep="\\")

  print("You loaded the following count file:")
  print(head(x,10))

  myrpkm = rpkm(x, lc = 0)

  print("CPM normalized count:")
  print(head(myrpkm,10))
  
  SaveInCache(rpkm.db, myrpkm, "myrpkm_key")
  
  boxplot(log(myrpkm+1),col=(c("red","blue","gold","darkgreen")), main="CPM BoxPlot",las=2)

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
  print(message)

  outputName=paste(the.file2,"_CPM.txt", sep="")

  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\",sep="")

  b=paste(a,outputName,sep="\\")

  myrpkm_plus_ids = cbind(row.names(x),myrpkm) # add gene IDs

  SaveInCache(rpkm.db, myrpkm_plus_ids, "myrpkmids_key")
  
  write.table(myrpkm_plus_ids, file = b, quote=FALSE, sep="\t", row.names=FALSE)

  message=paste("The ",outputName," file has been saved in the ", Project,"\\Results folder!", sep="")
  print(message)

#write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **CPM** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
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

message5 <- paste("require(NOISeq)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("rpkm.db <- InitDb(db.name='rpkm_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(rpkm.db, 'rpkm_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(rpkm.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(rpkm.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#myrpkm = rpkm(x, lc = 0) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("myrpkm <- LoadCachedObject(rpkm.db, 'myrpkm_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("boxplot(log(myrpkm+1),col=(c('red','blue','gold','darkgreen')), main='CPM BoxPlot',las=2) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  }else{ #Linux

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
  the.file2 = strsplit(the.file,"/")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
  outputName=paste(the.file2,"_CPM.pdf", sep="")
  b=paste(a,outputName,sep="/")

  print("You loaded the following count file:")
  print(head(x,10))

  myrpkm = rpkm(x, lc = 0)

  print("CPM normalized count:")
  print(head(myrpkm,10))

  SaveInCache(rpkm.db, myrpkm, "myrpkm_key")
  
  boxplot(log(myrpkm+1),col=(c("red","blue","gold","darkgreen")), main="CPM BoxPlot",las=2)

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"/Plots folder!", sep="")
  print(message)

  outputName=paste(the.file2,"_CPM.txt", sep="")

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",sep="")

  b=paste(a,outputName,sep="/")

  myrpkm_plus_ids = cbind(row.names(x),myrpkm) # add gene IDs
  
  SaveInCache(rpkm.db, myrpkm_plus_ids, "myrpkmids_key")

  write.table(myrpkm_plus_ids, file = b, quote=FALSE, sep="\t", row.names=FALSE)

  message=paste("The ",outputName," file has been saved in the ", Project,"/Results folder!", sep="")
  print(message)

#write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **CPM** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
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

message5 <- paste("require(NOISeq)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("rpkm.db <- InitDb(db.name='rpkm_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(rpkm.db, 'rpkm_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(rpkm.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(rpkm.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#myrpkm = rpkm(x, lc = 0) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("myrpkm <- LoadCachedObject(rpkm.db, 'myrpkm_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("boxplot(log(myrpkm+1),col=(c('red','blue','gold','darkgreen')), main='CPM BoxPlot',las=2) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

}

}

FQUA <- function(x,the.file,Project){

 require(preprocessCore)
 
 if(Sys.info()[[1]]=="Windows"){

  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
  the.file2 = strsplit(the.file,"\\\\")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
  outputName=paste(the.file2,"_FQUA.pdf", sep="")

 fqua.db <- InitDb(db.name=paste(the.file2,'fqua_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
 SaveInCache(fqua.db, the.file, "the_file_key")
 SaveInCache(fqua.db, Project, "project_key")
 x <- as.matrix(x)
 SaveInCache(fqua.db, x, "fqua_dataframe_key")

  b=paste(a,outputName,sep="\\")

  print("You loaded the following count file:")
  print(head(x,10))

  myfqua = normalize.quantiles(x,copy=TRUE)

  SaveInCache(fqua.db, myfqua, "myfqua_key")

  colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred') 

  boxplot(log(myfqua+1),names=colnames(x),col=colors, main=paste(the.file2," Full Quantile BoxPlot",sep=""),las=2)

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
  print(message)

  outputName=paste(the.file2,"_FQUA.txt", sep="")

  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\",sep="")

  b=paste(a,outputName,sep="\\")  

  myfqua_plus_ids = cbind(row.names(x),myfqua) # add gene IDs
  
  SaveInCache(fqua.db, myfqua_plus_ids, "myfqua_plus_ids_key")
  
  write.table(myfqua_plus_ids, file = b, quote=FALSE, sep="\t", row.names=FALSE)

  message=paste("The ",outputName," file has been saved in the ", Project,"\\Results folder!", sep="")
  print(message)

#write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **FQUA** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
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

message5 <- paste(" require(preprocessCore)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("fqua.db <- InitDb(db.name=paste(the.file2,'fqua_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(fqua.db, 'fqua_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(fqua.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(fqua.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#myfqua = normalize.quantiles(x,copy=TRUE)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("myfqua <- LoadCachedObject(fqua.db, 'myfqua_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("boxplot(log(myfqua+1),names=colnames(x),col=colors, main=paste(the.file2,' Full Quantile BoxPlot',sep=''),las=2)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


  }else{ #Linux

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
  the.file2 = strsplit(the.file,"/")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
  outputName=paste(the.file2,"_FQUA.pdf", sep="")
  b=paste(a,outputName,sep="/")

 fqua.db <- InitDb(db.name=paste(the.file2,'fqua_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
 SaveInCache(fqua.db, the.file, "the_file_key")
 SaveInCache(fqua.db, Project, "project_key")
 x <- as.matrix(x)
 SaveInCache(fqua.db, x, "fqua_dataframe_key")

  print("You loaded the following count file:")
  print(head(x,10))

  myfqua = normalize.quantiles(x,copy=TRUE)
  
  SaveInCache(fqua.db, myfqua, "myfqua_key")
  
  colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred')

  boxplot(log(myfqua+1),names=colnames(x),col=colors, main=paste(the.file2," Full Quantile BoxPlot",sep=""),las=2)

  dev.print(device = pdf, file=b)

  message=paste("The ",outputName," file has been saved in the ", Project,"/Plots folder!", sep="")
  print(message)

  outputName=paste(the.file2,"_FQUA.txt", sep="")

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",sep="")

  b=paste(a,outputName,sep="/")  

  myfqua_plus_ids = cbind(row.names(x),myfqua) # add gene IDs

  col_names=append("id",colnames(x))
   
  myfqua_plus_ids_pluscolnames = rbind(col_names,myfqua_plus_ids)  # add column names

  print("First lines of the new counts normalized:")

  print(head(myfqua_plus_ids_pluscolnames))

  SaveInCache(fqua.db, myfqua_plus_ids, "myfqua_plus_ids_key")
  
  write.table(myfqua_plus_ids_pluscolnames, file = b, quote=FALSE, sep="\t", row.names=FALSE, col.names = FALSE)

  message=paste("The ",outputName," file has been saved in the ", Project,"/Results folder!", sep="")
  print(message)

#write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **FQUA** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
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

message5 <- paste(" require(preprocessCore)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("fqua.db <- InitDb(db.name=paste(the.file2,'fqua_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(fqua.db, 'fqua_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(fqua.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(fqua.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#myfqua = normalize.quantiles(x,copy=TRUE)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("myfqua <- LoadCachedObject(fqua.db, 'myfqua_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("boxplot(log(myfqua+1),names=colnames(x),col=colors, main=paste(the.file2,' Full Quantile BoxPlot',sep=''),las=2)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 }
}

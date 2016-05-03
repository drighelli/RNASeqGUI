readCount <- function(the.file,Project){
  
  readcount.db <- InitDb(db.name='readcount_db', db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  SaveInCache(readcount.db, Project, "project_key")
  SaveInCache(readcount.db, the.file, "the_file_key")
  
  files=list.files(path=the.file, pattern = "bam$", full.names = TRUE)
  bfs <- BamFileList(files)
  colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred')   #Number of uniquely mapped reads
  RR=countBam(bfs)   #MAIN FUNCTION HERE 
  for (i in 1:length(rownames(RR))){ 
    rownames(RR)[i] = substring(rownames(RR)[i],1,nchar(rownames(RR)[i])-4)
  }
  
  SaveInCache(readcount.db, files, "files_key")
  SaveInCache(readcount.db, bfs, "bfs_key")
  SaveInCache(readcount.db, RR, "rr_key")

  barplot(RR$records,names.arg=rownames(RR),las=2,col=colors,main="Number of total lines in the sam file",  cex.names=0.6)


if(Sys.info()[[1]]=="Windows"){
  the.file2 = strsplit(the.file,"\\\\")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the name of the forlder

  a=paste(substring(getwd(),1,nchar(getwd())),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
  b=paste(a,the.file2,"_","ReadCountHist.pdf",sep="")
  dev.print(device = pdf, file=b)

  a=paste(substring(getwd(),1,nchar(getwd())),"\\RNASeqGUI_Projects\\",Project,"\\Results\\",sep="")
  b=paste(a,the.file2,"_","ReadCount.txt",sep="")

  numberReads=cbind(rownames(RR),RR$records)
  colnames(numberReads)=c("fileName","NumberOfSequences")
  write.table(numberReads, file = b , quote=FALSE, sep="\t", col.names = TRUE, row.names=FALSE)  

 message=paste("The ",paste(the.file2,"_","ReadCountHist.pdf",sep="")," file has been saved in the ", Project,"\\Plots folder!", sep="")
 print(message)
 message=paste("The ",paste(the.file2,"_","ReadCount.txt",sep="")," file has been saved in the ", Project,"\\Results folder!", sep="")
 print(message)

     #write into the project report
     report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")
 
     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Bam Exploration Interface*,  you clicked the **Read Counts** button at `", Sys.time(),"` and the `",paste(the.file2,"_","ReadCountHist.pdf",sep=""),"` file has been saved in the *", Project,"\\Plots* folder", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" and the `",paste(the.file2,"_","ReadCount.txt",sep=""),"` file has been saved in the *", Project,"\\Results* folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("readcount.db <- InitDb(db.name='readcount_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(readcount.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" Project <- LoadCachedObject(readcount.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" files <- LoadCachedObject(readcount.db, 'files_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" bfs <- LoadCachedObject(readcount.db, 'bfs_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("   colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" RR <- LoadCachedObject(readcount.db, 'rr_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

# message5 <- paste("the.file ='", the.file,"'", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("  ",sep="\n")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("Project ='", Project,"'", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("  ",sep="\n")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
message5 <- paste(" #files=list.files(path=the.file, pattern = 'bam$', full.names = TRUE) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" #bfs <- BamFileList(files) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   #RR=countBam(bfs)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   #for (i in 1:length(rownames(RR))){ rownames(RR)[i] = substring(rownames(RR)[i],1,nchar(rownames(RR)[i])-4) } ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   barplot(RR$records,names.arg=rownames(RR),las=2,col=colors,main='Read Count Histogram') ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  }else{  #Linux

  the.file2 = strsplit(the.file,"/")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the name of the forlder

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
  b=paste(a,the.file2,"_","ReadCountHist.pdf",sep="")
  dev.print(device = pdf, file=b)

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",sep="")
  b=paste(a,the.file2,"_","ReadCount.txt",sep="")

  numberReads=cbind(rownames(RR),RR$records)
  colnames(numberReads)=c("fileName","NumberOfSequences")
  print(numberReads)
  write.table(numberReads, file = b , quote=FALSE, sep="\t", col.names = TRUE, row.names=FALSE)  

 message=paste("The ",paste(the.file2,"_","ReadCountHist.pdf",sep="")," file has been saved in the ", Project,"/Plots folder!", sep="")
 print(message)
 message=paste("The ",paste(the.file2,"_","ReadCount.txt",sep="")," file has been saved in the ", Project,"/Results folder!", sep="")
 print(message)

     #write into the project report
     report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")
 
     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Bam Exploration Interface*, you clicked the **Read Counts** button at `", Sys.time(),"` and the `",paste(the.file2,"_","ReadCountHist.pdf",sep=""),"` file has been saved in the *", Project,"/Plots* folder", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" and the `",paste(the.file2,"_","ReadCount.txt",sep=""),"` file has been saved in the *", Project,"/Results* folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("readcount.db <- InitDb(db.name='readcount_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(readcount.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" Project <- LoadCachedObject(readcount.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" files <- LoadCachedObject(readcount.db, 'files_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" bfs <- LoadCachedObject(readcount.db, 'bfs_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("   colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" RR <- LoadCachedObject(readcount.db, 'rr_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


# message5 <- paste("#the.file ='", the.file,"'", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("  ",sep="\n")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("#Project ='", Project,"'", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("  ",sep="\n")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
message5 <- paste(" #files=list.files(path=the.file, pattern = 'bam$', full.names = TRUE) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" #bfs <- BamFileList(files) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   #colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   #RR=countBam(bfs)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   #for (i in 1:length(rownames(RR))){ rownames(RR)[i] = substring(rownames(RR)[i],1,nchar(rownames(RR)[i])-4) } ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   barplot(RR$records,names.arg=rownames(RR),las=2,col=colors,main='Read Count Histogram',  cex.names=0.6) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   numberReads=cbind(rownames(RR),RR$records)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   colnames(numberReads)=c('fileName','NumberOfSequences')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   print(numberReads)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

}

}


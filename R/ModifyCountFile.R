modifycountfile <- function(Count.File,Project,columns) {

  countfilename <- substring(Count.File, max(gregexpr("/",Count.File)[[1]])+1)
  
  dbfilename <- paste(countfilename,"_", paste(columns, sep = "", collapse = ""),'_keepcolumns_db',sep="")
  
  keepcolumns.db <- InitDb(dbfilename, db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(keepcolumns.db, Count.File, "Count_File_key")
  SaveInCache(keepcolumns.db, Project, "project_key")
  #SaveInCache(keepcolumns.db, columns, "columns_key")
  
  res=NULL

  counts=read.table(Count.File, row.names=1, header=TRUE)
  SaveInCache(keepcolumns.db, counts, "counts_key")
  print(dim(counts))
  print(head(counts))
  print("Columns selected:")
  columns = as.numeric(as.character(columns))
  SaveInCache(keepcolumns.db, columns, "columns_key")
  print(columns)
  sub.counts = counts[,columns]
  SaveInCache(keepcolumns.db, sub.counts, "sub_counts_key")
  print("Count file modified:")
  print(dim(sub.counts))
  print(head(sub.counts))
  
  outfilename <- paste(countfilename, "_cols", paste(columns, sep = "", collapse = ""), ".txt",sep="")
  results.path <- file.path(getwd(), "RNASeqGUI_Projects", Project, "Results")
  outfilepath <- file.path(results.path, outfilename)
  
  write.table(sub.counts, file = outfilepath, quote=FALSE, row.names=TRUE,sep="\t")
  message=paste("The file", outfilename," has just been written in ", results.path, " folder!", sep="")
  
 # if(Sys.info()[[1]]=="Windows"){
 # 
 #   write.table(sub.counts, file = paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\new_counts.txt",sep=""), quote=FALSE, row.names=TRUE,sep="\t")
 #   message=paste("The file", outfilename," has just been written in ", results.path, " folder!", sep="")
 #   print(message)
 # 
 # }else{ #Unix
 # 
 #   write.table(sub.counts, file = paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/new_counts.txt",sep=""), quote=FALSE, row.names=TRUE,sep="\t")
 #   message=paste("The file 'new_counts.txt' has just been written in RNASeqGUI_Projects/", Project,"/Results folder!", sep="")
 #   print(message)
 # 
 # }

 res <- sub.counts
 
 #write into the project report
 report=file.path("RNASeqGUI_Projects",Project,"Logs","report.Rmd")
   #paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")
 
 message5 <- paste("  ",sep="\n")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message2 <- paste(" * In the *Utility Interface*, you clicked the **Keep Columns** button at `", Sys.time(),"` and the new_counts.txt file has ben saved in the `", Project,"/Results` folder.", sep="")
 write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste("  ",sep="\n")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message2 <- paste("You chose the following count file: `",Count.File,"`. ",sep="\n")
 write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste(" This R code has been run:",sep="\n")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste("  ",sep="\n")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste(" ```{r} ",sep="\n")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste("dbfilename='",dbfilename,"'",sep="")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste("keepcolumns.db <- InitDb(dbfilename, db.path=file.path('cache'))", sep="")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste("Count.File <- LoadCachedObject(keepcolumns.db, 'Count_File_key')",sep="")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 # message5 <- paste("columns='",c(columns),"'",sep="")
 # write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste("counts <- LoadCachedObject(keepcolumns.db, 'counts_key')",sep="")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 # message5 <- paste("counts=read.table(Count.File, row.names=1, header=TRUE)",sep="")
 # write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste("print(dim(counts))",sep="")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste("print(head(counts))",sep="")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste("counts <- LoadCachedObject(keepcolumns.db, 'counts_key')",sep="")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste("columns <- LoadCachedObject(keepcolumns.db, 'columns_key')",sep="")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 # message5 <- paste("columns = as.numeric(as.character(columns))",sep="")
 # write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste("sub.counts <- LoadCachedObject(keepcolumns.db, 'sub_counts_key')",sep="")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 # message5 <- paste("sub.counts = counts[,columns]",sep="")
 # write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste("print(dim(sub.counts))",sep="")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste("print(head(sub.counts))",sep="")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
 # counts=read.table(Count.File, row.names=1, header=TRUE)
 # print(dim(counts))
 # print(head(counts))
 # print("Columns selected:")
 # columns = as.numeric(as.character(columns))
 # print(columns)
 # counts = counts[,columns]
 # print("Count file modified:")
 # print(dim(counts))
 # print(head(counts))
 # 
 
 message8 <- paste(" ``` ",sep="\n")
 write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
 
 message5 <- paste(" _______________________________________________________________________ ",sep="\n")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 

 res

}

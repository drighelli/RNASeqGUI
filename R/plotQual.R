plotQual <-
function(the.file,Project){

  plotqual.db <- InitDb(db.name='plotqual_db', db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  SaveInCache(plotqual.db, Project, "project_key")
  SaveInCache(plotqual.db, the.file, "the_file_key")
  
  if(Sys.info()[[1]]=="Windows"){

  #pat = paste("..\\Data\\Bam\\", the.file, sep="")
  files=list.files(path=the.file, pattern = "bam$", full.names = TRUE)
  bfs <- BamFileList(files)
  colors=c('red', 'blue', 'green',' brown','purple','darkgreen','black','pink','orange','gold','darkblue','cyan','darkred')

  SaveInCache(plotqual.db, files, "files_key")
  SaveInCache(plotqual.db, bfs, "bfs_key")
  
  plotQuality(bfs,col=colors,lty=1,main="Per Base Mean Quality Plot")  # MAIN FUNCTION HERE
  legend('topright', names(bfs) , lty=1, col=colors, bty='n', cex=.75)

  the.file2 = strsplit(the.file,"\\\\")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the name of the forlder

  a=paste(substring(getwd(),1,nchar(getwd())),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
  b=paste(a,the.file2,"_","QualityPlot.pdf",sep="")
  dev.print(device = pdf, file=b)

 message=paste("The ",paste(the.file2,"_","QualityPlot.pdf",sep="")," file has been saved in the ", Project,"\\Plots folder!", sep="")
 print(message)

#write into the project report
    report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")
 
     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Bam Exploration Interface*, you clicked the **Mean Quality of Reads** button at `", Sys.time(),"` and the `",paste(the.file2,"_","QualityPlot.pdf",sep=""),"` file has been saved in the *", Project,"/Plots* folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("plotqual.db <- InitDb(db.name='plotqual_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(plotqual.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" Project <- LoadCachedObject(plotqual.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" files <- LoadCachedObject(plotqual.db, 'files_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" bfs <- LoadCachedObject(plotqual.db, 'bfs_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# 
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
# message5 <- paste(" files=list.files(path=the.file, pattern = 'bam$', full.names = TRUE) ",sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("  ",sep="\n")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" #bfs <- BamFileList(files) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("     plotQuality(bfs,col=colors,lty=1,main='Per Base Mean Quality Plot') ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   legend('topright', names(bfs) , lty=1, col=colors, bty='n', cex=.75) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")



  }else{  #Linux

  #pat = paste("../Data/Bam/", the.file, sep="")
  files=list.files(path=the.file, pattern = "bam$", full.names = TRUE)
  bfs <- BamFileList(files)
  colors=c('red', 'blue', 'green',' brown','purple','darkgreen','black','pink','orange','gold','darkblue','cyan','darkred')

  SaveInCache(plotqual.db, files, "files_key")
  SaveInCache(plotqual.db, bfs, "bfs_key")
  
  plotQuality(bfs,col=colors,lty=1,main="Per Base Mean Quality Plot")  # MAIN FUNCTION HERE
  legend('topright', names(bfs) , lty=1, col=colors, bty='n', cex=.75)

  the.file2 = strsplit(the.file,"/")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the name of the forlder

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
  b=paste(a,the.file2,"_","QualityPlot.pdf",sep="")
  dev.print(device = pdf, file=b)

 message=paste("The ",paste(the.file2,"_","QualityPlot.pdf",sep="")," file has been saved in the ", Project,"/Plots folder!", sep="")
 print(message)

  #write into the project report
     report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")
 
     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Bam Exploration Interface*, you clicked the **Mean Quality of Reads** button at `", Sys.time(),"` and the `",paste(the.file2,"_","QualityPlot.pdf",sep=""),"` file has been saved in the *", Project,"/Plots* folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


message5 <- paste("plotqual.db <- InitDb(db.name='plotqual_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(plotqual.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" Project <- LoadCachedObject(plotqual.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" files <- LoadCachedObject(plotqual.db, 'files_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" bfs <- LoadCachedObject(plotqual.db, 'bfs_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
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
# message5 <- paste(" files=list.files(path=the.file, pattern = 'bam$', full.names = TRUE) ",sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("  ",sep="\n")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" #bfs <- BamFileList(files) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("     plotQuality(bfs,col=colors,lty=1,main='Per Base Mean Quality Plot') ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   legend('topright', names(bfs) , lty=1, col=colors, bty='n', cex=.75) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


}

}

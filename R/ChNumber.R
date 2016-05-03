ChNumber <-
function(the.file,Project){

  chnumber.db <- InitDb(db.name='chnumber_db', db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  SaveInCache(chnumber.db, Project, "project_key")
  SaveInCache(chnumber.db, the.file, "the_file_key")
  
  #pat = paste("Data/Bam/", the.file, sep="")
  files=list.files(path=the.file, pattern = "bam$", full.names = TRUE)
  bfs <- BamFileList(files)
  colors=c('red', 'blue', 'green',' brown','purple','darkgreen','black','pink','orange','gold','darkblue','cyan','darkred')
  SaveInCache(chnumber.db, files, "files_key")
  SaveInCache(chnumber.db, bfs, "bfs_key")
  
 if(Sys.info()[[1]]=="Windows"){
  the.file = strsplit(the.file,"\\\\")
  the.file = the.file[[1]][length(the.file[[1]])]  #estract the name of the forlder

  barplotFun <- function(bamfile){
   name = strsplit(path(bamfile),"\\\\")
   name = name[[1]][length(name[[1]])]  #estract the name of the folder
   barplot(bamfile,col=colors,las=2,main=paste("Chromosome Number Histogram of ",name,sep=""),sub=name)
   a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
   b=paste(a,name,"_",the.file,"_","ChromosomeNumberHist.pdf",sep="")
   dev.print(device = pdf, file=b)
   dev.off()
  }

  lapply(bfs, barplotFun)

  message=paste("The ChromosomeNumberHist.pdf files have been saved in the ", Project,"\\Plots folder!", sep="")
  print(message)

     #write into the project report
     report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.txt",sep="") 
 
     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Bam Exploration Interface*, you clicked the **Reads Per Chromosome** button at `", Sys.time(),"` and the `",paste(the.file2,"_","ChromosomeNumberHist.pdf",sep=""),"` files have been saved in the *", Project,"/Plots* folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("chnumber.db <- InitDb(db.name='chnumber_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(chnumber.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" Project <- LoadCachedObject(chnumber.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" files <- LoadCachedObject(chnumber.db, 'files_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" bfs <- LoadCachedObject(chnumber.db, 'bfs_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# # message5 <- paste("the.file ='", the.file,"'", sep="")
# # write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# # 
# # message5 <- paste("  ",sep="\n")
# # write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# # 
# # message5 <- paste("Project ='", Project,"'", sep="")
# # write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# # 
# # message5 <- paste("  ",sep="\n")
# # write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# # 
# message5 <- paste(" files=list.files(path=the.file, pattern = 'bam$', full.names = TRUE) ",sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("  ",sep="\n")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
message5 <- paste(" #bfs <- BamFileList(files) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  the.file2 = strsplit(the.file,'/')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the name of the forlder",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   barplotFun <- function(bamfile){ ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    name = strsplit(path(bamfile),'/') ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    name = name[[1]][length(name[[1]])]  #estract the name of the forlder ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("       barplot(bamfile,col=colors,las=2,main=paste('Chromosome Number Histogram of ',name,sep=''),sub=name) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Plots/',sep='') ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    b=paste(a,name,'_',the.file2,'_ChromosomeNumberHist.pdf',sep='') ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    dev.print(device = pdf, file=b) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    dev.off() } ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   lapply(bfs, barplotFun) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" Please, ignore the Error above saying << ` ## Error: cannot open file '..._PerBaseQual.pdf' ` >> . I can assure you that everything went fine. If you copy the code shown above and paste in the R environment, you can see that this code works perfectly! :)",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 }else{  #Linux

  the.file2 = strsplit(the.file,"/")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the name of the forlder

  barplotFun <- function(bamfile){
   #name = strsplit(path(bamfile),"/")
   name =  strsplit(bamfile$path,"/")
   name = name[[1]][length(name[[1]])]  #estract the name of the forlder
   barplot(bamfile,col=colors,las=2,main=paste("Chromosome Number Histogram of ",name,sep=""),sub=name)
   a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
   b=paste(a,name,"_",the.file2,"_","ChromosomeNumberHist.pdf",sep="")
   dev.print(device = pdf, file=b)
   dev.off()
  }

  lapply(bfs, barplotFun)

  message=paste("The ChromosomeNumberHist.pdf files have been saved in the ", Project,"/Plots folder!", sep="")
  print(message)

#write into the project report
     report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")
 
     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Bam Exploration Interface*, you clicked the **Reads Per Chromosome** button at `", Sys.time(),"` and the `",paste(the.file2,"_","ChromosomeNumberHist.pdf",sep=""),"` files have been saved in the *", Project,"/Plots* folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


message5 <- paste("chnumber.db <- InitDb(db.name='chnumber_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(chnumber.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" Project <- LoadCachedObject(chnumber.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" files <- LoadCachedObject(chnumber.db, 'files_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" bfs <- LoadCachedObject(chnumber.db, 'bfs_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


# message5 <- paste("the.file ='", the.file,"'", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("Project ='", Project,"'", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste(" files=list.files(path=the.file, pattern = 'bam$', full.names = TRUE) ",sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste(" bfs <- BamFileList(files) ",sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   colors=c('red', 'blue', 'green',' brown','purple','darkgreen','pink','orange','gold','darkblue','cyan','darkred')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  the.file2 = strsplit(the.file,'/')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the name of the forlder",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   barplotFun <- function(bamfile){ ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    name =  strsplit(bamfile$path,'/') ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    name = name[[1]][length(name[[1]])]  #estract the name of the forlder ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("       barplot(bamfile,col=colors,las=2,main=paste('Chromosome Number Histogram of ',name,sep=''),sub=name) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Plots/',sep='') ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    b=paste(a,name,'_',the.file2,'_ChromosomeNumberHist.pdf',sep='') ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  #  dev.print(device = pdf, file=b) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  #  dev.off() ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" } ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    lapply(bfs, barplotFun) ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

#message8 <- paste(" Please, ignore the Error above saying << ` ## Error: cannot open file '..._ChromosomeNumberHist.pdf' ` >> . I can assure you that everything went fine. If you copy the code shown above and paste in the R environment, you can see that this code works perfectly! :)",sep="\n")
#write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 }

}

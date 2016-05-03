countingReads <-
function(the.file,Bam.FolderNew,union,IntersectionStrict,IntersectionNonEmpty,StrandEntry,Project,Paired){

 require(GenomicAlignments)
 #require(GRanges)
 require(GenomicFeatures)
 require(BiocParallel)

 #Project = 'pippo' 
 #the.file="/media/6b8a4404-f3e2-4fbc-9fc4-5a0ebc8d0635/2L_Drosophila_melanogaster.BDGP5.70.gtf"
 #Bam.FolderNew="/media/6b8a4404-f3e2-4fbc-9fc4-5a0ebc8d0635/Bam/demo"
 #union=T
 #IntersectionStrict=F
 #IntersectionNonEmpty=F
 #StrandEntry=T
 
 countingreads.db <- InitDb(db.name='countingreads_db', db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
 SaveInCache(countingreads.db, the.file, "the_file_key")
 SaveInCache(countingreads.db, Project, "project_key")
 SaveInCache(countingreads.db, Bam.FolderNew, "bamfoldernew_key")
 SaveInCache(countingreads.db, union, "union_key")
 SaveInCache(countingreads.db, StrandEntry, "strandentry_key")
 SaveInCache(countingreads.db, IntersectionStrict, "intersectionstrict_key")
 SaveInCache(countingreads.db, IntersectionNonEmpty, "intersectionnonempty_key")
 SaveInCache(countingreads.db, Paired, "paired")

 Single <- !Paired

print(paste("Paired: ",Paired,sep=""))
 
 res=NULL

       if (Single == TRUE){print("summarizeOverlaps is working on single-end reads.")
                          }else{print("summarizeOverlaps is working on paired-end reads.")
       }

 if(Sys.info()[[1]]=="Windows"){

 gtf = makeTxDbFromGFF(the.file,format="gtf")
 exonsByGene <- exonsBy(gtf, by="gene") 

 fls <- list.files(Bam.FolderNew,pattern="bam$",full.names=TRUE)
 bamlst <- BamFileList(fls,obeyQname=TRUE)     

summarizeOverlapsFun <- function(bamfile){
  #print("Ecco il nome prima della modifica")
  #print(bamfile)
  name =  strsplit(bamfile$path,"\\\\")
  #print("Ecco il nome dopo lo stringsplit")
  #print(name)
  name = name[[1]][names(name[[1]])]  #estract the name of the bam file
  #print("The final name of the bam file is")
  #print(name)
  name2 = strsplit(path(bamfile),"/")
  #print("name2 is")
  #print(name2)
  name3 = name2[[1]][length(name2[[1]])]
  #print("name3 is")
  #print(name3) 
 message=paste("Please, WAIT. Working on file: ", name3, sep="")
 print(message)
  geneHitsSingletons <- NULL
   if (union==TRUE){  
      geneHitsSingletons <- GRanges::summarizeOverlaps(exonsByGene, bamfile, mode="Union", ignore.strand=StrandEntry, singleEnd=Single)  
   }
   if (IntersectionStrict==TRUE){  
      geneHitsSingletons <- GRanges::summarizeOverlaps(exonsByGene, bamfile, mode="IntersectionStrict", ignore.strand=StrandEntry, singleEnd=Single)  
   }   
   if (IntersectionNonEmpty==TRUE){             
      geneHitsSingletons <- GRanges::summarizeOverlaps(exonsByGene, bamfile, mode="IntersectionNotEmpty", ignore.strand=StrandEntry, singleEnd=Single)  
   }
  res = assays(geneHitsSingletons)$counts
  #print("path a is: ")
  #print(a)
  b=paste(BAM.name_Counts,"\\",name3,"_summarizeOverlapsCounts.txt",sep="")
  #print("path a is: ")
  #print(b)
  write.table(res, file = b , quote=FALSE, sep="\t", row.names=TRUE) 
}


apply_pb <- function()
{

pb <- tkProgressBar("Started", "Some information in %",min=0,max=100,initial=0, width = 500)
Sys.sleep(0.5)
u <- c(0, sort(runif(30, 0, 12)), 12)
for(i in u) {
    Sys.sleep(0.3)
    info <- sprintf("%d%% done. PLEASE WAIT !", round(i))
    setTkProgressBar(pb, i, sprintf("(%s)", info), info)    
}
bplapply(bamlst , summarizeOverlapsFun)        #MAIN FUNCTION HERE
Sys.sleep(1)
close(pb)
}
apply_pb()



     #write into the project report
     report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.txt",sep="") 
     message2 <- paste("You ckicked the 'summarizeOverlaps' button at ", Sys.time()," and the count file has been saved in the ",BAM.name_Counts," folder!", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n") 
     message2 <- paste("You chose the following count file: ",the.file,"the bam folder: ",Bam.FolderNew,"the count mode: union = ",union,", IntersectionStrict = ",IntersectionStrict,", IntersectionNonEmpty = ",IntersectionNonEmpty," and StrandEntry: ",StrandEntry, sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

   
  txtfls <- list.files(paste(BAM.name_Counts,"\\",sep=""),pattern="txt$",full.names=TRUE)
  name = strsplit(txtfls[1],"\\\\")
  name = name[[1]][length(name[[1]])]  #estract the name of the txt file
   # name = strsplit(name,".bam")
   # name = name[[1]][1]  #estract the name of the first sample
  x = read.table(paste(BAM.name_Counts,"\\",name,sep="") , header=TRUE, row.names=1)
  for (i in txtfls[2:length(txtfls)]){ 
   xi <- read.table(i, header=TRUE, row.names=1)
   x = cbind(x,xi)
  }

 res=x

SaveInCache(countingreads.db, res, "res_key")
 
 write.table(res, file = paste(BAM.name_Counts,"\\","\\counts.txt",sep="") , quote=FALSE, sep="\t", row.names=TRUE)

   #write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Read Count Interface*, you clicked the **summarizeOverlaps** button at ", Sys.time()," and the count file has been saved in the ",BAM.name_Counts," folder!", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste("You chose the following count file: ",the.file,"the bam folder: ",Bam.FolderNew,"the GTF file: ",the.file,"the count mode: union = ",union,", IntersectionStrict = ",IntersectionStrict,", IntersectionNonEmpty = ",IntersectionNonEmpty," and StrandEntry: ",StrandEntry, sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file ='", the.file,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Bam.FolderNew ='", Bam.FolderNew,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project='",Project,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("union='",union,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("StrandEntry='",StrandEntry,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Single='", Single,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("IntersectionStrict='",IntersectionStrict,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("IntersectionNonEmpty='",IntersectionNonEmpty,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#gtf = makeTxDbFromGFF(the.file,format='gtf')",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#exonsByGene <- exonsBy(gtf, by='gene') ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#fls <- list.files(Bam.FolderNew,pattern='bam$',full.names=TRUE)",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#bamlst <- BamFileList(fls,obeyQname=TRUE) ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#Vector.Bam.FolderNew = strsplit(Bam.FolderNew,'/')[[1]]",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#BAM.name = Vector.Bam.FolderNew[length(Vector.Bam.FolderNew)]",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#BAM.name_Counts <- paste(getwd(),'RNASeqGUI_Projects',Project,'Results/summarizeOverlaps_Report',sep='/') ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "")

message5 <- paste("#dir.create(BAM.name_Counts, showWarnings = TRUE, recursive = FALSE)",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
     
message5 <- paste("# summarizeOverlapsFun <- function(bamfile){",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#name =  strsplit(bamfile$path,'/')",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#name = name[[1]][length(name[[1]])]  #estract the name of the forlder", sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#geneHitsSingletons <- NULL",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#if (union==TRUE){geneHitsSingletons <- summarizeOverlaps(exonsByGene, bamfile, mode='Union', ignore.strand=StrandEntry, singleEnd=Single)   }",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#if (IntersectionStrict==TRUE){geneHitsSingletons <- summarizeOverlaps(exonsByGene, bamfile, mode='IntersectionStrict', ignore.strand=StrandEntry, singleEnd=Single)   } ",sep="\n")  
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#if (IntersectionNonEmpty==TRUE){geneHitsSingletons <- summarizeOverlaps(exonsByGene, bamfile, mode='IntersectionNonEmpty', ignore.strand=StrandEntry, singleEnd=Single)   }",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#res = assays(geneHitsSingletons)$counts",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#b=paste(BAM.name_Counts,'/',name,'_summarizeOverlapsCounts.txt',sep='')",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#write.table(res, file = b , quote=FALSE, sep='\t', row.names=TRUE)}",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#bplapply(bamlst , summarizeOverlapsFun) ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 

 }else{         # Linux

#Bam.FolderNew="/media/6b8a4404-f3e2-4fbc-9fc4-5a0ebc8d0635/Bam/demo"
#the.file="/media/6b8a4404-f3e2-4fbc-9fc4-5a0ebc8d0635/2L_Drosophila_melanogaster.BDGP5.70.gtf"
#Project="pippo"
#union="TRUE"
#StrandEntry="TRUE"
#IntersectionStrict="FALSE"
#IntersectionNonEmpty="FALSE"

 gtf = makeTxDbFromGFF(the.file,format="gtf")
 exonsByGene <- exonsBy(gtf, by="gene") 

 fls <- list.files(Bam.FolderNew,pattern="bam$",full.names=TRUE)
 bamlst <- BamFileList(fls,obeyQname=TRUE) 

   Vector.Bam.FolderNew = strsplit(Bam.FolderNew,"/")[[1]]
   BAM.name = Vector.Bam.FolderNew[length(Vector.Bam.FolderNew)]
   BAM.name_Counts <- paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",BAM.name,"_summarizeOverlaps",sep="")
   dir.create(BAM.name_Counts, showWarnings = TRUE, recursive = FALSE)
   print(paste("The '",BAM.name,"_summarizeOverlaps' folder has been created to store the results of the 'Read Count Interface' inside the ", getwd(),"/RNASeqGUI_Projects/",Project,"/Results folder.",sep=""))

#bamfile = bamlst[[1]]


summarizeOverlapsFun <- function(bamfile){
  name =  strsplit(bamfile$path,"/")
  #name = names(bamfile) # <- This ( names() ) does not work inside the bplapply loop
  name = name[[1]][length(name[[1]])]  #estract the name of the forlder
  geneHitsSingletons <- NULL
  print("summarizeOverlaps just started. PLEASE, WAIT!")
   if (union==TRUE){  
      geneHitsSingletons <- GenomicAlignments::summarizeOverlaps(exonsByGene, bamfile, mode="Union", ignore.strand=StrandEntry, singleEnd=Single)  
   }
   if (IntersectionStrict==TRUE){  
      geneHitsSingletons <- GRanges::summarizeOverlaps(exonsByGene, bamfile, mode="IntersectionStrict", ignore.strand=StrandEntry, singleEnd=Single)  
   }   
   if (IntersectionNonEmpty==TRUE){             
      geneHitsSingletons <- GRanges::summarizeOverlaps(exonsByGene, bamfile, mode="IntersectionNotEmpty", ignore.strand=StrandEntry, singleEnd=Single)  
   }
  res = assays(geneHitsSingletons)$counts
  b=paste(BAM.name_Counts,"/",name,"_summarizeOverlapsCounts.txt",sep="")
  write.table(res, file = b , quote=FALSE, sep="\t", row.names=TRUE) 
}

  bplapply(bamlst , summarizeOverlapsFun)  #MAIN FUNCTION HERE

  txtfls <- list.files(paste(BAM.name_Counts,"/",sep=""),pattern="txt$",full.names=TRUE)
  name = strsplit(txtfls[1],"/")
  name = name[[1]][length(name[[1]])]  #estract the name of the txt file
  x = read.table(paste(BAM.name_Counts,"/",name,sep="") , header=TRUE, row.names=1)

if ( length(txtfls) >= 2 ) {
  for (i in txtfls[2:length(txtfls)]){ # there is a problem here when I have just one sample
   xi <- read.table(i, header=TRUE, row.names=1)
   x = cbind(x,xi)
}

print("Finished!")

  res=x

SaveInCache(countingreads.db, res, "res_key")

  write.table(res, file = paste(BAM.name_Counts,"/","/counts.txt",sep="") , quote=FALSE, sep="\t", row.names=TRUE)

   #write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Read Count Interface*, you clicked the **summarizeOverlaps** button at ", Sys.time()," and the count file has been saved in the ",BAM.name_Counts," folder!", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste("You chose the following count file: ",the.file,"the bam folder: ",Bam.FolderNew,"the GTF file: ",the.file,"the count mode: union = ",union,", IntersectionStrict = ",IntersectionStrict,", IntersectionNonEmpty = ",IntersectionNonEmpty," and StrandEntry: ",StrandEntry, sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file ='", the.file,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Bam.FolderNew ='", Bam.FolderNew,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project='",Project,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("union='",union,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("StrandEntry='",StrandEntry,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Single='", Single,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("IntersectionStrict='",IntersectionStrict,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("IntersectionNonEmpty='",IntersectionNonEmpty,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#gtf = makeTxDbFromGFF(the.file,format='gtf')",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#exonsByGene <- exonsBy(gtf, by='gene') ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#fls <- list.files(Bam.FolderNew,pattern='bam$',full.names=TRUE)",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#bamlst <- BamFileList(fls,obeyQname=TRUE) ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#Vector.Bam.FolderNew = strsplit(Bam.FolderNew,'/')[[1]]",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#BAM.name = Vector.Bam.FolderNew[length(Vector.Bam.FolderNew)]",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#BAM.name_Counts <- paste(getwd(),'RNASeqGUI_Projects',Project,'Results/summarizeOverlaps_Report',sep='/') ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "")

message5 <- paste("#dir.create(BAM.name_Counts, showWarnings = TRUE, recursive = FALSE)",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
     
message5 <- paste("#summarizeOverlapsFun <- function(bamfile){",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#name =  strsplit(bamfile$path,'/')",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#name = name[[1]][length(name[[1]])]  #estract the name of the forlder", sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#geneHitsSingletons <- NULL",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#if (union==TRUE){geneHitsSingletons <- summarizeOverlaps(exonsByGene, bamfile, mode='Union', ignore.strand=StrandEntry, singleEnd=Single)   }",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#if (IntersectionStrict==TRUE){geneHitsSingletons <- summarizeOverlaps(exonsByGene, bamfile, mode='IntersectionStrict', ignore.strand=StrandEntry, singleEnd=Single)   } ",sep="\n")  
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#if (IntersectionNonEmpty==TRUE){geneHitsSingletons <- summarizeOverlaps(exonsByGene, bamfile, mode='IntersectionNonEmpty', ignore.strand=StrandEntry, singleEnd=Single)   }",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#res = assays(geneHitsSingletons)$counts",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#b=paste(BAM.name_Counts,'/',name,'_summarizeOverlapsCounts.txt',sep='')",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#write.table(res, file = b , quote=FALSE, sep='\t', row.names=TRUE)}",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#bplapply(bamlst , summarizeOverlapsFun) ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

}

 message=paste(" FINISHED! ", sep="")
 print(message)

 message=paste("Results of 'summarizeOverlaps' have been saved in '",BAM.name_Counts,"' folder.", sep="")
 print(message)


}

res

}


countingFeatures <- function(the.file,Bam.FolderNew,Paired,unstr,pstr,rstr,Project,threads2,threads4,threads6,threads8,threads16){

require(GenomicRanges)
require(GenomicFeatures)
require(Rsubread)
require(Rsamtools)
require(BiocParallel)

print("Start calling countingFeatures function")

 #Project = 'pippo' 
 #the.file="/media/6b8a4404-f3e2-4fbc-9fc4-5a0ebc8d0635/2L_Drosophila_melanogaster.BDGP5.70.gtf"
 #Bam.FolderNew="/media/6b8a4404-f3e2-4fbc-9fc4-5a0ebc8d0635/Bam/demo2"
 #Paired = TRUE
 #strand = 0
 #Nthread = 8

 strand = 0
 Nthread = 0

if (unstr == TRUE) {
   strand = 0  #default 0 '0'(unstranded), '1' (stranded) and '2' (reversely stranded).
}

if (pstr == TRUE) {
   strand = 1  #default 0 '0'(unstranded), '1' (stranded) and '2' (reversely stranded).
}

if (rstr == TRUE) {
   strand = 2  #default 0 '0'(unstranded), '1' (stranded) and '2' (reversely stranded).
}

if (threads2 == TRUE) {
   Nthread = 2
}

if (threads4 == TRUE) {
   Nthread = 4
}

if (threads6 == TRUE) {
   Nthread = 6
}

if (threads8 == TRUE) {
   Nthread = 8
}

if (threads16 == TRUE) {
   Nthread = 16
}

featurecounts.db <- InitDb(db.name='featurecounts_db', db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
SaveInCache(featurecounts.db, the.file, "the_file_key")
SaveInCache(featurecounts.db, Project, "project_key")
SaveInCache(featurecounts.db, Bam.FolderNew, "bamfoldernew_key")
SaveInCache(featurecounts.db, Paired, "paired_key")
SaveInCache(featurecounts.db, strand, "strand_key")
SaveInCache(featurecounts.db, Nthread, "nthread_key")

fc_SE = NULL
file.counter <- 0

if(Sys.info()[[1]]=="Windows"){

 fls <- list.files(Bam.FolderNew,pattern="bam$",full.names=TRUE)  
 bamlst <- BamFileList(fls,obeyQname=TRUE) 

   Vector.Bam.FolderNew = strsplit(Bam.FolderNew,"\\\\")[[1]]
   BAM.name = Vector.Bam.FolderNew[length(Vector.Bam.FolderNew)]
   BAM.name_Counts <- paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\",BAM.name,"_FeatureCounts",sep="")
   dir.create(BAM.name_Counts, showWarnings = TRUE, recursive = TRUE)
   print(paste("The ",BAM.name_Counts," folder has been created to store the results of the 'Read Count Interface' inside the ", getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results folder.",sep="")) 

FeatureCountsFun <- function(bamfile){
  print("bamfile: ")
  print(bamfile)
  print("gtf: ")
  print(the.file)
fc_SE <- Rsubread::featureCounts(files=bamfile, annot.ext=the.file, isGTFAnnotationFile=TRUE,GTF.featureType="exon",GTF.attrType="gene_id",
     useMetaFeatures=TRUE,allowMultiOverlap=FALSE,nthreads=Nthread,strandSpecific=strand,countMultiMappingReads=FALSE,isPairedEnd=Paired)

  a=paste(substring(getwd(),1,nchar(getwd())),"\\RNASeqGUI_Projects\\",Project,"\\Results\\",sep="")

  name = strsplit((bamfile),"\\\\")
  name = name[[1]][length(name[[1]])]  #estract the name of the forlder
  b=paste(a,name,"_FeatureCounts.txt",sep="")
  write.table(fc_SE[1], file = b , quote=FALSE, sep="\t", row.names=TRUE) 
}

# apply_pb <- function()
# {
# 
# pb <- tkProgressBar("Started", "Some information in %",min=0,max=100,initial=0, width = 500)
# Sys.sleep(0.5)
# u <- c(0, sort(runif(30, 0, 12)), 12)
# for(i in u) {
#     Sys.sleep(0.3)
#     info <- sprintf("%d%% done. PLEASE WAIT !", round(i))
#     setTkProgressBar(pb, i, sprintf("(%s)", info), info)    
# }

# nfiles <- length(fls)
# SaveInCache(featurecounts.db, nfiles, "nfiles_key")
# 
# bplapply(fls, FeatureCountsFun)   #MAIN FUNCTION HERE
# Sys.sleep(1)
# close(pb)
# }
# apply_pb()

SaveInCache(featurecounts.db, nfiles, "nfiles_key")

bplapply(fls, FeatureCountsFun)   #MAIN FUNCTION HERE


#for (i in fls){
#  FeatureCountsFun(i)
#}

 message=paste("Results of Count Reads have been saved in RNASeqGUI_Projects\\", Project,"\\Results folder!", sep="")
 print(message)

     #write into the project report
     report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.txt",sep="") 
     message2 <- paste("You ckicked the 'FeatureCounts' button at ", Sys.time()," and the count file has been saved in the ", Project,"\\Results folder!", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n") 
     message2 <- paste("You chose the following count file: ",the.file,"the bam folder: ",Bam.FolderNew, sep="\n")
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
  SaveInCache(featurecounts.db, res, "res_key")

  write.table(res, file = paste(BAM.name_Counts,"\\","\\counts.txt",sep="") , quote=FALSE, sep="\t", row.names=TRUE)
  


#write into the project report
report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message2 <- paste(" * In the *Read Count Interface*, you clicked the **FeatureCounts** button at `", Sys.time(),"` and the count file has been saved in the `",BAM.name_Counts,"` folder.", sep="")
write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message2 <- paste("You chose the following count file: `",the.file,"`, the bam folder: `",Bam.FolderNew,"`, the GTF file: `",the.file,"`",sep="\n")
write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" This R code has been run:",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  the.file ='", the.file,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  Bam.FolderNew ='", Bam.FolderNew,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  Project='",Project,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  Paired='",Paired,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  strand='",strand,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  Nthread='",Nthread,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" # gtf = makeTranscriptDbFromGFF(the.file,format='gtf')",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" # exonsByGene <- exonsBy(gtf, by='gene') ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" # fls <- list.files(Bam.FolderNew,pattern='bam$',full.names=TRUE)",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" # bamlst <- BamFileList(fls,obeyQname=TRUE) ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  # Vector.Bam.FolderNew = strsplit(Bam.FolderNew,'\\')[[1]]",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  # BAM.name = Vector.Bam.FolderNew[length(Vector.Bam.FolderNew)]",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  # BAM.name_Counts <- paste(getwd(),'RNASeqGUI_Projects',Project,'Results\\featureCounts_Report',sep='\\') ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "")

message5 <- paste(" #  dir.create(BAM.name_Counts, showWarnings = TRUE, recursive = TRUE)",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" # FeatureCountsFun <- function(bamfile){",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" # fc_SE <- featureCounts(files=bamfile, annot.ext=the.file, isGTFAnnotationFile=TRUE,GTF.featureType='exon',GTF.attrType='gene_id',  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" # useMetaFeatures=TRUE,allowMultiOverlap=FALSE,nthreads=Nthread,strandSpecific=strand,countMultiMappingReads=FALSE,minMQS=0,  ", sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" # isPairedEnd=Paired,requireBothEndsMapped=FALSE,checkFragLength=FALSE,minFragLength=50,maxFragLength=600,  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  # countChimericFragments=TRUE,chrAliases=NULL,reportReads=FALSE)  ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   # name = strsplit((bamfile),'\\')   ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  #  name = name[[1]][length(name[[1]])]  #estract the name of the forlder   ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" #   b=paste(BAM.name_Counts,'\\',name,'_FeatureCounts.txt',sep='')   ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#    write.table(fc_SE[1], file = b , quote=FALSE, sep='\t', row.names=TRUE) }   ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#    bplapply(bamlst , FeatureCountsFun) ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

}else{ #Linux

 fls <- list.files(Bam.FolderNew,pattern="bam$",full.names=TRUE)  
 bamlst <- BamFileList(fls,obeyQname=TRUE) 

message5 <- paste("Paired='",Paired,"'", sep="")
print(message5)
if(strand == 0){
  message5 <- paste("strand= unstranded", sep="")
  print(message5)
}
if(strand == 1){
  message5 <- paste("strand= positive strand", sep="")
  print(message5)
}
if(strand == 2){
  message5 <- paste("strand= reverse strand", sep="")
  print(message5)
}
message5 <- paste("Nthread='",Nthread,"'", sep="")
print(message5)


   Vector.Bam.FolderNew = strsplit(Bam.FolderNew,"/")[[1]]
   BAM.name = Vector.Bam.FolderNew[length(Vector.Bam.FolderNew)]
   BAM.name_Counts <- paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",BAM.name,"_FeatureCounts",sep="")
   dir.create(BAM.name_Counts, showWarnings = TRUE, recursive = TRUE)
   print(paste("The ",BAM.name_Counts," folder has been created to store the results of the 'Read Count Interface' inside the ", getwd(),"/RNASeqGUI_Projects/",Project,"/Results folder.",sep="")) 

 #bamfile = fls[[1]]

FeatureCountsFun <- function(bamfile){
  print("bamfile: ")
  print(bamfile)
  print("gtf: ")
  print(the.file)
     fc_SE <- Rsubread::featureCounts(files=bamfile, annot.ext=the.file, isGTFAnnotationFile=TRUE,GTF.featureType='exon',GTF.attrType='gene_id',
     useMetaFeatures=TRUE,allowMultiOverlap=FALSE,nthreads=Nthread,strandSpecific=strand,countMultiMappingReads=FALSE,isPairedEnd=Paired)

  name = strsplit((bamfile),"/")
  name = name[[1]][length(name[[1]])]  #estract the name of the forlder
  b=paste(BAM.name_Counts,"/",name,"_FeatureCounts.txt",sep="")
  write.table(fc_SE[1], file = b , quote=FALSE, sep="\t", row.names=TRUE)
}

#lapply(fls, FeatureCountsFun)   #MAIN FUNCTION HERE
bplapply(fls, FeatureCountsFun)   #MAIN FUNCTION HERE


 message=paste("Results of Count Reads have been saved in RNASeqGUI_Projects/", Project,"/Results folder!", sep="")
 print(message)

     #write into the project report
     report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.txt",sep="") 
     message2 <- paste("You clicked the 'FeatureCounts' button at ", Sys.time()," and the count file has been saved in the ", Project,"/Results folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n") 
     message2 <- paste("You chose the following count file: ",the.file,"the bam folder: ",Bam.FolderNew, sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

  txtfls <- list.files(paste(BAM.name_Counts,"/",sep=""),pattern="txt$",full.names=TRUE)
  name = strsplit(txtfls[1],"/")
  name = name[[1]][length(name[[1]])]  #estract the name of the txt file
   # name = strsplit(name,".bam")
   # name = name[[1]][1]  #estract the name of the first sample
  x = read.table(paste(BAM.name_Counts,"/",name,sep="") , header=TRUE, row.names=1)
  for (i in txtfls[2:length(txtfls)]){ 
   xi <- read.table(i, header=TRUE, row.names=1)
   x = cbind(x,xi)
  }

  res=x
  SaveInCache(featurecounts.db, res, "res_key")

  write.table(res, file = paste(BAM.name_Counts,"/","/counts.txt",sep="") , quote=FALSE, sep="\t", row.names=TRUE)

   #write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Read Count Interface*, you clicked the **FeatureCounts** button at `", Sys.time(),"` and the count file has been saved in the `",BAM.name_Counts,"` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste("You chose the following count file: `",the.file,"`, the bam folder: `",Bam.FolderNew,"`, the GTF file: `",the.file,"`",sep="\n")
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

message5 <- paste("Paired='",Paired,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("strand='",strand,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Nthread='",Nthread,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#fls <- list.files(Bam.FolderNew,pattern='bam$',full.names=TRUE)",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#bamlst <- BamFileList(fls,obeyQname=TRUE) ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#Vector.Bam.FolderNew = strsplit(Bam.FolderNew,'/')[[1]]",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#BAM.name = Vector.Bam.FolderNew[length(Vector.Bam.FolderNew)]",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#BAM.name_Counts <- paste(getwd(),'RNASeqGUI_Projects',Project,'Results/featureCounts_Report',sep='/') ",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "")

message5 <- paste("#dir.create(BAM.name_Counts, showWarnings = TRUE, recursive = TRUE)",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
     
message5 <- paste("#FeatureCountsFun <- function(bamfile){",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#fc_SE<-Rsubread::featureCounts(files=bamfile,annot.ext=the.file,isGTFAnnotationFile=TRUE,GTF.featureType='exon'",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#GTF.attrType='gene_id',useMetaFeatures=TRUE,allowMultiOverlap=FALSE,nthreads=Nthread,strandSpecific=strand,",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#countMultiMappingReads=FALSE,isPairedEnd=Paired)",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#bplapply(fls, FeatureCountsFun) ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 }

 message=paste(" FINISHED! ", sep="")
 print(message)

 message=paste("Results of 'FeatureCounts' have been saved in ",BAM.name_Counts," folder!", sep="")
 print(message)

 res

}

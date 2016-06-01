roundcountfile <- function(Count.File,Project){

  res=NULL

  #Count.File = "/media/6b8a4404-f3e2-4fbc-9fc4-5a0ebc8d0635/a"

  counts=read.table(Count.File, row.names=1, header=TRUE)
  print(dim(counts))
  print(head(counts))
  counts = round(counts)
  print("Count file modified:")
  print(dim(counts))
  print(head(counts))
 
 if(Sys.info()[[1]]=="Windows"){

   write.table(counts, file = paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\round_counts.txt",sep=""), quote=FALSE, row.names=TRUE, sep="\t")
   message=paste("The file 'round_counts.txt' has just been written in RNASeqGUI_Projects\\", Project,"\\Results folder!", sep="")
   print(message)

 }else{ #Unix

   write.table(counts, file = paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/round_counts.txt",sep=""), quote=FALSE, row.names=TRUE, sep="\t")
   message=paste("The file 'round_counts.txt' has just been written in RNASeqGUI_Projects/", Project,"/Results folder!", sep="")
   print(message)

 }

 res <- counts

 res

}

cbinding <- function(Count.Folder,Project){

#Project = "pippo"
#Count.Folder = "/media/6b8a4404-f3e2-4fbc-9fc4-5a0ebc8d0635/q"

if(Sys.info()[[1]]=="Windows"){

  print("You selected the folder: ")
  print(Count.Folder)

  txtfls <- list.files(Count.Folder,pattern="$",full.names=TRUE)
  name = strsplit(txtfls[1],"\\\\")
  name = name[[1]][length(name[[1]])]  #estract the name of the txt file
  x = read.table(paste(Count.Folder,name,sep="\\") , header=FALSE, row.names=1)
  for (i in txtfls[2:length(txtfls)]){ 
   xi <- read.table(i, header=FALSE, row.names=1)
   x = cbind(x,xi)
  }

 message=paste("The table of counts has been saved in RNASeqGUI_Projects\\", Project,"\\Results folder!", sep="")
 print(message)

  res=x

  write.table(res, file = paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\table.txt",sep="") , quote=FALSE, sep="\t", row.names=TRUE)
 


}else{ #Linux

  print("You selected the folder: ")
  print(Count.Folder)

  txtfls <- list.files(Count.Folder,pattern="txt$",full.names=TRUE)
  name = strsplit(txtfls[1],"/")
  name = name[[1]][length(name[[1]])]  #estract the name of the txt file
  x = read.table(paste(Count.Folder,name,sep="/") , header=TRUE, row.names=1)
  print("Loaded file: ")
  print(dim(x))
  print(head(x))
  for (i in txtfls[2:length(txtfls)]){ 
   xi <- read.table(i, header=TRUE, row.names=1)
   print("Loaded file: ")
   print(dim(xi))
   print(head(xi))
   x = cbind(x,xi)
  }

 message=paste("The table of counts has been saved in RNASeqGUI_Projects/", Project,"/Results folder!", sep="")
 print(message)

  res=x

  write.table(res, file = paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/table.txt",sep="") , quote=FALSE, sep="\t", row.names=TRUE)

 }

 res

}

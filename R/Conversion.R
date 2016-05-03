conversion_function <- function(file.name,Project,hsa,mmu,dme,dre,Convers,Convers2,Convers3){

 require(biomaRt)
  
  justfilename <- substring(file.name, max(gregexpr("/",file.name)[[1]])+1)
 res=NULL
 file.name=read.table(file.name, header=TRUE, row.names=1)
 print(dim(file.name))
 print(head(file.name))
 print("Selecting a BioMart database and dataset. This step might take a while. PLEASE, WAIT!")
 if(Convers==TRUE){ #ensembl gene ids 
    if(hsa==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="hsapiens_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","hgnc_symbol"), mart=ensembl)
    }
    if(mmu==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="mmusculus_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","mgi_symbol"), mart=ensembl)
    } 
    if(dme==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="dmelanogaster_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","flybasename_gene"), mart=ensembl)
    } #to connect to a BioMart database
    if(dre==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="drerio_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","zfin_symbol"), mart=ensembl)
    } #to connect to a BioMart database
  }

  if(Convers2==TRUE){ # gencode names 
    print("Converting gene codes to ensembl gene ids")   
    newrownames = strsplit(rownames(file.name),"\\.")
    nrownames = NULL
    for (i in 1:length(newrownames) ){
        nrownames[i] = newrownames[[i]][1]
    }
    rownames(file.name) = nrownames
    print("genecode rewritten as follows:")
    print(head(file.name))
    if(hsa==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="hsapiens_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","hgnc_symbol"), mart=ensembl)
    }
    if(mmu==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="mmusculus_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","mgi_symbol"), mart=ensembl)
    } 
    if(dme==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="dmelanogaster_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","flybasename_gene"), mart=ensembl)
    } #to connect to a BioMart database
    if(dre==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="drerio_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","zfin_symbol"), mart=ensembl)
    } #to connect to a BioMart database
  }

  if(Convers3==TRUE){ # reverse conversion 
    if(hsa==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="hsapiens_gene_ensembl")
         results <- getBM(attributes = c("hgnc_symbol","ensembl_gene_id"), mart=ensembl)
    }
    if(mmu==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="mmusculus_gene_ensembl")
         results <- getBM(attributes = c("mgi_symbol","ensembl_gene_id"), mart=ensembl)
    } 
    if(dme==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="dmelanogaster_gene_ensembl")
         results <- getBM(attributes = c("flybasename_gene","ensembl_gene_id"), mart=ensembl)
    } #to connect to a BioMart database
    if(dre==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="drerio_gene_ensembl")
         results <- getBM(attributes = c("zfin_symbol","ensembl_gene_id"), mart=ensembl)
    } #to connect to a BioMart database
  }

 print("Conversion map:")
 dim(results)
 print(head(results,10))
 
 #write.table(results, file = paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/Conversion_map.txt",sep=""), quote=FALSE, row.names=FALSE)
 id=NULL
 print("Converting row names. PLEASE, WAIT!")

# for(i in 1:length(file.name[,1])){
#   if ( is.element(file.name[i,1], results[,1])==TRUE ) {
#      sub_results = subset( results, results[,1] == file.name[i,1] )
#      if (sub_results[,2] != ""){
#         id[i] = sub_results[,2]
#      }else{ id[i] = as.character(file.name[i,1]) }
#   }else{
#      id[i] = as.character(file.name[i,1])
#   }
# }

 for(i in 1:length(row.names(file.name))){
   if ( is.element(row.names(file.name)[i], results[,1])==TRUE ) {
      sub_results = subset( results, results[,1] == row.names(file.name)[i] )
      if (sub_results[,2] != ""){
         id[i] = sub_results[,2]
      }else{ id[i] = as.character(row.names(file.name)[i]) }
   }else{
      id[i] = as.character(row.names(file.name)[i])
   }
 }

 print(head(id))

 res=cbind(id,file.name)
 print(head(res))
 
 your.map <- cbind(rownames(file.name),id)
 colnames(your.map) <- NULL
 print(head(your.map))
 

 if(Sys.info()[[1]]=="Windows"){

   write.table(id, file = paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\", justfilename,"_list_of_gene_symbol.txt",sep=""), quote=FALSE, row.names=FALSE, sep="\t")
   message=paste("The file '", justfilename,"_list_of_gene_symbol.txt' has just been written in RNASeqGUI_Projects\\", Project,"\\Results folder!", sep="")
   print(message)
   write.table(res, file = paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\", justfilename,"_converted_table.txt",sep=""), quote=FALSE, row.names=FALSE, sep="\t")
   message=paste("The file '", justfilename,"_converted_table.txt' has just been written in RNASeqGUI_Projects\\", Project,"\\Results folder!", sep="")
   print(message)
   write.table(your.map, file = paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\", justfilename,"_converted_map.txt",sep=""), quote=FALSE, row.names=FALSE, col.names = F, sep="\t")
   message=paste("The file '", justfilename,"_converted_map.txt' has just been written in RNASeqGUI_Projects\\", Project,"\\Results folder!", sep="")
   print(message)

 }else{ #Unix

   write.table(id, file = paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/", justfilename,"_list_of_gene_symbol.txt",sep=""), quote=FALSE, row.names=FALSE, sep="\t")
   message=paste("The file '", justfilename,"_list_of_gene_symbol.txt' has just been written in RNASeqGUI_Projects/", Project,"/Results folder!", sep="")
   print(message)
   write.table(res, file = paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/", justfilename,"_converted_table.txt",sep=""), quote=FALSE, row.names=FALSE, sep="\t")
   message=paste("The file '", justfilename,"_converted_table.txt' has just been written in RNASeqGUI_Projects/", Project,"/Results folder!", sep="")
   print(message)
   write.table(your.map, file = paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/", justfilename,"_converted_map.txt",sep=""), quote=FALSE, row.names=FALSE, , col.names = F, sep="\t")
   message=paste("The file '", justfilename,"_converted_map.txt' has just been written in RNASeqGUI_Projects/", Project,"/Results folder!", sep="")
   print(message)
 }

 res

}

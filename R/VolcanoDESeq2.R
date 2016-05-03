VolcanoDESeq2 <- function(name,p,results_DESeq2,the.file, Project){

require(DESeq2)

print("This file has been loaded: ")
print(head(results_DESeq2))

 p = as.numeric(p)
 print ("Padj chosen: ")
 print(p)
 print ("Gene Id chosen: ")
 print(name)

if(Sys.info()[[1]]=="Windows"){
  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
  the.file2 = strsplit(the.file,"\\\\")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
  the.file2 = substring(the.file2,1,nchar(the.file2)-4)  # eliminates ".txt"

db.cache <- InitDb(db.name=paste(the.file2,'_volcanodeseq2_db',sep=''), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))

SaveInCache(db.cache, the.file, "thefile_key")
SaveInCache(db.cache, Project, "project_key")
SaveInCache(db.cache, results_DESeq2, "res_key")
SaveInCache(db.cache, name, "name_key")
SaveInCache(db.cache, p, "p_key")


 plot(results_DESeq2$log2FoldChange, -log10(results_DESeq2$padj), col = "black", main="DESeq2 Volcano Plot", xlab="log2FoldChange", ylab="-log10(padj)", pch=16,cex=0.4)

 DE_genes_DESeq2 = subset(results_DESeq2, padj<p)

 points(DE_genes_DESeq2$log2FoldChange, -log10(DE_genes_DESeq2$padj), col="red", cex=0.5)

 if (name!=""){

   OneGene = subset(results_DESeq2, row.names(results_DESeq2)==name)

   text(OneGene$log2FoldChange, -log(OneGene$padj), label=name, col="green", cex=0.6)

 }

 a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")

 outputName=paste(the.file2,"_Volcano_DESeq2.pdf", sep="")

 b=paste(a,outputName,sep="\\")

 dev.print(device = pdf, file=b)

#write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Result Inspection Interface*, you clicked the **Volcano Plot** button for DESeq2 at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 message2 <- paste("You chose the following count file: `",the.file,"`, Padj: `",p,"`, Project: `",Project,"`, ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2='",the.file2,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("db.cache <- InitDb(db.name=paste(the.file2,'_volcanodeseq2_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" results_DESeq2 <- LoadCachedObject(db.cache, 'res_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(db.cache, 'thefile_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" Project <- LoadCachedObject(db.cache, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("name <- LoadCachedObject(db.cache, 'name_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" p <- LoadCachedObject(db.cache, 'p_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print('This file has been loaded: ')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(head(results_DESeq2))",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" p = as.numeric(p)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" print ('Padj chosen: ')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" print(p)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" print ('Gene Id chosen: ')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" print(name)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" # results_DESeq2=as.matrix(results_DESeq2)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" plot(results_DESeq2$log2FoldChange, -log10(results_DESeq2$padj), col = 'black', main='DESeq2 Volcano Plot',",sep="") 
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" xlab='log2FoldChange', ylab='-log10(padj)', pch=16,cex=0.4)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" DE_genes_DESeq = subset(results_DESeq2, padj<p)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("points(DE_genes_DESeq$log2FoldChange, -log10(DE_genes_DESeq$padj), pch=19, col='red', cex=0.5),",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("if (name!=''){   OneGene = subset(results_DESeq2, row.names(results_DESeq2)==name)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("text(OneGene$log2FoldChange, -log(OneGene$padj), label=name, col='green', cex=0.6) }",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


  }else{ #Linux

  the.file2 = strsplit(the.file,"/")

  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

  the.file2 = substring(the.file2,1,nchar(the.file2)-4)  # eliminates ".txt"

db.cache <- InitDb(db.name=paste(the.file2,'_volcanodeseq2_db',sep=''), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))

SaveInCache(db.cache, the.file, "thefile_key")
SaveInCache(db.cache, Project, "project_key")
SaveInCache(db.cache, results_DESeq2, "res_key")
SaveInCache(db.cache, name, "name_key")
SaveInCache(db.cache, p, "p_key")


 plot(results_DESeq2$log2FoldChange, -log10(results_DESeq2$padj), col = "black", main="DESeq2 Volcano Plot", xlab="log2FoldChange", ylab="-log10(padj)", pch=19,cex=0.3)

 DE_genes_DESeq2 = subset(results_DESeq2, padj<p)

 points(DE_genes_DESeq2$log2FoldChange, -log10(DE_genes_DESeq2$padj), pch=19, col="red", cex=0.5)

 if (name!=""){

   OneGene = subset(results_DESeq2, row.names(results_DESeq2)==name)

   text(OneGene$log2FoldChange, -log(OneGene$padj), label=name, col="green", cex=0.6)

 }

 a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")

 outputName=paste(the.file2,"_Volcano_DESeq2.pdf", sep="")

 b=paste(a,outputName,sep="/")

 dev.print(device = pdf, file=b)

#write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Result Inspection Interface*, you clicked the **Volcano Plot** button for DESeq2 at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 message2 <- paste("You chose the following count file: `",the.file,"`, Padj: `",p,"`, Project: `",Project,"`, ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2='",the.file2,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("db.cache <- InitDb(db.name=paste(the.file2,'_volcanodeseq2_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" results_DESeq2 <- LoadCachedObject(db.cache, 'res_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(db.cache, 'thefile_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" Project <- LoadCachedObject(db.cache, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("name <- LoadCachedObject(db.cache, 'name_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" p <- LoadCachedObject(db.cache, 'p_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print('This file has been loaded: ')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(head(results_DESeq2))",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" p = as.numeric(p)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" print ('Padj chosen: ')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" print(p)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" print ('Gene Id chosen: ')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" print(name)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" plot(results_DESeq2$log2FoldChange, -log10(results_DESeq2$padj), col = 'black', main='DESeq2 Volcano Plot',",sep="") 
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" xlab='log2FoldChange', ylab='-log10(padj)', pch=19,cex=0.3)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("DE_genes_DESeq = subset(results_DESeq2, padj<p)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("points(DE_genes_DESeq$log2FoldChange, -log10(DE_genes_DESeq$padj), pch=19, col='red', cex=0.5)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("if (name!=''){   OneGene = subset(results_DESeq2, row.names(results_DESeq2)==name)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("text(OneGene$log2FoldChange, -log(OneGene$padj), label=name, col='green', cex=0.6) }",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

}
         
}

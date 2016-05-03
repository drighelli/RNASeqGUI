VolcanoNoiSeq <- function(name,p,results_noi,the.file, Project){

require(NOISeq)

 p = as.numeric(p)
 print ("Probability chosen: ")
 print(p)
 print ("Gene Id chosen: ")
 print(name)
 print('This file has been loaded: ')
 print(head(results_noi))
 results_noi <- read.table(the.file, header=TRUE, row.names=1)
 print(head(results_noi))

if(Sys.info()[[1]]=="Windows"){

 a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
 the.file = strsplit(the.file,"\\\\")
 the.file2 = the.file[[1]][length(the.file[[1]])]  #estract the namefile
 the.file2 = substring(the.file2,1,nchar(the.file2)-4)  # eliminates ".txt"

db.cache <- InitDb(db.name=paste(the.file2,'_volcanonois_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))

SaveInCache(db.cache, the.file, "thefile_key")
SaveInCache(db.cache, Project, "project_key")
SaveInCache(db.cache, results_noi, "res_key")
SaveInCache(db.cache, name, "name_key")
SaveInCache(db.cache, p, "p_key")

 plot(log10(results_noi[,2]/results_noi[,1]), - log10(1 - results_noi$prob + 0.000001), col = "black", main="NoiSeq Volcano Plot", xlab="log10FC", ylab="-log10(1 - results_noi$prob + 0.000001)", pch=19,cex=0.3)
 DE_genes_NoiSeq = subset(results_noi, prob>p)
 points(log10(DE_genes_NoiSeq[,2]/DE_genes_NoiSeq[,1]), DE_genes_NoiSeq$prob, pch=19, col="red", cex=0.5)

 if (name!=""){
   OneGene = subset(results_noi, row.names(results_noi)==name)
   text(log10(OneGene[,2]/OneGene[,1]), - log10(1 -  OneGene$prob + 0.000001), label=name, col="green", cex=0.6)
 }

 a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")

 outputName=paste(the.file2,"_Volcano_NoiSeq.pdf", sep="")

 b=paste(a,outputName,sep="\\")

 dev.print(device = pdf, file=b)

#write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Result Inspection Interface*, you clicked the **Volcano Plot** button for NOISeq at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 message2 <- paste("You chose the following count file: `",the.file,"`, prob: `",p,"`, Project: `",Project,"`, ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2='",the.file2,"'",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("db.cache <- InitDb(db.name=paste(the.file2,'_volcanonois_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    

message5 <- paste(" results_noi <- LoadCachedObject(db.cache, 'res_key')", sep="")
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

message5 <- paste("print(head(results_noi))",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("p = as.numeric(p)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print ('prob chosen: ')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(p)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print ('Gene Id chosen: ')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(name)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("plot(log10(results_noi[,2]/results_noi[,1]), -log10(1 - results_noi$prob + 0.000001), col = 'black',",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("main=paste('Volcano Plot',the.file2,sep=''),xlab='log10FC',ylab='-log10(1-results_noi$prob+0.000001)',pch=19,cex=0.3)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("DE_genes_NoiSeq = subset(results_noi, prob>p)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("points(log10(DE_genes_NoiSeq[,2]/DE_genes_NoiSeq[,1]),-log10(1-DE_genes_NoiSeq$prob+0.000001),",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("pch=19,col='red',cex=0.5)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("if (name!=''){    OneGene = subset(results_noi, row.names(results_noi)==name)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("text(OneGene$M, OneGene$prob, label=name, col='green', cex=0.6) }",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 }else{ #Linux

 the.file2 = strsplit(the.file,"/")
 the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
 the.file2 = substring(the.file2,1,nchar(the.file2)-4)  # eliminates ".txt"

db.cache <- InitDb(db.name=paste(the.file2,'_volcanonois_db',sep=''), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))

SaveInCache(db.cache, the.file, "thefile_key")
SaveInCache(db.cache, Project, "project_key")
SaveInCache(db.cache, results_noi, "res_key")
SaveInCache(db.cache, name, "name_key")
SaveInCache(db.cache, p, "p_key")


 plot(log10(results_noi[,2]/results_noi[,1]), -log10(1 - results_noi$prob + 0.000001), col = "black", main=paste("Volcano Plot ", the.file2,sep=""), xlab="log10FC", ylab="-log10(1 - results_noi$prob + 0.000001)",pch=19,cex=0.3)
 DE_genes_NoiSeq = subset(results_noi, prob>p)
 points(log10(DE_genes_NoiSeq[,2]/DE_genes_NoiSeq[,1]), -log10(1 - DE_genes_NoiSeq$prob + 0.000001) , pch=19, col="red", cex=0.5)
   if (name!=""){
     OneGene = subset(results_noi, row.names(results_noi)==name)
     #text(OneGene$M, OneGene$prob, label=name, col="green", cex=0.6)
     text(log10(OneGene[,2]/OneGene[,1]), - log10(1 -  OneGene$prob + 0.000001), label=name, col="green", cex=0.6)
   }

 a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")

 outputName=paste(the.file2,"_Volcano_NoiSeq.pdf", sep="")

 b=paste(a,outputName,sep="/")

 dev.print(device = pdf, file=b)

#write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Result Inspection Interface*, you clicked the **Volcano Plot** button for NOISeq at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 message2 <- paste("You chose the following count file: `",the.file,"`, prob: `",p,"`, Project: `",Project,"`, ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2='",the.file2,"'",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("db.cache <- InitDb(db.name=paste(the.file2,'_volcanonois_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    

message5 <- paste("results_noi <- LoadCachedObject(db.cache, 'res_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(db.cache, 'thefile_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("Project <- LoadCachedObject(db.cache, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("name <- LoadCachedObject(db.cache, 'name_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("p <- LoadCachedObject(db.cache, 'p_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print('This file has been loaded: ')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(head(results_noi))",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("p = as.numeric(p)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print ('prob chosen: ')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(p)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print ('Gene Id chosen: ')",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(name)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("plot(log10(results_noi[,2]/results_noi[,1]), -log10(1 - results_noi$prob + 0.000001), col = 'black',",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("main=paste('Volcano Plot',the.file2,sep=''),xlab='log10FC',ylab='-log10(1-results_noi$prob+0.000001)',pch=19,cex=0.3)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("DE_genes_NoiSeq = subset(results_noi, prob>p)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("points(log10(DE_genes_NoiSeq[,2]/DE_genes_NoiSeq[,1]),-log10(1-DE_genes_NoiSeq$prob+0.000001),",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("pch=19,col='red',cex=0.5)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("if (name!=''){    OneGene = subset(results_noi, row.names(results_noi)==name)",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("text(OneGene$M, OneGene$prob, label=name, col='green', cex=0.6) }",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

}
         
}


PCAfun <-
function(x,the.file,legendpos,Project){
  
  n = 2

  PCA=prcomp(t(log(x+1)))
  colors = c(rep("blue",n),rep("darkgreen",n),rep("red",n),rep("purple",n),rep("black",n),rep("green",n),rep("brown",n),rep("pink",n),rep("gold",n))

 if(Sys.info()[[1]]=="Windows"){
  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
  the.file2 = strsplit(the.file,"\\\\")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

 pcafun.db <- InitDb(db.name=paste(the.file2,'pcafun_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
 
 SaveInCache(pcafun.db, the.file, "the_file_key")
 SaveInCache(pcafun.db, Project, "project_key")
 SaveInCache(pcafun.db, legendpos, "legendpos_key")
 SaveInCache(pcafun.db, x, "pcafun_dataframe_key")
 
 SaveInCache(pcafun.db, PCA, "pca_key")
 SaveInCache(pcafun.db, colors, "colors_key")
 SaveInCache(pcafun.db, n, "n_key")
 condition=colnames(x)

  outputName=paste(the.file2,"_PCA.pdf",sep="")
  plot(PCA$x,pch=unclass(as.factor(condition)),col=colors,cex=1.5, main = outputName,lwd=2)

  legend(legendpos,legend=condition,pch=unclass(as.factor(condition)),col=colors,fill="transparent",border="NA")
  b=paste(a,outputName,sep="")
  dev.print(device = pdf, file=b)
  message=paste("The ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
  print(message)

 #write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **PCA** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste("You chose the following count file: `",the.file," . ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("pcafun.db <- InitDb(db.name=paste(the.file2,'pcafun_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(pcafun.db, 'pcafun_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("condition <- colnames(x)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(pcafun.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(pcafun.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("condition <- LoadCachedObject(pcafun.db, 'condition_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("legendpos <- LoadCachedObject(pcafun.db, 'legendpos_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("PCA <- LoadCachedObject(pcafun.db, 'pca_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("colors <- LoadCachedObject(pcafun.db, 'colors_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("n <- LoadCachedObject(pcafun.db, 'n_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 message5 <- paste("PCA=prcomp(t(log(x+1)))", sep="")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 message5 <- paste("colors = c(rep('blue',n),rep('darkgreen',n),rep('red',n),rep('purple',n),rep('black',n),rep('green',n),rep('brown',n),rep('pink',n),rep('gold',n))", sep="")
 write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Plots\\',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = strsplit(the.file,'\\')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("outputName=paste(the.file2,'_PCA.pdf',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("plot(PCA$x,pch=unclass(as.factor(condition)),col=colors,cex=1.5, main = outputName, lwd=2)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("legend(legendpos,legend=condition,pch=unclass(as.factor(condition)),col=colors,fill='transparent',border='NA')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 }else{ #Linux

 the.file2 = strsplit(the.file,"/")
 the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

 pcafun.db <- InitDb(db.name=paste(the.file2,'pcafun_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
 
 SaveInCache(pcafun.db, the.file, "the_file_key")
 SaveInCache(pcafun.db, Project, "project_key")
 SaveInCache(pcafun.db, legendpos, "legendpos_key")
 SaveInCache(pcafun.db, x, "pcafun_dataframe_key")
 
 SaveInCache(pcafun.db, PCA, "pca_key")
 SaveInCache(pcafun.db, colors, "colors_key")
 SaveInCache(pcafun.db, n, "n_key")

 a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
 outputName=paste(the.file2,"_PCA.pdf",sep="")

 print("You loaded the following table of counts:")
 print(head(x))
 condition=colnames(x)
 
 
 plot(PCA$x,pch=unclass(as.factor(condition)),col=colors,cex=1.5, main = outputName,lwd=2)

 legend(legendpos,legend=condition,pch=unclass(as.factor(condition)),col=colors,fill="transparent",border="NA")
 b=paste(a,outputName,sep="")
 dev.print(device = pdf, file=b)
 message=paste("The ",outputName," file has been saved in the ", Project,"/Plots folder!", sep="")
 print(message)

 #write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **PCA** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste("You chose the following count file: `",the.file," . ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("pcafun.db <- InitDb(db.name=paste(the.file2,'pcafun_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(pcafun.db, 'pcafun_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("condition <- colnames(x)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(pcafun.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(pcafun.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("condition <- LoadCachedObject(pcafun.db, 'condition_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("legendpos <- LoadCachedObject(pcafun.db, 'legendpos_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("PCA <- LoadCachedObject(pcafun.db, 'pca_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("colors <- LoadCachedObject(pcafun.db, 'colors_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("n <- LoadCachedObject(pcafun.db, 'n_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("PCA=prcomp(t(log(x+1)))", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("colors = c(rep('blue',n),rep('darkgreen',n),rep('red',n),rep('purple',n),rep('black',n),", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("rep('green',n),rep('brown',n),rep('pink',n),rep('gold',n))", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Plots/',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = strsplit(the.file,'/')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("outputName=paste(the.file2,'_PCA.pdf',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("plot(PCA$x,pch=unclass(as.factor(condition)),col=colors,cex=1.5, main = outputName, lwd=2)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("legend(legendpos,legend=condition,pch=unclass(as.factor(condition)),col=colors,", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("fill='transparent',border='NA')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 }

}

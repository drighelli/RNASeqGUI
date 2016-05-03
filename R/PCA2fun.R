PCA2fun <-
function(x,the.file,legendpos,Project){

 pca=prcomp(t(log(x+1)))

 if(Sys.info()[[1]]=="Windows"){
 a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
 the.file = strsplit(the.file,"\\\\") 
 the.file2 = the.file[[1]][length(the.file[[1]])]  #estract the namefile

 pca2fun.db <- InitDb(db.name=paste(the.file2,'pca2fun_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
 SaveInCache(pca2fun.db, the.file, "the_file_key")
 SaveInCache(pca2fun.db, Project, "project_key")
 SaveInCache(pca2fun.db, legendpos, "legendpos_key")
 SaveInCache(pca2fun.db, x, "pca2fun_dataframe_key")
 SaveInCache(pca2fun.db, pca, "pca_key")

 outputName=paste(the.file,"_PCA3D.pdf",sep="")
 condition=colnames(x)
 scatterplot3d(pca$x[,1], pca$x[,2], pca$x[,3], main = outputName, pch=unclass(as.factor(condition)), xlab="PCA1", ylab="PCA2", zlab="PCA3")
 legend(legendpos,legend=condition,pch=unclass(as.factor(condition)),fill="transparent",border="NA")
 b=paste(a,outputName,sep="")
 dev.print(device = pdf, file=b)
 message=paste("The ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
 print(message)

 #write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **PCA3D** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
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

message5 <- paste("pca2fun.db <- InitDb(db.name='pca2fun_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(pca2fun.db, 'pca2fun_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(pca2fun.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(pca2fun.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("condition <- colnames(x)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("condition <- LoadCachedObject(pca2fun.db, 'condition_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("legendpos <- LoadCachedObject(pca2fun.db, 'legendpos_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message1 <- paste("condition = c(")
     write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message2 <- paste("'",condition[1:(length(condition)-1)],"',",sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message3 <- paste("'",condition[length(condition)],"'",sep="")
     write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message4 <- paste(") ")
     write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

message5 <- paste("outputName=paste(the.file2,'_PCA3D.pdf',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("pca <- LoadCachedObject(pca2fun.db, 'pca_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("# pca=prcomp(t(log(x+1)))", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("scatterplot3d(pca$x[,1], pca$x[,2], pca$x[,3], highlight.3d=TRUE, col.axis='blue', col.grid='lightblue', main = outputName, pch=unclass(as.factor(condition)), xlab='PCA1', ylab='PCA2', zlab='PCA3')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("legend(legendpos,legend=condition,pch=unclass(as.factor(condition)),fill='transparent',border='NA')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 }else{ #Linux

 the.file2 = strsplit(the.file,"/")
 the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

 pca2fun.db <- InitDb(db.name=paste(the.file2,'pca2fun_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
 SaveInCache(pca2fun.db, the.file, "the_file_key")
 SaveInCache(pca2fun.db, Project, "project_key")
 SaveInCache(pca2fun.db, legendpos, "legendpos_key")
 SaveInCache(pca2fun.db, x, "pca2fun_dataframe_key")
 SaveInCache(pca2fun.db, pca, "pca_key")

 condition=colnames(x)
 a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
 outputName=paste(the.file2,"_PCA3D.pdf",sep="")
 scatterplot3d(pca$x[,1], pca$x[,2], pca$x[,3], highlight.3d=TRUE, col.axis='blue', col.grid='lightblue', main = outputName, pch=unclass(as.factor(condition)), xlab='PCA1', ylab='PCA2', zlab='PCA3')
 legend(legendpos,legend=condition,pch=unclass(as.factor(condition)),fill="transparent",border="NA")
 b=paste(a,outputName,sep="")
 dev.print(device = pdf, file=b)
 message=paste("The ",outputName," file has been saved in the ", Project,"/Plots folder!", sep="")
 print(message)

 #write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **PCA3D** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
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

message5 <- paste("pca2fun.db <- InitDb(db.name='pca2fun_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(pca2fun.db, 'pca2fun_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file <- LoadCachedObject(pca2fun.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project <- LoadCachedObject(pca2fun.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("condition <- colnames(x)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("condition <- LoadCachedObject(pca2fun.db, 'condition_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("legendpos <- LoadCachedObject(pca2fun.db, 'legendpos_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message1 <- paste("condition = c(")
     write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message2 <- paste("'",condition[1:(length(condition)-1)],"',",sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message3 <- paste("'",condition[length(condition)],"'",sep="")
     write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message4 <- paste(") ")
     write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

message5 <- paste("outputName=paste(the.file2,'_PCA3D.pdf',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("pca <- LoadCachedObject(pca2fun.db, 'pca_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("# pca=prcomp(t(log(x+1)))", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("scatterplot3d(pca$x[,1], pca$x[,2], pca$x[,3], highlight.3d=TRUE, col.axis='blue', col.grid='lightblue', main = outputName, pch=unclass(as.factor(condition)), xlab='PCA1', ylab='PCA2', zlab='PCA3')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("legend(legendpos,legend=condition,pch=unclass(as.factor(condition)),fill='transparent',border='NA')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  }

}

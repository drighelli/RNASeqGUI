QPlotDensity <- function(x, the.file, log, Project){

require(ggplot2)



if(Sys.info()[[1]]=="Windows"){
 a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
 the.file2 = strsplit(the.file,"\\\\")
 the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
 outputName=paste(the.file2,"_QPlotDensity.pdf",sep="")

qplotdensity.db <- InitDb(db.name=paste(the.file2,'qplotdensity_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))

SaveInCache(qplotdensity.db, the.file, "the_file_key")
SaveInCache(qplotdensity.db, Project, "project_key")
SaveInCache(qplotdensity.db, log, "log_key")
SaveInCache(qplotdensity.db, x, "_dataframe_key")


  df = list(data.frame())

  for ( i in 1:length(colnames(x)) ) {

    pp=x[,i]

    colnames(x)[i]

    col=rep(colnames(x)[i],length(pp))

    df[[i]]=data.frame(counts=pp,idsample <- col)

    if (i>1) { 

       df[[i]] = rbind(df[[i-1]],df[[i]]) 

    }

  }

 df=data.frame(df[[length(colnames(x))]])
SaveInCache(qplotdensity.db, df, "df_key")

 samples = df$idsample....col
 if (log==TRUE) { print(qplot(log(counts+1), main=outputName, data = df, geom = "density",colour=samples)) }

 if (log==FALSE){ print(qplot(counts,        main=outputName, data = df, geom = "density",colour=samples)) }
 b=paste(a,outputName,sep="")
 dev.print(device = pdf, file=b)
 message=paste("The ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
 print(message)

#write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **QPlot Density** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
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

message5 <- paste("require(ggplot2) ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("qplotdensity.db <- InitDb(db.name=paste(the.file2,'qplotdensity_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x = read.table('",the.file,"',header=TRUE,row.names=1)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x=data.matrix(x, rownames.force = NA)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file ='",the.file,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  Project ='", Project,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  log ='", log,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Plots/',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   the.file2 = strsplit(the.file,'/')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  outputName=paste(the.file2,'_QPlotDensity.pdf',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  #df = list(data.frame()) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  #for ( i in 1:length(colnames(x)) ) { pp=x[,i] ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   # colnames(x)[i] ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   # col=rep(colnames(x)[i],length(pp)) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("    #df[[i]]=data.frame(counts=pp,idsample <- col) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  #if (i>1) {   df[[i]] = rbind(df[[i-1]],df[[i]]) } } ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   #df=data.frame(df[[length(colnames(x))]]) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   df <- LoadCachedObject(qplotdensity.db, 'df_key') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   samples = df$idsample....col ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  if (log==TRUE) { print(qplot(log(counts+1),main=the.file2, data = df, geom = 'density',colour=samples)) } ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  if (log==FALSE){ print(qplot(counts       ,main=the.file2, data = df, geom = 'density',colour=samples)) } ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("   b=paste(a,outputName,sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("  # dev.print(device = pdf, file=b)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

}else{ #Linux

 the.file2 = strsplit(the.file,"/")
 the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
 a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
 outputName=paste(the.file2,"_QPlotDensity.pdf",sep="")

qplotdensity.db <- InitDb(db.name=paste(the.file2,'qplotdensity_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))

SaveInCache(qplotdensity.db, the.file, "the_file_key")
SaveInCache(qplotdensity.db, Project, "project_key")
SaveInCache(qplotdensity.db, log, "log_key")
SaveInCache(qplotdensity.db, x, "_dataframe_key")


  df = list(data.frame())

  for ( i in 1:length(colnames(x)) ) {

    pp=x[,i]

    colnames(x)[i]

    col=rep(colnames(x)[i],length(pp))

    df[[i]]=data.frame(counts=pp,idsample <- col)

    if (i>1) { 

       df[[i]] = rbind(df[[i-1]],df[[i]]) 

    }

  }

 df=data.frame(df[[length(colnames(x))]])
SaveInCache(qplotdensity.db, df, "df_key")

 samples = df$idsample....col
 if (log==TRUE) { print(qplot(log(counts+1), main=outputName, data = df, geom = "density",colour=samples)) }

 if (log==FALSE){ print(qplot(counts,        main=outputName, data = df, geom = "density",colour=samples)) }
 b=paste(a,outputName,sep="")
 dev.print(device = pdf, file=b)
 message=paste("The ",outputName," file has been saved in the ", Project,"/Plots folder!", sep="")
 print(message)

#write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **QPlot Density** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste("You chose the following count file: `",the.file," . ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("require(ggplot2) ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2='",the.file2,"'",sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("qplotdensity.db <- InitDb(db.name=paste(the.file2,'qplotdensity_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x=read.table('",the.file,"',header=TRUE,row.names=1)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x=data.matrix(x, rownames.force = NA)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file='",the.file,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("Project='", Project,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("log='", log,"'", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#df=list(data.frame())", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#for(i in 1:length(colnames(x))){ pp=x[,i]", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#colnames(x)[i]", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#col=rep(colnames(x)[i],length(pp))", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#df[[i]]=data.frame(counts=pp,idsample <- col)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#if(i>1){df[[i]]=rbind(df[[i-1]],df[[i]])}}", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#df=data.frame(df[[length(colnames(x))]])", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("df<-LoadCachedObject(qplotdensity.db, 'df_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("samples=df$idsample....col", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("if(log==TRUE){print(qplot(log(counts+1),main=the.file2,data=df,geom='density',colour=samples))} ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("if(log==FALSE){print(qplot(counts,main=the.file2,data=df,geom='density',colour=samples))} ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 }

}

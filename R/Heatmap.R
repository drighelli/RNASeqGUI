Heatmap <- function(x,the.file,listofnames,log,Project){

  # x=read.table("a",header=T,row.names=1)
  # listofnames=read.table("listofgenes.txt",header=T)
  # listofnames <- scan("listofgenes.txt", what="", sep="\n")
  print("You selected the following count file:")
  print(head(x))
  print("You selected the following list of genes:")
  print(head(listofnames))

  require("gplots")
    
if(Sys.info()[[1]]=="Windows"){
  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
  the.file2 = strsplit(the.file,"\\\\")
  the.file2 = the.file[[1]][length(the.file2[[1]])]  #estract the namefile

  heatmap.db <- InitDb(db.name=paste(the.file2,'heatmap_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  SaveInCache(heatmap.db, x, "heatmap_dataframe_key")
  SaveInCache(heatmap.db, the.file, "the_file_key")
  SaveInCache(heatmap.db, log, "log_key")
  SaveInCache(heatmap.db, Project, "project_key")
  SaveInCache(heatmap.db, listofnames, "listofnames_key")

  outputName=NULL

  if (log==TRUE) { 

    outputName=paste(the.file2,"_LogHeatmap.pdf",sep="")
    means=rowMeans(x)

    select = NULL
    select =  subset(x, row.names(x)==listofnames[1])
    for (i in 2:length(listofnames)){
      temp_select =  subset(x, row.names(x)==listofnames[i])
      select = rbind(select, temp_select)
    }


    #select = NULL
    #select =  subset(x, row.names(x)==row.names(listofnames)[1])
    #for (i in 2:dim(listofnames)[1]){
    #  temp_select =  subset(x, row.names(x)==row.names(listofnames)[i])
    #  select = rbind(select, temp_select)
    #}

    x2 <- select
    my_palette = colorRampPalette(c("green", "black", "red"))(n = 1000)
    x2  <- as.matrix(log(x2+1))
    heatmap.2(x2, col=my_palette, dendrogram="col", cexRow=0.7, cexCol=0.9,srtCol=40, margins = c(7, 7),  density.info="none", trace="none", symkey=TRUE, symm=TRUE, symbreaks=F, main = outputName)

  }

 if (log==FALSE) {

    outputName=paste(the.file2,"_Heatmap.pdf",sep="")   
    means=rowMeans(x)

    select = NULL
    select =  subset(x, row.names(x)==listofnames[1])
    for (i in 2:length(listofnames)){
      temp_select =  subset(x, row.names(x)==listofnames[i])
      select = rbind(select, temp_select)
    }

    x <- select
    my_palette = colorRampPalette(c("green", "black", "red"))(n = 1000)
    x  <- as.matrix(x)
    heatmap.2(x, col=my_palette, dendrogram="col", cexRow=0.7, cexCol=0.9,srtCol=40, margins = c(7, 7),  density.info="none", trace="none", symkey=F, symm=F, symbreaks=F, main = outputName)

  }

  b=paste(a,outputName,sep="")

  dev.print(device = pdf, file=b)
  message=paste("The ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
  print(message)

 #write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **Heatmap** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
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

message5 <- paste("heatmap.db <- InitDb(db.name=paste(the.file2,'heatmap_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("x <- LoadCachedObject(heatmap.db, 'heatmap_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(heatmap.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("log <- LoadCachedObject(heatmap.db, 'log_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("Project <- LoadCachedObject(heatmap.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("listofnames <- LoadCachedObject(heatmap.db, 'listofnames_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Plots\\',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file2 = strsplit(the.file,'\\\\')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("outputName=NULL ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("if (log==TRUE) {    outputName=paste(the.file2,'_LogHeatmap.pdf',sep='') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  means=rowMeans(x)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("select = NULL", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("select =  subset(x, row.names(x)==listofnames[1])", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("for (i in 2:length(listofnames)){temp_select =  subset(x, row.names(x)==listofnames[i])", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  select = rbind(select, temp_select)}", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  x <- select", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("    my_palette = colorRampPalette(c('green', 'black', 'red'))(n = 1000)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  x  <- as.matrix(log(x+1)) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("      heatmap.2(x, col=my_palette, dendrogram='col', cexRow=0.7, cexCol=0.9,srtCol=40, margins = c(7, 7),  density.info='none', trace='none', symkey=F, symm=F, symbreaks=F, main = outputName) }", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("if (log==FALSE) { outputName=paste(the.file2,'_Heatmap.pdf',sep='') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  means=rowMeans(x)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("select = NULL", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("select =  subset(x, row.names(x)==listofnames[1])", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("for (i in 2:length(listofnames)){temp_select =  subset(x, row.names(x)==listofnames[i])", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  select = rbind(select, temp_select)}", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  x <- select", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("    my_palette = colorRampPalette(c('green', 'black', 'red'))(n = 1000)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  x  <- as.matrix(x) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("      heatmap.2(x, col=my_palette, dendrogram='col', cexRow=0.7, cexCol=0.9,srtCol=40, margins = c(7, 7),  density.info='none', trace='none', symkey=F, symm=F, symbreaks=F, main = outputName) }", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 }else{ #Linux

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
  the.file2 = strsplit(the.file,"/")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

  heatmap.db <- InitDb(db.name=paste(the.file2,'heatmap_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  SaveInCache(heatmap.db, x, "heatmap_dataframe_key")
  SaveInCache(heatmap.db, the.file, "the_file_key")
  SaveInCache(heatmap.db, log, "log_key")
  SaveInCache(heatmap.db, Project, "project_key")
  SaveInCache(heatmap.db, listofnames, "listofnames_key")

  outputName=NULL

  if (log==TRUE) { 
 
    outputName=paste(the.file2,"_LogHeatmap.pdf",sep="")
    means=rowMeans(x)

    select = NULL
    select =  subset(x, row.names(x)==listofnames[1])
    for (i in 2:length(listofnames)){
      temp_select =  subset(x, row.names(x)==listofnames[i])
      select = rbind(select, temp_select)
    }

    x <- select
    #hmcol = colorRampPalette(brewer.pal(9,"GnBu"))(100)
    my_palette = colorRampPalette(c("green", "black", "red"))(n = 1000)
    x  <- as.matrix(log(x+1))
    # heatmap(x, col = hmcol, keep.dendro = FALSE,   cexRow=0.7, margins = c(10, 10), density.info="none", trace="none", symkey=F, symm=F, symbreaks=F, main = outputName)
    heatmap.2(x, col=my_palette, dendrogram="col", cexRow=0.7, cexCol=0.9,srtCol=40, margins = c(7, 7),  density.info="none", trace="none", symkey=F, symm=F, symbreaks=F, main = outputName)

  }

 if (log==FALSE) {

    outputName=paste(the.file2,"_Heatmap.pdf",sep="")
    means=rowMeans(x)

    select = NULL
    select =  subset(x, row.names(x)==listofnames[1])
    for (i in 2:length(listofnames)){
      temp_select =  subset(x, row.names(x)==listofnames[i])
      select = rbind(select, temp_select)
    }

    x <- select 
    my_palette = colorRampPalette(c("green", "black", "red"))(n = 1000)
    x  <- as.matrix(x)
    heatmap.2(x, col=my_palette, dendrogram="col", cexRow=0.7, cexCol=0.9,srtCol=40, margins = c(7, 7),  density.info="none", trace="none", symkey=F, symm=F, symbreaks=F, main = outputName)


  }

  b=paste(a,outputName,sep="")

  dev.print(device = pdf, file=b)
  message=paste("The ",outputName," file has been saved in the ", Project,"/Plots folder!", sep="")
  print(message)

 #write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
     message2 <- paste(" * In the *Data Exploration Interface*, you clicked the **Heatmap** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
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

message5 <- paste("heatmap.db <- InitDb(db.name=paste(the.file2,'heatmap_db',sep=''), db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("x <- LoadCachedObject(heatmap.db, 'heatmap_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(heatmap.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("log <- LoadCachedObject(heatmap.db, 'log_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("Project <- LoadCachedObject(heatmap.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("listofnames <- LoadCachedObject(heatmap.db, 'listofnames_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Plots/',sep='')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file2 = strsplit(the.file,'/')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("outputName=NULL ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("if (log==TRUE) {    outputName=paste(the.file2,'_LogHeatmap.pdf',sep='') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("  means=rowMeans(x)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("select = NULL", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("select =  subset(x, row.names(x)==listofnames[1])", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("for (i in 2:length(listofnames)){temp_select =  subset(x, row.names(x)==listofnames[i])", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("select = rbind(select, temp_select)}", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("my_palette = colorRampPalette(c('green', 'black', 'red'))(n = 1000)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("x <- select", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("x  <- as.matrix(log(x+1)) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("heatmap.2(x, col=my_palette, dendrogram='col', cexRow=0.7, cexCol=0.9,srtCol=40, margins = c(7, 7),", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("density.info='none', trace='none', symkey=F, symm=F, symbreaks=F, main = outputName) }", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("if (log==FALSE) { outputName=paste(the.file2,'_Heatmap.pdf',sep='') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("means=rowMeans(x)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("select = NULL", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("select =  subset(x, row.names(x)==listofnames[1])", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("for (i in 2:length(listofnames)){temp_select =  subset(x, row.names(x)==listofnames[i])", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("select = rbind(select, temp_select)}", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("my_palette = colorRampPalette(c('green', 'black', 'red'))(n = 1000)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("x <- select", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("x  <- as.matrix(x) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("heatmap.2(x, col=my_palette, dendrogram='col', cexRow=0.7, cexCol=0.9,srtCol=40, margins = c(7, 7),", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("density.info='none', trace='none', symkey=F, symm=F, symbreaks=F, main = outputName) }", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 }

}

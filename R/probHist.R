probHist <- function(results_NoiSeq, the.file, Project){

  #results_NoiSeq <- read.table("a_results_NoiSeq.txt", header=TRUE, row.names=1)
  
  db.cache <- InitDb(db.name='probhist_db', db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(db.cache, the.file, "thefile_key")
  SaveInCache(db.cache, Project, "project_key")
  SaveInCache(db.cache, results_NoiSeq, "res_key")
  
  print('This file has been loaded: ')
  print(head(results_NoiSeq))
  hist(results_NoiSeq$prob, breaks=100, col="purple", border="slateblue", main="Prob Histogram", xlab="prob", ylab="Frequency")
  
  if(Sys.info()[[1]]=="Windows"){
    a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
    the.file = strsplit(the.file,"\\\\")
    the.file = the.file[[1]][length(the.file[[1]])]   #estract the namefile
    
    the.file = substring(the.file,1,nchar(the.file)-4)  # eliminates ".txt"
    
    outputName=paste(the.file,"_prob_hist.pdf", sep="")
    
    b=paste(a,outputName,sep="\\")
    
    dev.print(device = pdf, file=b)
    
    #write into the project report
    report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *Result Inspection Interface*, you clicked the **Prob Hist** button for NoiSeq at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("You chose the following count file: `",the.file,"`, Project: `",Project,"`, ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("db.cache <- InitDb(db.name='probhist_db', db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" results_NoiSeq <- LoadCachedObject(db.cache, 'res_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file <- LoadCachedObject(db.cache, 'thefile_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" Project <- LoadCachedObject(db.cache, 'project_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
#     message5 <- paste("results_NoiSeq = read.table(", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("'",the.file,"',header=TRUE,row.names=1)", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("the.file ='",the.file,"'", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("Project ='", Project,"'", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("print('This file has been loaded: ')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("print(head(results_NoiSeq))",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("the.file2 = strsplit(the.file,'\\')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("the.file2 = substring(the.file2,1,nchar(the.file2)-4)  # eliminates '.txt'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("hist(results_NoiSeq$prob, breaks=100, col='purple', border='slateblue', main='Prob Histogram', xlab='prob', ylab='Frequency') ",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("outputName=paste(the.file2,'_prob_hist.pdf', sep='')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Plots\\',sep='')",sep="")
    
    message5 <- paste("b=paste(a,outputName,sep='\\')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("# dev.print(device = pdf, file=b)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  }else{ # Linux
  
    a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
    
    the.file2 = strsplit(the.file,"/")
    
    the.file2 = the.file2[[1]][length(the.file2[[1]])]   #estract the namefile
    
    the.file2 = substring(the.file2,1,nchar(the.file2)-4)  # eliminates ".txt"
    
    outputName=paste(the.file2,"_prob_hist.pdf", sep="")
    
    b=paste(a,outputName,sep="/")
    
    dev.print(device = pdf, file=b)
    
    #write into the project report
    report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *Result Inspection Interface*, you clicked the **Prob Hist** button for NoiSeq at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("You chose the following count file: `",the.file,"`, Project: `",Project,"`, ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("db.cache <- InitDb(db.name='probhist_db', db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" results_NoiSeq <- LoadCachedObject(db.cache, 'res_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file <- LoadCachedObject(db.cache, 'thefile_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" Project <- LoadCachedObject(db.cache, 'project_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
#     message5 <- paste("results_NoiSeq = read.table(", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("'",the.file,"',header=TRUE,row.names=1)", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("the.file ='",the.file,"'", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("Project ='", Project,"'", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("print('This file has been loaded: ')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("print(head(results_NoiSeq))",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("the.file2 = strsplit(the.file,'/')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("the.file2 = substring(the.file2,1,nchar(the.file2)-4)  # eliminates '.txt'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
  message5 <- paste("hist(results_NoiSeq$prob, breaks=100, col='purple', border='slateblue', main='Prob Histogram', xlab='prob', ylab='Frequency') ",sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     message5 <- paste("hist(results_NoiSeq$prob, breaks=100, col='purple', border='slateblue', ",sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("main='Prob Histogram', xlab='prob', ylab='Frequency') ",sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("outputName=paste(the.file2,'_prob_hist.pdf', sep='')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Plots/',sep='')",sep="")
    
    message5 <- paste("b=paste(a,outputName,sep='/')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("# dev.print(device = pdf, file=b)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  }

}

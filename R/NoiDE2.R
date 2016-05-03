NoiDE2 <- function(x, conditions, TissueRuns, technical, biological, p, the.file, Project){

  require(NOISeq)

if(Sys.info()[[1]]=="Windows"){

  the.file2 = strsplit(the.file,"\\\\") 
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
  noide2.db <- InitDb(db.name=paste(the.file2,'noide2_db',sep="_"), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))  
  SaveInCache(noide2.db, the.file, "the_file_key")
  SaveInCache(noide2.db, Project, "project_key")
  SaveInCache(noide2.db, conditions, "conditions_key")
  SaveInCache(noide2.db, TissueRuns, "tissueruns_key")
  SaveInCache(noide2.db, x, "maindataframe_key")
  p = as.numeric(p)
  SaveInCache(noide2.db, p, "p_key")
  print("You loaded this count file: ")
  print(head(as.matrix(x)))
  mynoiseq = NULL
  if (technical == TRUE){ # technical replicate 

     print("NOISeq has been started on TECHNICAL replicates")
     myfactors = data.frame(Tissue = conditions, TissueRun = TissueRuns)
     mydata <- NOISeq::readData(data=x, factors = myfactors)
     mynoiseq = noiseq(mydata, k = 0.5, norm = "n", factor="Tissue", pnr = 0.2, nss = 5, v = 0.02, lc = 0, replicates = "technical")

  }else{ # biological replicate

     print("NOISeqBIO has been started on BIOLOGICAL replicates")
     myfactors = data.frame(Tissue = conditions, TissueRun = TissueRuns)
     mydata <- NOISeq::readData(data=x, factors=myfactors)
     mynoiseq = noiseqbio(mydata, k = 0.5, norm = "n", factor="Tissue", lc = 0, r = 20, adj = 1.5, plot = FALSE, a0per = 0.9, random.seed = 12345, filter = 0)

  }
  SaveInCache(noide2.db, mynoiseq, "mynoiseq_key")
  print('First five lines of the results.')
  print( head(mynoiseq@results[[1]]) )
  list_DE_NOISEQ = subset(mynoiseq@results[[1]], prob > p) # select significant genes
  SaveInCache(noide2.db, list_DE_NOISEQ, "listdenoiseq_key")

  
  #BUILD PIE CHART
  list_rest_of_genes_NOISEQ = NULL
  if (technical == TRUE){  
    list_rest_of_genes_NOISEQ = subset(mynoiseq@results[[1]], prob <= p | ranking==0)  # select rest of genes 
    list_UP_NOISEQ   = subset(list_DE_NOISEQ, list_DE_NOISEQ$M > 0)
    list_DOWN_NOISEQ = subset(list_DE_NOISEQ, list_DE_NOISEQ$M < 0)    
  }else{  
    list_rest_of_genes_NOISEQ = subset(mynoiseq@results[[1]], prob <= p) 
    list_UP_NOISEQ   = subset(list_DE_NOISEQ, list_DE_NOISEQ$log2FC > 0)
    list_DOWN_NOISEQ = subset(list_DE_NOISEQ, list_DE_NOISEQ$log2FC < 0)   
  }    # select rest of genes
  
  SaveInCache(noide2.db, list_UP_NOISEQ, "listupnoiseq_key")
  SaveInCache(noide2.db, list_DOWN_NOISEQ, "listdownnoiseq_key")
  
  num_up   = dim(list_UP_NOISEQ)[1]
  num_down = dim(list_DOWN_NOISEQ)[1]
  rest     = dim(list_rest_of_genes_NOISEQ)[1]
  
  slices <- c(num_up, num_down, rest)
  SaveInCache(noide2.db, slices, "slicesnoiseq_key")
  
  lbls <- c(paste(num_up,' UP Genes',sep=''), paste(num_down,' DOWN Genes',sep=''), paste(rest,' NON-DE Genes',sep=''))
  SaveInCache(noide2.db, lbls, "lblsnoiseq_key")
  
  pie3D(slices,labels=lbls,explode=0.1, main='Pie Chart of DE Genes')
  
  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
  outputName=paste(the.file2,"_results_NOISeq.txt", sep="")
  b=paste(a,outputName,sep="\\")
  write.table(mynoiseq@results[[1]], file = b , quote=FALSE, sep="\t", row.names=TRUE)
  outputName=paste(the.file2,"_prob=",p,"_DE_genes_NOISeq.txt", sep="")
  b=paste(a,outputName,sep="\\")
  write.table(list_DE_NOISEQ, file = b , quote=FALSE, sep="\t", row.names=TRUE)
  
  
  outputName=paste(the.file2,"_prob=",p,"_DE_UP_genes_NOISeq.txt", sep="")
  b=file.path(a,outputName)
  write.table(list_UP_NOISEQ, file = b , quote=FALSE, sep="\t", row.names=FALSE)
  
  outputName=paste(the.file2,"_prob=",p,"_DE_UP_genes_NOISeq.tsv", sep="")
  b=file.path(a,outputName)
  write.table(list_UP_NOISEQ, file = b , quote=FALSE, sep="\t", row.names=FALSE)
  
  outputName=paste(the.file2,"_prob=",p,"_DE_DOWN_genes_NOISeq.txt", sep="")
  b=file.path(a,outputName)
  write.table(list_UP_NOISEQ, file = b , quote=FALSE, sep="\t", row.names=FALSE)
  
  outputName=paste(the.file2,"_prob=",p,"_DE_DOWN_genes_NOISeq.tsv", sep="")
  b=file.path(a,outputName)
  write.table(list_UP_NOISEQ, file = b , quote=FALSE, sep="\t", row.names=FALSE)
  
  message=paste("Results of NoiSeq have been saved in the ", Project,"\\Results folder!", sep="")
  print(message)

#write into the project report
 cat('writing noiseq report... ')
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
     message2 <- paste(" * In the *NOISeq Interface*, you clicked the **Run NOISeq** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Results` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
     message2 <- paste("You chose the following count file: `",the.file2,"`, prob: `",p,"`, Project: `",Project,"`, ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
     message1 <- paste("Tissue= c(")
     write(message1, file = report,ncolumns = if(is.character(message1)) 1 else 5,append = TRUE, sep = "")
     message2 <- paste("'",conditions[1:(length(conditions)-1)],"',",sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
     message3 <- paste("'",conditions[length(conditions)],"'",sep="")
     write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "")
     message4 <- paste("), ")
     write(message4, file = report,ncolumns = if(is.character(message4)) 1 else 5,append = TRUE, sep = "")
     message1 <- paste("TissueRun= c(")
     write(message1, file = report,ncolumns = if(is.character(message1)) 1 else 5,append = TRUE, sep = "")
     message2 <- paste("'",TissueRuns[1:(length(TissueRuns)-1)],"',",sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
     message3 <- paste("'",TissueRuns[length(TissueRuns)],"'",sep="")
     write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "")
    message4 <- paste("). ")
    write(message4, file = report,ncolumns = if(is.character(message4)) 1 else 5,append = TRUE, sep = "")
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("require(NOISeq)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("require(plotrix)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("noide2_db <- InitDb(db.name=paste(the.file2,'noide2_db',sep='_'), db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("x <- LoadCachedObject(noide2_db, 'maindataframe_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file <- LoadCachedObject(noide2_db, 'the_file_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("Project <- LoadCachedObject(noide2_db, 'project_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("conditions <- LoadCachedObject(noide2_db, 'conditions_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("TissueRuns <- LoadCachedObject(noide2_db, 'tissueruns_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("technical='",technical,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("p <- LoadCachedObject(noide2_db, 'p_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message3 <- paste("print('You loaded this count file: ')",sep="\n")
    write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "\n")    
    message3 <- paste("print(head(as.matrix(x)))",sep="\n")
    write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "\n")
    message3 <- paste("mynoiseq = NULL",sep="\n")
    write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if (technical == TRUE){ # technical replicate ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("      print('NOISeq has been started on TECHNICAL replicates')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("     #myfactors = data.frame(Tissue = conditions, TissueRun = TissueRuns)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("     #mydata <- NOISeq::readData(data=x, factors = myfactors)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("     #mynoiseq = noiseq(mydata,k=0.5,norm='n',factor='Tissue',pnr = 0.2,nss = 5,",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("     #v = 0.02,lc = 0,replicates=technical)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("      mynoiseq <- LoadCachedObject(noide2_db, 'mynoiseq_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("  }else{ # biological replicate", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("     print('NOISeqBIO has been started on BIOLOGICAL replicates')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("     #myfactors = data.frame(Tissue = conditions, TissueRun = TissueRuns)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("     #mydata <- NOISeq::readData(data=x, factors=myfactors)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("     #mynoiseq = noiseqbio(mydata, k = 0.5, norm = 'n', factor='Tissue', lc = 0, r = 20, adj = 1.5,", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("     #plot = FALSE, a0per = 0.9, random.seed = 12345, filter = 0)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("      mynoiseq <- LoadCachedObject(noide2_db, 'mynoiseq_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("  }", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("  print('First five lines of the results.')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  print( head(mynoiseq@results[[1]]) )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  #list_DE_NOISEQ = subset(mynoiseq@results[[1]], prob > p) # select significant genes",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("  list_DE_NOISEQ <- LoadCachedObject(noide2_db, 'listdenoiseq_key')", sep="")
    
    
    message5 <- paste("  slices <- LoadCachedObject(noide2_db, 'slicesnoiseq_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  lbls <- LoadCachedObject(noide2_db, 'lblsnoiseq_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" pie3D(slices,labels=lbls,explode=0.1, main='Pie Chart of DE Genes')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 cat('done\n')

}else{ #Linux

  the.file2 = strsplit(the.file,"/")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
  noide2.db <- InitDb(db.name=paste(the.file2,'noide2_db',sep="_"), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))  
  SaveInCache(noide2.db, the.file, "the_file_key")
  SaveInCache(noide2.db, Project, "project_key")
  SaveInCache(noide2.db, conditions, "conditions_key")
  SaveInCache(noide2.db, TissueRuns, "tissueruns_key")
  SaveInCache(noide2.db, x, "maindataframe_key")
  p = as.numeric(p)
  SaveInCache(noide2.db, p, "p_key")
  print("You loaded this count file: ")
  print(head(as.matrix(x)))
  print("with this design file: ")
  print("Tissue:")
  print(conditions)
  print("TissueRun:")
  print(TissueRuns)
  mynoiseq = NULL
  if (technical == TRUE){ # technical replicate 

     print("NOISeq has been started on TECHNICAL replicates")
     myfactors = data.frame(Tissue = conditions, TissueRun = TissueRuns)
     mydata <- NOISeq::readData(data=x, factors = myfactors)
     mynoiseq = noiseq(mydata, k = 0.5, norm = "n", factor="Tissue", pnr = 0.2, nss = 5, v = 0.02, lc = 0, replicates = "technical")

  }else{ # biological replicate

     print("NOISeqBIO has been started on BIOLOGICAL replicates")
     myfactors = data.frame(Tissue = conditions, TissueRun = TissueRuns)
     mydata <- NOISeq::readData(data=x, factors=myfactors)
     mynoiseq = noiseqbio(mydata, k = 0.5, norm = "n", factor="Tissue", lc = 0, r = 20, adj = 1.5, plot = FALSE, a0per = 0.9, random.seed = 12345, filter = 0)

  }
  SaveInCache(noide2.db, mynoiseq, "mynoiseq_key")
  print('First five lines of the results.')
  print( head(mynoiseq@results[[1]]) )

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results",sep="")
  the.file2 = strsplit(the.file,"/")
  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

  outputName=paste(the.file2,"_results_NOISeq.tsv", sep="")
  b=paste(a,outputName,sep="/")
  rnames=(row.names(mynoiseq@results[[1]]))
  noiseqres = cbind(rnames,mynoiseq@results[[1]])   #put "rnames" in the header
  write.table(noiseqres, file = b , quote=FALSE, sep="\t", row.names=FALSE)

  outputName=paste(the.file2,"_results_NOISeq.txt", sep="")
  b=paste(a,outputName,sep="/")
  write.table(noiseqres, file = b , quote=FALSE, sep="\t", row.names=FALSE)

  list_DE_NOISEQ = subset(noiseqres, prob > p) # select significant genes
  SaveInCache(noide2.db, list_DE_NOISEQ, "listdenoiseq_key")

  #BUILD PIE CHART
  list_rest_of_genes_NOISEQ = NULL
  if (technical == TRUE){  
       list_rest_of_genes_NOISEQ = subset(mynoiseq@results[[1]], prob <= p | ranking==0)  # select rest of genes 
       list_UP_NOISEQ   = subset(list_DE_NOISEQ, list_DE_NOISEQ$M > 0)
       list_DOWN_NOISEQ = subset(list_DE_NOISEQ, list_DE_NOISEQ$M < 0)    
  }else{  
       list_rest_of_genes_NOISEQ = subset(mynoiseq@results[[1]], prob <= p) 
       list_UP_NOISEQ   = subset(list_DE_NOISEQ, list_DE_NOISEQ$log2FC > 0)
       list_DOWN_NOISEQ = subset(list_DE_NOISEQ, list_DE_NOISEQ$log2FC < 0)   
  }    # select rest of genes
  
  SaveInCache(noide2.db, list_UP_NOISEQ, "listupnoiseq_key")
  SaveInCache(noide2.db, list_DOWN_NOISEQ, "listdownnoiseq_key")
  
  num_up   = dim(list_UP_NOISEQ)[1]
  num_down = dim(list_DOWN_NOISEQ)[1]
  rest     = dim(list_rest_of_genes_NOISEQ)[1]
  
  slices <- c(num_up, num_down, rest)
  SaveInCache(noide2.db, slices, "slicesnoiseq_key")
  
  lbls <- c(paste(num_up,' UP Genes',sep=''), paste(num_down,' DOWN Genes',sep=''), paste(rest,' NON-DE Genes',sep=''))
  SaveInCache(noide2.db, lbls, "lblsnoiseq_key")
  
  pie3D(slices,labels=lbls,explode=0.1, main='Pie Chart of DE Genes')

  outputName=paste(the.file2,"_prob=",p,"_DE_genes_NOISeq.txt", sep="")
  b=paste(a,outputName,sep="/")
  write.table(list_DE_NOISEQ, file = b , quote=FALSE, sep="\t", row.names=FALSE)

  outputName=paste(the.file2,"_prob=",p,"_DE_genes_NOISeq.tsv", sep="")
  b=paste(a,outputName,sep="/")
  write.table(noiseqres, file = b , quote=FALSE, sep="\t", row.names=FALSE) 
  
  outputName=paste(the.file2,"_prob=",p,"_DE_UP_genes_NOISeq.txt", sep="")
  b=file.path(a,outputName)
  write.table(list_UP_NOISEQ, file = b , quote=FALSE, sep="\t", row.names=FALSE)
  
  outputName=paste(the.file2,"_prob=",p,"_DE_UP_genes_NOISeq.tsv", sep="")
  b=file.path(a,outputName)
  write.table(list_UP_NOISEQ, file = b , quote=FALSE, sep="\t", row.names=FALSE)

  outputName=paste(the.file2,"_prob=",p,"_DE_DOWN_genes_NOISeq.txt", sep="")
  b=file.path(a,outputName)
  write.table(list_DOWN_NOISEQ, file = b , quote=FALSE, sep="\t", row.names=FALSE)
  
  outputName=paste(the.file2,"_prob=",p,"_DE_DOWN_genes_NOISeq.tsv", sep="")
  b=file.path(a,outputName)
  write.table(list_DOWN_NOISEQ, file = b , quote=FALSE, sep="\t", row.names=FALSE)
  
  message=paste("Results of NoiSeq have been saved in the ", Project,"/Results folder!", sep="")
  print(message)
 #write into the project report
 cat('writing noiseq report... ')
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
     message2 <- paste(" * In the *NOISeq Interface*, you clicked the **Run NOISeq** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Results` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
     message2 <- paste("You chose the following count file: `",the.file2,"`, prob: `",p,"`, Project: `",Project,"`, ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
     message1 <- paste("Tissue= c(")
     write(message1, file = report,ncolumns = if(is.character(message1)) 1 else 5,append = TRUE, sep = "")
     message2 <- paste("'",conditions[1:(length(conditions)-1)],"',",sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
     message3 <- paste("'",conditions[length(conditions)],"'",sep="")
     write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "")
     message4 <- paste("), ")
     write(message4, file = report,ncolumns = if(is.character(message4)) 1 else 5,append = TRUE, sep = "")
     message1 <- paste("TissueRun= c(")
     write(message1, file = report,ncolumns = if(is.character(message1)) 1 else 5,append = TRUE, sep = "")
     message2 <- paste("'",TissueRuns[1:(length(TissueRuns)-1)],"',",sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
     message3 <- paste("'",TissueRuns[length(TissueRuns)],"'",sep="")
     write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "")
    message4 <- paste("). ")
    write(message4, file = report,ncolumns = if(is.character(message4)) 1 else 5,append = TRUE, sep = "")
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("require(NOISeq)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("require(plotrix)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("noide2_db <- InitDb(db.name=paste(the.file2,'noide2_db',sep='_'), db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("x <- LoadCachedObject(noide2_db, 'maindataframe_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file <- LoadCachedObject(noide2_db, 'the_file_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("Project <- LoadCachedObject(noide2_db, 'project_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("conditions <- LoadCachedObject(noide2_db, 'conditions_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("TissueRuns <- LoadCachedObject(noide2_db, 'tissueruns_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("p <- LoadCachedObject(noide2_db, 'p_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("technical='",technical,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message3 <- paste("print('You loaded this count file: ')",sep="\n")
    write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "\n")    
    message3 <- paste("print(head(as.matrix(x)))",sep="\n")
    write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "\n")
    message3 <- paste("mynoiseq = NULL",sep="\n")
    write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if (technical == TRUE){ # technical replicate ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("      print('NOISeq has been started on TECHNICAL replicates')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("     #myfactors = data.frame(Tissue = conditions, TissueRun = TissueRuns)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("     #mydata <- NOISeq::readData(data=x, factors = myfactors)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("     #mynoiseq = noiseq(mydata,k=0.5,norm='n',factor='Tissue',pnr = 0.2,nss = 5,",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("     #v = 0.02,lc = 0,replicates=technical)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("      mynoiseq <- LoadCachedObject(noide2_db, 'mynoiseq_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("  }else{ # biological replicate", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("     print('NOISeqBIO has been started on BIOLOGICAL replicates')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("     #myfactors = data.frame(Tissue = conditions, TissueRun = TissueRuns)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("     #mydata <- NOISeq::readData(data=x, factors=myfactors)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("     #mynoiseq = noiseqbio(mydata, k = 0.5, norm = 'n', factor='Tissue', lc = 0, r = 20, adj = 1.5,", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("     #plot = FALSE, a0per = 0.9, random.seed = 12345, filter = 0)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("      mynoiseq <- LoadCachedObject(noide2_db, 'mynoiseq_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("  }", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("  print('First five lines of the results.')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  print( head(mynoiseq@results[[1]]) )",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  #list_DE_NOISEQ = subset(mynoiseq@results[[1]], prob > p) # select significant genes",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("  list_DE_NOISEQ <- LoadCachedObject(noide2_db, 'listdenoiseq_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  slices <- LoadCachedObject(noide2_db, 'slicesnoiseq_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  lbls <- LoadCachedObject(noide2_db, 'lblsnoiseq_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" pie3D(slices,labels=lbls,explode=0.1, main='Pie Chart of DE Genes')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 cat('done\n')
 }

  mynoiseq@results[[1]]

}

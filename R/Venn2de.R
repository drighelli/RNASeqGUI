Venn2de <-
function(x, y, the.file1, the.file2, label1, label2, Project){

  require(limma)
  
  # justfilename1<-ExtractFileNameFromPath(the.file1)
  # justfilename2<-ExtractFileNameFromPath(the.file2)
  
  dbfilename <- paste(label1, label2, 'venn2de_db', sep="_")
  
  db <- InitDb(db.name=dbfilename, db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(db, the.file1, "thefile1_key")
  SaveInCache(db, the.file2, "thefile2_key")
  SaveInCache(db, Project, "project_key")
  SaveInCache(db, x, "maindataframe1_key")
  SaveInCache(db, y, "maindataframe2_key")
  SaveInCache(db, label1, "label1_key")
  SaveInCache(db, label2, "label2_key")

  a15 = row.names(x)

  b15 = row.names(y)

  c15 <- intersect(a15,b15)     #common gene names

  ab <- setdiff(a15,b15)

  ba <- setdiff(b15,a15)

  SaveInCache(db, c15, "c15_key")

  Lists <- list(a15, b15)  #put the word vectors into a list to supply lapply  
  Lists <- lapply(Lists, function(x) as.character(unlist(x)))
  items <- sort(unique(unlist(Lists)))   #put in alphabetical order
  MAT <- matrix(rep(0, length(items)*length(Lists)), ncol=2)  #make a matrix of 0's
  names <- c(label1,label2)                
  colnames(MAT) <- names
  rownames(MAT) <- items
  lapply(seq_along(Lists), function(i) {   #fill the matrix
      MAT[items %in% Lists[[i]], i] <<- table(Lists[[i]])
  })
  #MAT   #look at the results
  SaveInCache(db, MAT, "mat_key")

  if(Sys.info()[[1]]=="Windows"){
    a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\",sep="") #eliminate "src" and put "Results"
    outputName=paste(label1,"_",label2,"_genes_in_intersection.txt",sep="")
    b=paste(a,outputName,sep="")
    write.table(c15, file = b , quote=FALSE, sep="\t", row.names=FALSE)
    a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="") #eliminate "src" and put "Plots"
    outputName=paste(label1,"_",label2,"_VennDiagramDE.pdf",sep="")
    b=paste(a,outputName,sep="")
    vennDiagram(MAT,circle.col= c("red","green"),main="Venn Diagram of DE genes")
    dev.print(device = pdf, file=b)
      
    #write into the project report
    report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *Result Comparison Interface*, you clicked the **VennDiagram 2 sets DE** button for  at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("require(limma)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("db <- InitDb(db.name='venn2de_db', db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" x <- LoadCachedObject(db, 'maindataframe1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" y <- LoadCachedObject(db, 'maindataframe2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file1 <- LoadCachedObject(db, 'thefile1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2 <- LoadCachedObject(db, 'thefile2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label1 <- LoadCachedObject(db, 'label1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label2 <- LoadCachedObject(db, 'label2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message2 <- paste("  a15 = row.names(x)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  b15 = row.names(y)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  c15 <- intersect(a15,b15)     #common gene names",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  Lists <- list(a15, b15)  #put the word vectors into a list to supply lapply  ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  Lists <- lapply(Lists, function(x) as.character(unlist(x)))",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  items <- sort(unique(unlist(Lists)))   #put in alphabetical order",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #MAT <- matrix(rep(0, length(items)*length(Lists)), ncol=2)  #make a matrix of 0's",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #names <- c(label1,label2) ",sep="\n")     
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #colnames(MAT) <- names",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #rownames(MAT) <- items",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #lapply(seq_along(Lists), function(i) {   #fill the matrix",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   #   MAT[items %in% Lists[[i]], i] <<- table(Lists[[i]]) })",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("MAT <- LoadCachedObject(db, 'mat_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message2 <- paste("   a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Results\\',sep='')",sep="\n") 
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   outputName=paste(label1,'_',label2,'_genes_in_intersection.txt',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   b=paste(a,outputName,sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   #write.table(c15, file = b , quote=FALSE, sep='\t', row.names=FALSE)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Plots\\',sep='') ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   outputName=paste(label1,'_',label2,'_VennDiagramDE.pdf',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   b=paste(a,outputName,sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   vennDiagram(MAT,circle.col= c('red','green'),main='Venn Diagram of DE genes')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   #dev.print(device = pdf, file=b)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  }else{ #Linux
    a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",sep="") #eliminate "src" and put "Results"
  
    outputName=paste(label1,"_",label2,"_genes_in_intersection.txt",sep="")

    outputName2=paste("genes_in_",label1,"_not_in_",label2,".txt",sep="")

    outputName3=paste("genes_in_",label2,"_not_in_",label1,".txt",sep="")
  
    b=paste(a,outputName,sep="")

    b2=paste(a,outputName2,sep="")

    b3=paste(a,outputName3,sep="")
  
    write.table(c15, file = b , quote=FALSE, sep="\t", row.names=FALSE)

    write.table(ab, file = b2 , quote=FALSE, sep="\t", row.names=FALSE)

    write.table(ba, file = b3 , quote=FALSE, sep="\t", row.names=FALSE)
  
    a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="") #eliminate "src" and put "Plots"
  
    outputName=paste(label1,"_",label2,"_VennDiagramDE.pdf",sep="")
  
    b=paste(a,outputName,sep="")
  
    vennDiagram(MAT,circle.col= c("red","green"),main="Venn Diagram of DE genes")
  
    dev.print(device = pdf, file=b)

    #write into the project report
    report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *Result Comparison Interface*, you clicked the **VennDiagram 2 sets DE** button for  at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    
    message5 <- paste("require(limma)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     
    #     message5 <- paste("'",the.file1,"',header=TRUE,row.names=1)", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     
    #     message5 <- paste("y = read.table(", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     
    #     message5 <- paste("'",the.file2,"',header=TRUE,row.names=1)", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     
    #     message5 <- paste("label1='",label1,"'", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     
    #     message5 <- paste("label2='",label2,"'", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     
    #     message5 <- paste("Project='",Project,"'", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("db <- InitDb(db.name='venn2de_db', db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" x <- LoadCachedObject(db, 'maindataframe1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" y <- LoadCachedObject(db, 'maindataframe2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file1 <- LoadCachedObject(db, 'thefile1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2 <- LoadCachedObject(db, 'thefile2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label1 <- LoadCachedObject(db, 'label1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label2 <- LoadCachedObject(db, 'label2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  a15 = row.names(x)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  b15 = row.names(y)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  c15 <- intersect(a15,b15)     #common gene names",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  Lists <- list(a15, b15)  #put the word vectors into a list to supply lapply  ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  Lists <- lapply(Lists, function(x) as.character(unlist(x)))",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  items <- sort(unique(unlist(Lists)))   #put in alphabetical order",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #MAT <- matrix(rep(0, length(items)*length(Lists)), ncol=2)  #make a matrix of 0's",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #names <- c(label1,label2) ",sep="\n")     
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #colnames(MAT) <- names",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #rownames(MAT) <- items",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #lapply(seq_along(Lists), function(i) {   #fill the matrix",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   #   MAT[items %in% Lists[[i]], i] <<- table(Lists[[i]]) })",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("MAT <- LoadCachedObject(db, 'mat_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Results/',sep='')",sep="\n") 
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   outputName=paste(label1,'_',label2,'_genes_in_intersection.txt',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   b=paste(a,outputName,sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   #write.table(c15, file = b , quote=FALSE, sep='\t', row.names=FALSE)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Plots/',sep='') ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   outputName=paste(label1,'_',label2,'_VennDiagramDE.pdf',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   b=paste(a,outputName,sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   vennDiagram(MAT,circle.col= c('red','green'),main='Venn Diagram of DE genes')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   #dev.print(device = pdf, file=b)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    

  }

}

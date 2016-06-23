Venn3de <-
function(x, y, z, the.file1, the.file2, the.file3, label1, label2, label3, Project){

  require(limma)

  
  dbfilename <- paste(label1, label2, label3, 'venn3de_db', sep="_")
  
  db <- InitDb(db.name=dbfilename, db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(db, the.file1, "thefile1_key")
  SaveInCache(db, the.file2, "thefile2_key")
  SaveInCache(db, the.file3, "thefile3_key")
  SaveInCache(db, Project, "project_key")
  SaveInCache(db, x, "maindataframe1_key")
  SaveInCache(db, y, "maindataframe2_key")
  SaveInCache(db, z, "maindataframe3_key")
  SaveInCache(db, label1, "label1_key")
  SaveInCache(db, label2, "label2_key")
  SaveInCache(db, label3, "label3_key")

  a = row.names(x)
  b = row.names(y)
  c = row.names(z)
  #d <- intersect(a,b)     #common gene names
  #SaveInCache(db, d, "d_key")
  #e <- intersect(d,c)     #common gene names
  #SaveInCache(db, e, "e_key")
  
  
  res.path <- file.path(substring(getwd(),1,nchar(getwd())), "RNASeqGUI_Projects", Project, "Results")

  ab <- intersect(a, b)
  SaveInCache(db, ab, "ab_key")

  filename <- paste(label1,"_",label2,"_genes_in_intersection.txt",sep="")
  filepathname <- file.path(res.path, filename)
  write.table(ab, file = filepathname , quote=FALSE, sep="\t", row.names=FALSE)
  
  bc <- intersect(b, c)
  SaveInCache(db, bc, "bc_key")
  filename <- paste(label2,"_",label3,"_genes_in_intersection.txt",sep="")
  filepathname <- file.path(res.path, filename)
  write.table(bc, file = filepathname , quote=FALSE, sep="\t", row.names=FALSE)
  
  ac <- intersect(a, c)
  SaveInCache(db, ac, "ac_key")
  filename <- paste(label1,"_",label3,"_genes_in_intersection.txt",sep="")
  filepathname <- file.path(res.path, filename)
  write.table(ac, file = filepathname , quote=FALSE, sep="\t", row.names=FALSE)
  
  abc <- intersect(ab, bc)
  SaveInCache(db, abc, "abc_key")
  filename <- paste(label1, "_", label2, "_", label3, "_genes_in_intersection.txt",sep="")
  filepathname <- file.path(res.path, filename)
  write.table(abc, file = filepathname , quote=FALSE, sep="\t", row.names=FALSE)
  
  a.not.b <-  setdiff(a, b)
  SaveInCache(db, a.not.b, "anotb_key")
  filename <- paste(label1, "_NOT_", label2, "_genes.txt",sep="")
  filepathname <- file.path(res.path, filename)
  write.table(a.not.b, file = filepathname , quote=FALSE, sep="\t", row.names=FALSE)
  
  b.not.a <-  setdiff(b, a)
  SaveInCache(db, b.not.a, "bnota_key")
  filename <- paste(label2, "_NOT_", label1, "_genes.txt",sep="")
  filepathname <- file.path(res.path, filename)
  write.table(b.not.a, file = filepathname , quote=FALSE, sep="\t", row.names=FALSE)
  
  c.not.b <-  setdiff(c, b)
  SaveInCache(db, c.not.b, "cnotb_key")
  filename <- paste(label3, "_NOT_", label2, "_genes.txt",sep="")
  filepathname <- file.path(res.path, filename)
  write.table(c.not.b, file = filepathname , quote=FALSE, sep="\t", row.names=FALSE)
  
  b.not.c <-  setdiff(b, c)
  SaveInCache(db, b.not.c, "bnotc_key")
  filename <- paste(label2, "_NOT_", label3, "_genes.txt",sep="")
  filepathname <- file.path(res.path, filename)
  write.table(b.not.c, file = filepathname , quote=FALSE, sep="\t", row.names=FALSE)
  
  a.not.c <-  setdiff(a, c)
  SaveInCache(db, a.not.c, "anotc_key")
  filename <- paste(label1, "_NOT_", label3, "_genes.txt",sep="")
  filepathname <- file.path(res.path, filename)
  write.table(a.not.c, file = filepathname , quote=FALSE, sep="\t", row.names=FALSE)
  
  c.not.a <-  setdiff(c, a)
  SaveInCache(db, c.not.a, "cnota_key")
  filename <- paste(label3, "_NOT_", label1, "_genes.txt",sep="")
  filepathname <- file.path(res.path, filename)
  write.table(c.not.a, file = filepathname , quote=FALSE, sep="\t", row.names=FALSE)
  
  a.not.bc <- setdiff(a.not.b, c)
  SaveInCache(db, a.not.bc, "anotbc_key")
  filename <- paste(label1, "_NOT_", label2, "_NOT_", label3, "_genes.txt",sep="")
  filepathname <- file.path(res.path, filename)
  write.table(a.not.bc, file = filepathname , quote=FALSE, sep="\t", row.names=FALSE)
  
  b.not.ac <- setdiff(b.not.a, c)
  SaveInCache(db, b.not.ac, "bnotac_key")
  filename <- paste(label2, "_NOT_", label1, "_NOT_", label3, "_genes.txt",sep="")
  filepathname <- file.path(res.path, filename)
  write.table(b.not.ac, file = filepathname , quote=FALSE, sep="\t", row.names=FALSE)
  
  c.not.ab <- setdiff(c.not.a, b)
  SaveInCache(db, c.not.ab, "cnotab_key")
  filename <- paste(label3, "_NOT_", label1, "_NOT_", label2, "_genes.txt",sep="")
  filepathname <- file.path(res.path, filename)
  write.table(c.not.ab, file = filepathname , quote=FALSE, sep="\t", row.names=FALSE)
  

  Lists <- list(a, b, c)  #put the word vectors into a list to supply lapply  
  Lists <- lapply(Lists, function(x) as.character(unlist(x)))
  items <- sort(unique(unlist(Lists)))   #put in alphabetical order
  MAT <- matrix(rep(0, length(items)*length(Lists)), ncol=3)  #make a matrix of 0's
  names <- c(label1,label2,label3)                
  colnames(MAT) <- names
  rownames(MAT) <- items
  lapply(seq_along(Lists), function(i) {   #fill the matrix
      MAT[items %in% Lists[[i]], i] <<- table(Lists[[i]])
  })
  SaveInCache(db, MAT, "mat_key")

  if(Sys.info()[[1]]=="Windows"){
    a=paste(substring(getwd(),1,nchar(getwd())),"\\RNASeqGUI_Projects\\",Project,"\\Results\\",sep="") #eliminate "src" and put "Results"
    #outputName=paste(label1,"_",label2,"_",label3,"_genes_in_intersection.txt",sep="")
    
    #b=paste(a,outputName,sep="")
    
    #write.table(e, file = b , quote=FALSE, sep="\t", row.names=FALSE)
    
    
    a=paste(substring(getwd(),1,nchar(getwd())),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="") #eliminate "src" and put "Plots"
    
    outputName=paste(label1,"_",label2,"_",label3,"_VennDiagramDE.pdf",sep="")
    
    b=paste(a,outputName,sep="")
    
    vennDiagram(MAT, circle.col= c("red","green","yellow"), main="Venn Diagram of DE genes")
    
    dev.print(device = pdf, file=b)
    
    #write into the project report
    report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *Result Comparison Interface*, you clicked the **VennDiagram 3 sets DE** button for  at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("require(limma)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("dbfilename <- '",dbfilename,"'", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("db <- InitDb(db.name=dbfilename, db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("x <- LoadCachedObject(db, 'maindataframe1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("y <- LoadCachedObject(db, 'maindataframe2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("z <- LoadCachedObject(db, 'maindataframe3_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file1 <- LoadCachedObject(db, 'thefile1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2 <- LoadCachedObject(db, 'thefile2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file3 <- LoadCachedObject(db, 'thefile3_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label1 <- LoadCachedObject(db, 'label1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label2 <- LoadCachedObject(db, 'label2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label3 <- LoadCachedObject(db, 'label3_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     message5 <- paste("x = read.table(", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
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
#     message5 <- paste("z = read.table(", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("'",the.file3,"',header=TRUE,row.names=1)", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("label1='",label1,"'", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("label2='",label2,"'", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("label3='",label3,"'", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("Project='",Project,"'", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  a = row.names(x)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  b = row.names(y)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  c = row.names(z)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  d <- intersect(a,b)     #common gene names",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  e <- intersect(d,c)     #common gene names",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  Lists <- list(a, b, c)  #put the word vectors into a list to supply lapply  ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  Lists <- lapply(Lists, function(x) as.character(unlist(x)))",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  items <- sort(unique(unlist(Lists)))   #put in alphabetical order",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #MAT <- matrix(rep(0, length(items)*length(Lists)), ncol=3)  #make a matrix of 0's",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #names <- c(label1,label2,label3)   ",sep="\n")     
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #colnames(MAT) <- names",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #rownames(MAT) <- items",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #lapply(seq_along(Lists), function(i) {   #fill the matrix",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #MAT[items %in% Lists[[i]], i] <<- table(Lists[[i]]) })",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("MAT <- LoadCachedObject(db, 'mat_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    
    message2 <- paste("  a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Results\\',sep='')",sep="\n") 
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   outputName=paste(label1,'_',label2,'_',label3,'_genes_in_intersection.txt',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   b=paste(a,outputName,sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   #write.table(e, file = b , quote=FALSE, sep='\t', row.names=FALSE)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Plots\\',sep='') ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   outputName=paste(label1,'_',label2,'_',label3,'_VennDiagramDE.pdf',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   b=paste(a,outputName,sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   vennDiagram(MAT,circle.col= c('red','green','yellow'),main='Venn Diagram of DE genes')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   #dev.print(device = pdf, file=b)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    

  }else{# Linux

    a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",sep="") #eliminate "src" and put "Results"
    
    outputName=paste(label1,"_",label2,"_",label3,"_genes_in_intersection.txt",sep="")
    
    b=paste(a,outputName,sep="")
    
    write.table(e, file = b , quote=FALSE, sep="\t", row.names=FALSE)
    
    a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="") #eliminate "src" and put "Plots"
    
    outputName=paste(label1,"_",label2,"_",label3,"_VennDiagramDE.pdf",sep="")
    
    b=paste(a,outputName,sep="")
    
    vennDiagram(MAT, circle.col= c("red","green","yellow"), main="Venn Diagram of DE genes")
    
    dev.print(device = pdf, file=b)
    
    #write into the project report
    report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *Result Comparison Interface*, you clicked the **VennDiagram 3 sets DE** button for  at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("require(limma)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("dbfilename <- '",dbfilename,"'", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("db <- InitDb(db.name=dbfilename, db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    ##message5 <- paste("db <- InitDb(db.name='venn3de_db', db.path='cache')", sep="")
    ##write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("x <- LoadCachedObject(db, 'maindataframe1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("y <- LoadCachedObject(db, 'maindataframe2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("z <- LoadCachedObject(db, 'maindataframe3_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file1 <- LoadCachedObject(db, 'thefile1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2 <- LoadCachedObject(db, 'thefile2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file3 <- LoadCachedObject(db, 'thefile3_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label1 <- LoadCachedObject(db, 'label1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label2 <- LoadCachedObject(db, 'label2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label3 <- LoadCachedObject(db, 'label3_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     message5 <- paste("x = read.table(", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
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
    #     message5 <- paste("z = read.table(", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     
    #     message5 <- paste("'",the.file3,"',header=TRUE,row.names=1)", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     
    #     message5 <- paste("label1='",label1,"'", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     
    #     message5 <- paste("label2='",label2,"'", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     
    #     message5 <- paste("label3='",label3,"'", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     
    #     message5 <- paste("Project='",Project,"'", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  a = row.names(x)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  b = row.names(y)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  c = row.names(z)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  d <- intersect(a,b)     #common gene names",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  e <- intersect(d,c)     #common gene names",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  Lists <- list(a, b, c)  #put the word vectors into a list to supply lapply  ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  Lists <- lapply(Lists, function(x) as.character(unlist(x)))",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  items <- sort(unique(unlist(Lists)))   #put in alphabetical order",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #MAT <- matrix(rep(0, length(items)*length(Lists)), ncol=3)  #make a matrix of 0's",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #names <- c(label1,label2,label3)   ",sep="\n")     
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
    
    message2 <- paste("   outputName=paste(label1,'_',label2,'_',label3,'_genes_in_intersection.txt',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   b=paste(a,outputName,sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   #write.table(e, file = b , quote=FALSE, sep='\t', row.names=FALSE)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Plots/',sep='') ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   outputName=paste(label1,'_',label2,'_',label3,'_VennDiagramDE.pdf',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   b=paste(a,outputName,sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   vennDiagram(MAT,circle.col= c('red','green','yellow'),main='Venn Diagram of DE genes')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   #dev.print(device = pdf, file=b)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  }
}

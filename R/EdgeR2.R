EdgeR2 <- function(x,n,factors,the.file,fdr,Project){

  require(edgeR)
  
  if(Sys.info()[[1]]=="Windows"){
    a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
    the.file2 = strsplit(the.file,"\\\\")
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
    sys.sep='\\'
  }else{
    a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
    the.file2 = strsplit(the.file,"/")
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
    sys.sep='/'
  }

  fdr = as.numeric(fdr)

  g <- factor(factors)
  
  edger2.db <- InitDb(db.name=paste(the.file2,'edger2.db',sep="_"), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(edger2.db, the.file, "the_file_key")
  SaveInCache(edger2.db, Project, "project_key")
  SaveInCache(edger2.db, x, "maindataframe_key")
  SaveInCache(edger2.db, n, "n_key")
  SaveInCache(edger2.db, factors, "factors_key")
  SaveInCache(edger2.db, g, "g_key")
  SaveInCache(edger2.db, fdr, "fdr_key")

  res9 <- DGEList(counts = x[,1:n], group=g)  # modify here the entries of the count columns

  SaveInCache(edger2.db, res9, "res9_key")
  
  print("You loaded this count file: ")
  print(head(res9$counts))

  colnames(res9) <- factor(c(1:n))

  res9 <- calcNormFactors(res9,method='none')
  SaveInCache(edger2.db, res9, "res9fact_key")
  
  print("Summary of the design: ")
  print(res9$samples)

  res9 <- estimateCommonDisp(res9, verbose=TRUE)
  SaveInCache(edger2.db, res9, "res9comdisp_key")
  res9 <- estimateTagwiseDisp(res9)
  SaveInCache(edger2.db, res9, "res9tagdisp_key")

  print("EdgeR did NOT use any normalization procedure!")
  print("EdgeR just performed a library size transformation of the count file according to the lib.size list. Counts transformed:")
  print(head(res9$pseudo.counts))
  print("Factors from the design file provided:")
  print(factors)

  
  if(Sys.info()[[1]]=="Windows"){
    a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
    the.file2 = strsplit(the.file,"\\\\")
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
    outputName=paste(the.file2,"_DispersionEdgeR.pdf", sep="")
    b=paste(a,outputName,sep="")
  }else{
    a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
    the.file2 = strsplit(the.file,"/")
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile
    outputName=paste(the.file2,"_DispersionEdgeR.pdf", sep="")
    b=paste(a,outputName,sep="")
  }

  plotBCV(res9, main="BCV Plot")

  dev.print(device = pdf, file=b)

  dev.off()

  outputName=paste(the.file2,"_MDS_EdgeR.pdf", sep="")

  #b=paste(a,outputName,sep="\\")
  b=paste(a,outputName,sep=sys.sep)
  #plotMDS(res9, main="MDS Plot")
  plotMDS(res9, labels=res9$samples$group,col=c("red","blue")[factor(g)], main="MDS Plot")

  dev.print(device = pdf, file=b)

  et9 <- exactTest(res9)
  SaveInCache(edger2.db, et9, "et9_key")
  #res[,c(1,2,7,8)]

  et9_results <- topTags( et9 , n = nrow( et9$table ) , sort.by = "none" )$table # n = nrow( et9$table ) is used to print all results!
  SaveInCache(edger2.db, et9_results, "et9results_key")
  
  #a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\",sep="")
  a=paste(getwd(),"RNASeqGUI_Projects",Project,"Results",sep=sys.sep)

  outputName=paste(the.file2,"_results_EdgeR.txt", sep="")
  b=paste(a,outputName,sep=sys.sep)
  et9_results$id <- rownames(et9_results)
  et9_results<-(et9_results[,c(5,1,2,3,4)])
  write.table( et9_results, file = b , quote=FALSE, sep="\t", row.names=FALSE)

  outputName=paste(the.file2,"_results_EdgeR.tsv", sep="")
  b=paste(a,outputName,sep=sys.sep)
  write.table( et9_results, file = b , quote=FALSE, sep="\t", row.names=FALSE)

  #BUILD PIE CHART
  list_DE_EDGER = subset(et9_results, FDR<fdr) # select significant genes
  SaveInCache(edger2.db, list_DE_EDGER, "listdeedger_key")

  list_rest_of_genes_EDGER = subset(et9_results, (FDR >= fdr) | (logFC == 0) ) # select rest of genes
  list_UP_EDGER = subset(list_DE_EDGER, list_DE_EDGER$logFC > 0)
  list_DOWN_EDGER = subset(list_DE_EDGER, list_DE_EDGER$logFC < 0)
  num_up   = dim(list_UP_EDGER)[1]
  num_down = dim(list_DOWN_EDGER)[1]
  rest     = dim(list_rest_of_genes_EDGER)[1]
  slices <- c(num_up, num_down, rest)
  lbls <- c(paste(num_up,' UP Genes',sep=''), paste(num_down,' DOWN Genes',sep=''), paste(rest,' remaining Genes',sep=''))
  pie3D(slices,labels=lbls,explode=0.1, main='Pie Chart of DE Genes')
  
  #de <- decideTestsDGE(et9) #genes significantly up-regulated or down-regulated at 5% FDR
  #isDE <- as.logical(de)
  #DEnames <- rownames(res9)[isDE]

  outputName=paste(the.file2,"_fdr=",fdr,"_DE_genes_EDGER.txt", sep="")
  b=paste(a,outputName,sep=sys.sep)
  write.table(list_DE_EDGER, file = b , quote=FALSE, sep="\t", row.names=FALSE)

  outputName=paste(the.file2,"_fdr=",fdr,"_DE_genes_EDGER.tsv", sep="")
  b=paste(a,outputName,sep=sys.sep)
  write.table(list_DE_EDGER, file = b , quote=FALSE, sep="\t", row.names=FALSE)

     message=paste("Results of EdgeR have been saved in the Results folder of ",Project," project!", sep="")
     print(message)

  if(Sys.info()[[1]]=="Windows") {
    #write into the project report
    report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *EdgeR Interface*, you clicked the **Run EdgeR** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Results` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
   message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message2 <- paste("You chose the following count file: `",the.file,"`, fdr: `",fdr,"`, Project: `",Project,"`, ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")    
    message1 <- paste("factors= c(")
    write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")   
    message2 <- paste("'",factors[1:(length(factors)-1)],"',",sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message3 <- paste("'",factors[length(factors)],"'",sep="")
    write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")  
    message4 <- paste("). ")
    write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")   
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("require(edgeR)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("edger2.db <- InitDb(db.name=paste(the.file2,'edger2.db',sep='_'), db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n") 
message5 <- paste("x <- LoadCachedObject(edger2.db, 'maindataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(edger2.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("Project <- LoadCachedObject(edger2.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("n <- LoadCachedObject(edger2.db, 'n_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("factors <- LoadCachedObject(edger2.db, 'factors_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("g <- LoadCachedObject(edger2.db, 'g_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("fdr <- LoadCachedObject(edger2.db, 'fdr_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#res9 <- DGEList(counts = x[,1:n], group=g)  # modify here the entries of the count columns", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("res9 <- LoadCachedObject(edger2.db, 'res9_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("print('You loaded this count file: ') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("print(head(res9$counts)) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("colnames(res9) <- factor(c(1:n))", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#res9 <- calcNormFactors(res9,method='none')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("res9 <- LoadCachedObject(edger2.db, 'res9fact_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#res9 <- estimateCommonDisp(res9, verbose=TRUE)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("res9 <- LoadCachedObject(edger2.db, 'res9comdisp_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#res9 <- estimateTagwiseDisp(res9)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("res9 <- LoadCachedObject(edger2.db, 'res9tagdisp_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")  
    message5 <- paste("plotBCV(res9, main='BCV Plot')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("plotMDS(res9, labels=res9$samples$group,col=c('red','blue')[factor(g)], main='MDS Plot')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#et9 <- exactTest(res9)  ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("et9 <- LoadCachedObject(edger2.db, 'et9_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#et9_results <- topTags( et9 , n = nrow( et9$table ) , sort.by = 'none' )$table # n = nrow( et9$table ) is used to print all results!", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("et9_results <- LoadCachedObject(edger2.db, 'et9results_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("print('First five lines of the results.') ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("print( head(et9_results) ) ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#list_DE_EDGER = subset(et9_results, FDR<fdr) # select significant genes", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("list_DE_EDGER <- LoadCachedObject(edger2.db, 'listdeedger_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
  
    print(" FINISHED ! ")
    
    et9_results

  }else{

 #write into the project report
    report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message2 <- paste(" * In the *EdgeR Interface*, you clicked the **Run EdgeR** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Results` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message2 <- paste("You chose the following count file: `",the.file,"`, fdr: `",fdr,"`, Project: `",Project,"`, ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")    
    message1 <- paste("factors= c(")
    write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")   
    message2 <- paste("'",factors[1:(length(factors)-1)],"',",sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")    
    message3 <- paste("'",factors[length(factors)],"'",sep="")
    write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")  
    message4 <- paste("). ")
    write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")   
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("require(edgeR)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("edger2.db <- InitDb(db.name=paste(the.file2,'edger2.db',sep='_'), db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n") 
message5 <- paste("x <- LoadCachedObject(edger2.db, 'maindataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(edger2.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("Project <- LoadCachedObject(edger2.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("n <- LoadCachedObject(edger2.db, 'n_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("factors <- LoadCachedObject(edger2.db, 'factors_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("g <- LoadCachedObject(edger2.db, 'g_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("fdr <- LoadCachedObject(edger2.db, 'fdr_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#res9 <- DGEList(counts = x[,1:n], group=g)  # modify here the entries of the count columns", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("res9 <- LoadCachedObject(edger2.db, 'res9_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("print('You loaded this count file: ') ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("print(head(res9$counts)) ", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("colnames(res9) <- factor(c(1:n))", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#res9 <- calcNormFactors(res9,method='none')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("res9 <- LoadCachedObject(edger2.db, 'res9fact_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#res9 <- estimateCommonDisp(res9, verbose=TRUE)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("res9 <- LoadCachedObject(edger2.db, 'res9comdisp_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("#res9 <- estimateTagwiseDisp(res9)", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("res9 <- LoadCachedObject(edger2.db, 'res9tagdisp_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")  
    message5 <- paste("plotBCV(res9, main='BCV Plot')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("plotMDS(res9, labels=res9$samples$group,col=c('red','blue')[factor(g)], main='MDS Plot')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#et9 <- exactTest(res9)  ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("et9 <- LoadCachedObject(edger2.db, 'et9_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#et9_results <- topTags( et9 , n = nrow( et9$table ) , sort.by = 'none' )$table # n = nrow( et9$table ) is used to print all results!", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("et9_results <- LoadCachedObject(edger2.db, 'et9results_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("print('First five lines of the results.') ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("print( head(et9_results) ) ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    print(" FINISHED ! ")
    
    et9_results
  }

}

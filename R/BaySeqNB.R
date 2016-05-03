BaySeqNB <-
function(x, conditions, list_nde, list_de, estType, samsize, sA, sB, the.file, fdr, Project){

require(baySeq)

  bayseqnb.db <- InitDb(db.name="BaySeqNB_db", db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  SaveInCache(bayseqnb.db, x, "bayseqnb_dataframe_key")
  SaveInCache(bayseqnb.db, conditions, "conditions_key")
  SaveInCache(bayseqnb.db, list_nde, "list_nde_key")
  SaveInCache(bayseqnb.db, list_de, "list_de_key")
  SaveInCache(bayseqnb.db, estType, "estType_key")
  SaveInCache(bayseqnb.db, samsize, "samsize_key")
  SaveInCache(bayseqnb.db, sA, "sA_key")
  SaveInCache(bayseqnb.db, sB, "sB_key")
  SaveInCache(bayseqnb.db, the.file, "the_file_key")
  SaveInCache(bayseqnb.db, fdr, "fdr_key")
  SaveInCache(bayseqnb.db, Project, "project_key")

  set.seed(102)
  options(width = 90)
  samsize = as.numeric(samsize)
  fdr = as.numeric(fdr)

  counts = data.matrix(x, rownames.force = NA)

  print("Count file loaded: ")
  print(head(counts))

  NDE = as.numeric(list_nde)

  DE = as.numeric(list_de)

  groups <- list(NDE = NDE, DE = DE)

  CD <- new("countData", data = counts, replicates =  conditions, groups = groups)

  libsizes(CD) <- getLibsizes(CD, estimationType = estType)
  
  SaveInCache(bayseqnb.db, CD, "CD_key")

  CD <- getPriors.NB(CD, samplesize = samsize, estimation = "QL", cl = NULL)
  
  SaveInCache(bayseqnb.db, CD, "CD_priors_key")

  CD <- getLikelihoods.NB(CD, pET = 'BIC', cl = NULL)

  SaveInCache(bayseqnb.db, CD, "CD_likelihoods_key")

  plotMA.CD(CD, samplesA = sA, samplesB = sB, col = c(rep("red", 100), rep("black", 900)))

 if(Sys.info()[[1]]=="Windows"){

  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")

  the.file2 = strsplit(the.file,"\\\\")

  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

  outputName=paste(the.file2,"_PlotMA_BaySeqNB.pdf", sep="")

  b=paste(a,outputName,sep="\\")

  dev.print(device = pdf, file=b)

  dev.off()

  CD@estProps

  bayseq_table = topCounts(CD, group="DE", decreasing = TRUE, number = nrow( CD@data )) # PRINT THE ENTIRE TABLE of results
  
  SaveInCache(bayseqnb.db, bayseq_table, "bayseq_table_key")

  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\",sep="")

  outputName=paste(the.file2,"_results_BaySeqNB.txt", sep="")

  b=paste(a,outputName,sep="\\")

  write.table(bayseq_table, file = b , quote=FALSE, sep="\t", row.names=TRUE)

  plotPosteriors(CD, group = "DE", col = c(rep("red", 100), rep("black", 900)))

  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")

  outputName=paste(the.file2,"_Posteriors_BaySeqNB.pdf", sep="")

  b=paste(a,outputName,sep="\\")

  dev.print(device = pdf, file=b)

  # Now print the table of results with FDR < 0.05

  bayseq_table_FDR = subset(bayseq_table, FDR.DE < fdr)

  a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\",sep="")

  outputName=paste(the.file2,"_fdr=",fdr,"_DE_genes_BaySeqNB.txt", sep="")

  b=paste(a,outputName,sep="\\")

  write.table(bayseq_table_FDR, file = b , quote=FALSE, sep="\t", row.names=TRUE)

     message=paste("Results of BaySeqNB have been saved in the ", Project,"\\Results folder!", sep="")
     print(message)

#write into the project report
  report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *BaySeq Interface*, you clicked the **Run BaySeqNB** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Results` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 message2 <- paste("You chose the following count file: `",the.file2,"`, FDR: `",fdr,"`, Project: `",Project,"`, ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("   Estimation Type :`", estType,"`", sep="")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("   Sample Size =`", samsize,"`", sep="")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("   Sample A =`", sA,"`", sep="")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("   Sample B =`", sB,"`", sep="")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("   FDR =`", fdr,"`", sep="")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message1 <- paste("Factors= `c(")
     write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message2 <- paste("'",conditions[1:(length(conditions)-1)],"',",sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message3 <- paste("'",conditions[length(conditions)],"'",sep="")
     write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message4 <- paste(")`, ")
     write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message1 <- paste("NDE= `c(")
     write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message2 <- paste("'",list_nde[1:(length(list_nde)-1)],"',",sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message3 <- paste("'",list_nde[length(list_nde)],"'",sep="")
     write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message4 <- paste(")`, ")
     write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message1 <- paste("DE= `c(")
     write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message2 <- paste("'",list_de[1:(length(list_de)-1)],"',",sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message3 <- paste("'",list_de[length(list_de)],"'",sep="")
     write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message4 <- paste(")`. ")
     write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("bayseqnb.db <- InitDb(db.name='BaySeqNB_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" x <- LoadCachedObject(bayseqnb.db, 'bayseqnb_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" Project <- LoadCachedObject(bayseqnb.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(bayseqnb.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("conditions <- LoadCachedObject(bayseqnb.db, 'conditions_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("list_nde <- LoadCachedObject(bayseqnb.db, 'list_nde_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("list_de <- LoadCachedObject(bayseqnb.db, 'list_de_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("estType <- LoadCachedObject(bayseqnb.db, 'estType_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("samsize <- LoadCachedObject(bayseqnb.db, 'samsize_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("sA <- LoadCachedObject(bayseqnb.db, 'sA_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("sB <- LoadCachedObject(bayseqnb.db, 'sB_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("fdr <- LoadCachedObject(bayseqnb.db, 'fdr_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


# message5 <- paste("x = read.table('",the.file,"',header=TRUE,row.names=1)", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("the.file ='",the.file,"'", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("Project ='", Project,"'", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
#      message5 <- paste("estType ='", estType,"'", sep="")
#      write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
#      message4 <- paste("fdr ='",fdr,"'",sep="")
#      write(message4, file = report,ncolumns = if(is.character(message4)) 1 else 5,append = TRUE, sep = "")
# 
#      message5 <- paste("samsize ='", samsize,"'", sep="")
#      write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
#      message5 <- paste("sA ='", sA,"'", sep="")
#      write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
#      message5 <- paste("sB ='", sB,"'", sep="")
#      write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
#      message1 <- paste("conditions= c(")
#      write(message1, file = report,ncolumns = if(is.character(message1)) 1 else 5,append = TRUE, sep = "")
# 
#      message2 <- paste("'",conditions[1:(length(conditions)-1)],"',",sep="")
#      write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message3 <- paste("'",conditions[length(conditions)],"'",sep="")
#      write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "")
# 
#      message4 <- paste(") ")
#      write(message4, file = report,ncolumns = if(is.character(message4)) 1 else 5,append = TRUE, sep = "")

#      message1 <- paste("NDE= c(")
#      write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message2 <- paste("'",list_nde[1:(length(list_nde)-1)],"',",sep="")
#      write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message3 <- paste("'",list_nde[length(list_nde)],"'",sep="")
#      write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message4 <- paste(") ")
#      write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message1 <- paste("DE= c(")
#      write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message2 <- paste("'",list_de[1:(length(list_de)-1)],"',",sep="")
#      write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message3 <- paste("'",list_de[length(list_de)],"'",sep="")
#      write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message4 <- paste(") ")
#      write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("set.seed(102) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("options(width = 90 )",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("samsize = as.numeric(samsize) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("fdr = as.numeric(fdr) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("counts = data.matrix(x, rownames.force = NA) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print('Count file loaded: ') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(head(counts)) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("CD <- new('countData', data = counts)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("libsizes(CD) <- getLibsizes(CD, estimationType = estType)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("replicates(CD) = conditions",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("NDE = as.numeric(list_nde)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("DE = as.numeric(list_de)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("groups(CD) <- list(NDE = NDE, DE = DE)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#############
message5 <- paste("#CD <- getPriors.NB(CD, samplesize = samsize, estimation = 'QL', cl = NULL)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("CD <- LoadCachedObject(bayseqnb.db, 'CD_priors_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#CD <- getLikelihoods.NB(CD, pET = 'BIC', cl = NULL) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("CD <- LoadCachedObject(bayseqnb.db, 'CD_likelihoods_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
############
message5 <- paste("plotMA.CD(CD, samplesA = sA, samplesB = sB, col = c(rep('red', 100), rep('black', 900))) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Plots\\',sep='') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = strsplit(the.file,'\\') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("outputName=paste(the.file2,'_PlotMA_BaySeqNB.pdf', sep='') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("b=paste(a,outputName,sep='\\') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#dev.print(device = pdf, file=b) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("CD@estProps ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

######################
message5 <- paste("#bayseq_table = topCounts(CD, group='DE', decreasing = TRUE, number = nrow( CD@data )) # PRINT THE ENTIRE TABLE of results ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("bayseq_table <- LoadCachedObject(bayseqnb.db, 'bayseq_table_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
################
message5 <- paste("print('These are the first five lines of the results: ') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(head(bayseq_table)) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Results\\',sep='') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("outputName=paste(the.file2,'_results_BaySeqNB.txt', sep='') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("b=paste(a,outputName,sep='\\') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#write.table(bayseq_table, file = b , quote=FALSE, sep='\t', row.names=TRUE) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("bayseq_table_FDR = subset(bayseq_table, FDR.DE < fdr)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Results\\',sep='')",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("outputName=paste(the.file2,'_fdr=',fdr,'_DE_genes_BaySeqNB.txt', sep='')",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("b=paste(a,outputName,sep='\\')",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#write.table(bayseq_table_FDR, file = b , quote=FALSE, sep='\t', row.names=TRUE)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  bayseq_table
  
 }else{ #Linux

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")

  the.file2 = strsplit(the.file,"/")

  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

  outputName=paste(the.file2,"_PlotMA_BaySeqNB.pdf", sep="")

  b=paste(a,outputName,sep="/")

  dev.print(device = pdf, file=b)

  dev.off()

  CD@estProps 

  bayseq_table = topCounts(CD, group="DE", decreasing = TRUE, number = nrow( CD@data )) # PRINT THE ENTIRE TABLE of results
  
  SaveInCache(bayseqnb.db, bayseq_table, "bayseq_table_key")
  
  print("These are the first five lines of the results: ")

  print(head(bayseq_table))

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",sep="")

  outputName=paste(the.file2,"_results_BaySeqNB.txt", sep="")

  b=paste(a,outputName,sep="/")

  write.table(bayseq_table, file = b , quote=FALSE, sep="\t", row.names=TRUE)

  #plotPosteriors(CD, group = "DE", col = c(rep("red", 100), rep("black", 900)))

  #a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")

  #outputName=paste(the.file2,"_Posteriors_BaySeqNB.pdf", sep="")

  #b=paste(a,outputName,sep="/")

  #dev.print(device = pdf, file=b)

  # Now print the table of results with FDR < 0.05

  bayseq_table_FDR = subset(bayseq_table, FDR.DE < fdr)

  a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",sep="")

  outputName=paste(the.file2,"_fdr=",fdr,"_DE_genes_BaySeqNB.txt", sep="")

  b=paste(a,outputName,sep="/")

  write.table(bayseq_table_FDR, file = b , quote=FALSE, sep="\t", row.names=TRUE)

     message=paste("Results of BaySeqNB have been saved in the ", Project,"/Results folder!", sep="")
     print(message)

#write into the project report
  report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste(" * In the *BaySeq Interface*, you clicked the **Run BaySeqNB** button at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Results` folder.", sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

 message2 <- paste("You chose the following count file: `",the.file2,"`, FDR: `",fdr,"`, Project: `",Project,"`, ",sep="\n")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("   Estimation Type :`", estType,"`", sep="")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("   Sample Size =`", samsize,"`", sep="")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("   Sample A =`", sA,"`", sep="")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("   Sample B =`", sB,"`", sep="")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("   FDR =`", fdr,"`", sep="")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message1 <- paste("Factors= `c(")
     write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message2 <- paste("'",conditions[1:(length(conditions)-1)],"',",sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message3 <- paste("'",conditions[length(conditions)],"'",sep="")
     write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message4 <- paste(")`, ")
     write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message1 <- paste("NDE= `c(")
     write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message2 <- paste("'",list_nde[1:(length(list_nde)-1)],"',",sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message3 <- paste("'",list_nde[length(list_nde)],"'",sep="")
     write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message4 <- paste(")`, ")
     write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message1 <- paste("DE= `c(")
     write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message2 <- paste("'",list_de[1:(length(list_de)-1)],"',",sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message3 <- paste("'",list_de[length(list_de)],"'",sep="")
     write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message4 <- paste(")`. ")
     write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste(" This R code has been run:",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" ```{r} ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


message5 <- paste("bayseqnb.db <- InitDb(db.name='BaySeqNB_db', db.path='cache')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" x <- LoadCachedObject(bayseqnb.db, 'bayseqnb_dataframe_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste(" Project <- LoadCachedObject(bayseqnb.db, 'project_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("the.file <- LoadCachedObject(bayseqnb.db, 'the_file_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("conditions <- LoadCachedObject(bayseqnb.db, 'conditions_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("list_nde <- LoadCachedObject(bayseqnb.db, 'list_nde_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("list_de <- LoadCachedObject(bayseqnb.db, 'list_de_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("estType <- LoadCachedObject(bayseqnb.db, 'estType_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("samsize <- LoadCachedObject(bayseqnb.db, 'samsize_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("sA <- LoadCachedObject(bayseqnb.db, 'sA_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("sB <- LoadCachedObject(bayseqnb.db, 'sB_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("fdr <- LoadCachedObject(bayseqnb.db, 'fdr_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


# message5 <- paste("x = read.table('",the.file,"',header=TRUE,row.names=1)", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("the.file ='",the.file,"'", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
# message5 <- paste("Project ='", Project,"'", sep="")
# write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
#      message5 <- paste("estType ='", estType,"'", sep="")
#      write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
#      message4 <- paste("fdr ='",fdr,"'",sep="")
#      write(message4, file = report,ncolumns = if(is.character(message4)) 1 else 5,append = TRUE, sep = "")
# 
#      message5 <- paste("samsize ='", samsize,"'", sep="")
#      write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
#      message5 <- paste("sA ='", sA,"'", sep="")
#      write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
#      message5 <- paste("sB ='", sB,"'", sep="")
#      write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
# 
#      message1 <- paste("conditions= c(")
#      write(message1, file = report,ncolumns = if(is.character(message1)) 1 else 5,append = TRUE, sep = "")
# 
#      message2 <- paste("'",conditions[1:(length(conditions)-1)],"',",sep="")
#      write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message3 <- paste("'",conditions[length(conditions)],"'",sep="")
#      write(message3, file = report,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "")
# 
#      message4 <- paste(") ")
#      write(message4, file = report,ncolumns = if(is.character(message4)) 1 else 5,append = TRUE, sep = "")
# 
#      message1 <- paste("NDE= c(")
#      write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message2 <- paste("'",list_nde[1:(length(list_nde)-1)],"',",sep="")
#      write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message3 <- paste("'",list_nde[length(list_nde)],"'",sep="")
#      write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message4 <- paste(") ")
#      write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message1 <- paste("DE= c(")
#      write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message2 <- paste("'",list_de[1:(length(list_de)-1)],"',",sep="")
#      write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message3 <- paste("'",list_de[length(list_de)],"'",sep="")
#      write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message4 <- paste(") ")
#      write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")
# 
#      message5 <- paste("  ",sep="\n")
#      write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("set.seed(102) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("options(width = 90 )",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("samsize = as.numeric(samsize) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("fdr = as.numeric(fdr) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("counts = data.matrix(x, rownames.force = NA) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print('Count file loaded: ') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(head(counts)) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("CD <- new('countData', data = counts)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("libsizes(CD) <- getLibsizes(CD, estimationType = estType)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("replicates(CD) = conditions",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("NDE = as.numeric(list_nde)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("DE = as.numeric(list_de)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("groups(CD) <- list(NDE = NDE, DE = DE)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  

###############

message5 <- paste("#CD <- getPriors.NB(CD, samplesize = samsize, estimation = 'QL', cl = NULL)",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
message5 <- paste("CD <- LoadCachedObject(bayseqnb.db, 'CD_priors_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#CD <- getLikelihoods.NB(CD, pET = 'BIC', cl = NULL) ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("CD <- LoadCachedObject(bayseqnb.db, 'CD_likelihoods_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
######################

message5 <- paste("plotMA.CD(CD, samplesA = sA, samplesB = sB, col = c(rep('red', 100), rep('black', 900))) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Plots/',sep='') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = strsplit(the.file,'/') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("outputName=paste(the.file2,'_PlotMA_BaySeqNB.pdf', sep='') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("b=paste(a,outputName,sep='/') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#dev.print(device = pdf, file=b) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("CD@estProps ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

######################
message5 <- paste("#bayseq_table = topCounts(CD, group='DE', decreasing = TRUE, number = nrow( CD@data )) # PRINT THE ENTIRE TABLE of results ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("bayseq_table <- LoadCachedObject(bayseqnb.db, 'bayseq_table_key')", sep="")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
################

message5 <- paste("print('These are the first five lines of the results: ') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("print(head(bayseq_table)) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Results/',sep='') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("outputName=paste(the.file2,'_results_BaySeqNB.txt', sep='') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("b=paste(a,outputName,sep='/') ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#write.table(bayseq_table, file = b , quote=FALSE, sep='\t', row.names=TRUE) ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("bayseq_table_FDR = subset(bayseq_table, FDR.DE < fdr)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Results/',sep='')",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("outputName=paste(the.file2,'_fdr=',fdr,'_DE_genes_BaySeqNB.txt', sep='')",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("b=paste(a,outputName,sep='/')",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste("#write.table(bayseq_table_FDR, file = b , quote=FALSE, sep='\t', row.names=TRUE)",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

message8 <- paste(" ``` ",sep="\n")
write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")

message5 <- paste(" _______________________________________________________________________ ",sep="\n")
write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  bayseq_table
 }

}

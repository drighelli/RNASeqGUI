Venn4de <- function(x,y,z,w,the.file1,the.file2,the.file3,the.file4,label1,label2,label3,label4,Project){

res=NULL # just to create the dialog window when the venn diagram is generated
require(VennDiagram)
require(tiff)
print("capabilities(tiff)")
print(capabilities("tiff"))

dbfilename <- paste(label1, label2, label3, label4, 'venn4de_db', sep="_")


db <- InitDb(db.name=dbfilename, db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))

SaveInCache(db, the.file1, "thefile1_key")
SaveInCache(db, the.file2, "thefile2_key")
SaveInCache(db, the.file3, "thefile3_key")
SaveInCache(db, the.file4, "thefile4_key")
SaveInCache(db, Project, "project_key")
SaveInCache(db, x, "maindataframe1_key")
SaveInCache(db, y, "maindataframe2_key")
SaveInCache(db, z, "maindataframe3_key")
SaveInCache(db, w, "maindataframe4_key")
SaveInCache(db, label1, "label1_key")
SaveInCache(db, label2, "label2_key")
SaveInCache(db, label3, "label3_key")
SaveInCache(db, label4, "label4_key")



#x <- read.table("a", header=TRUE, row.names=1)
#y <- read.table("b", header=TRUE, row.names=1)
#z <- read.table("c", header=TRUE, row.names=1)
#w <- read.table("d", header=TRUE, row.names=1)
#label1 = "a"
#label2 = "b"
#label3 = "c"
#label4 = "d"

 a = row.names(x)
 b = row.names(y)
 c = row.names(z)
 d = row.names(w)
 Lists <- list(a,b,c,d)

  names(Lists) <- c(label1,label2,label3,label4)

  ab <- intersect(a,b)     #common gene names
  SaveInCache(db, ab, "ab_key")
  cd <- intersect(c,d)
  SaveInCache(db, cd, "cd_key")
  abcd <- intersect(ab,cd)
  SaveInCache(db, abcd, "abcd_key")

  if(Sys.info()[[1]]=="Windows"){
    a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\",sep="") 
    outputName=paste(label1,"_",label2,"_",label3,"_",label4,"_genes_in_intersection.txt",sep="")
    
    b=paste(a,outputName,sep="")
    
    write.table(abcd, file = b , quote=FALSE, sep="\t", row.names=FALSE)
    
    a=paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Plots\\",sep="")
    
    outputName=paste(label1,"_",label2,"_",label3,"_",label4,"_VennDiagramDE.tiff",sep="")
    
    b=paste(a,outputName,sep="")
    
    print(venn.diagram(x=Lists, filename = b,  col = "transparent",fill=c("blue","red","green","pink"), alpha = 0.50, cex = 1.5, fontfamily ="serif", fontface = "bold", cat.cex = 1.2, cat.pos =0,cat.dist = 0.07,cat.fontfamily = "serif", rotation.degree = 270, margin = 0.2))
    
    #write into the project report
    report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *Result Comparison Interface*, you clicked the **VennDiagram 4 sets DE** button for  at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"\\Plots` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" ```{r} ",sep="\n")
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
#     message5 <- paste("w = read.table(", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("'",the.file4,"',header=TRUE,row.names=1)", sep="")
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
#     message5 <- paste("label4='",label4,"'", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
#     
#     message5 <- paste("Project='",Project,"'", sep="")
#     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("require(limma)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("db <- InitDb(db.name='venn4de_db', db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("x <- LoadCachedObject(db, 'maindataframe1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("y <- LoadCachedObject(db, 'maindataframe2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("z <- LoadCachedObject(db, 'maindataframe3_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("w <- LoadCachedObject(db, 'maindataframe4_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file1 <- LoadCachedObject(db, 'thefile1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2 <- LoadCachedObject(db, 'thefile2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file3 <- LoadCachedObject(db, 'thefile3_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file3 <- LoadCachedObject(db, 'thefile4_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label1 <- LoadCachedObject(db, 'label1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label2 <- LoadCachedObject(db, 'label2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label3 <- LoadCachedObject(db, 'label3_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label4 <- LoadCachedObject(db, 'label4_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message2 <- paste(" a = row.names(x)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" b = row.names(y)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" c = row.names(z)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" d = row.names(w)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" Lists <- list(a,b,c,d)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("names(Lists) <- c(label1,label2,label3,label4)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   ab <- intersect(a,b)     #common gene names",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  cd <- intersect(c,d)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" abcd <- intersect(ab,cd)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Results\\',sep='') ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  outputName=paste(label1,'_',label2,'_',label3,'_',label4,'_genes_in_intersection.txt',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  b=paste(a,outputName,sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #write.table(abcd, file = b , quote=FALSE, sep='\t', row.names=FALSE)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  a=paste(getwd(),'\\RNASeqGUI_Projects\\',Project,'\\Plots\\',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  outputName=paste(label1,'_',label2,'_',label3,'_',label4,'_VennDiagramDE.tiff',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  b=paste(a,outputName,sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("print(venn.diagram(x=Lists, filename = b,  col = 'transparent',fill=c('blue','red','green','pink'),",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("alpha = 0.50, cex = 1.5, fontfamily ='serif', fontface = 'bold', cat.cex = 1.2, cat.pos =0,",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("cat.dist = 0.07,cat.fontfamily = 'serif', rotation.degree = 270, margin = 0.2))",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    #write into the project report
    message=paste("The ",outputName," file has been saved in the ", Project,"\\Plots folder!", sep="")
    print(message)
    res = message
    res
  
  }else{ #Linux

    a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",sep="") 
    outputName=paste(label1,"_",label2,"_",label3,"_",label4,"_genes_in_intersection.txt",sep="")
    
    b=paste(a,outputName,sep="")
    
    write.table(abcd, file = b , quote=FALSE, sep="\t", row.names=FALSE)
    
    a=paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Plots/",sep="")
    
    outputName=paste(label1,"_",label2,"_",label3,"_",label4,"_VennDiagramDE.tiff",sep="")
    
    b=paste(a,outputName,sep="")
    
    print(venn.diagram(x=Lists, filename = b,  col = "transparent",fill=c("blue","red","green","pink"), alpha = 0.50, cex = 1.5, fontfamily ="serif", fontface = "bold", cat.cex = 1.2, cat.pos =0,cat.dist = 0.07,cat.fontfamily = "serif", rotation.degree = 270, margin = 0.2))
    
    #write into the project report
    report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *Result Comparison Interface*, you clicked the **VennDiagram 4 sets DE** button for  at `", Sys.time(),"` and the ",outputName," file has been saved in the `", Project,"/Plots` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" ```{r} ",sep="\n")
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
    #     message5 <- paste("w = read.table(", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     
    #     message5 <- paste("'",the.file4,"',header=TRUE,row.names=1)", sep="")
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
    #     message5 <- paste("label4='",label4,"'", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    #     
    #     message5 <- paste("Project='",Project,"'", sep="")
    #     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("require(limma)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("db <- InitDb(db.name='venn4de_db', db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("x <- LoadCachedObject(db, 'maindataframe1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("y <- LoadCachedObject(db, 'maindataframe2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("z <- LoadCachedObject(db, 'maindataframe3_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("w <- LoadCachedObject(db, 'maindataframe4_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file1 <- LoadCachedObject(db, 'thefile1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file2 <- LoadCachedObject(db, 'thefile2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file3 <- LoadCachedObject(db, 'thefile3_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("the.file3 <- LoadCachedObject(db, 'thefile4_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label1 <- LoadCachedObject(db, 'label1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label2 <- LoadCachedObject(db, 'label2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label3 <- LoadCachedObject(db, 'label3_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("label4 <- LoadCachedObject(db, 'label4_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message2 <- paste(" a = row.names(x)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" b = row.names(y)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" c = row.names(z)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" d = row.names(w)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" Lists <- list(a,b,c,d)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("names(Lists) <- c(label1,label2,label3,label4)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("   ab <- intersect(a,b)     #common gene names",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  cd <- intersect(c,d)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" abcd <- intersect(ab,cd)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Results/',sep='') ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  outputName=paste(label1,'_',label2,'_',label3,'_',label4,'_genes_in_intersection.txt',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  b=paste(a,outputName,sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  #write.table(abcd, file = b , quote=FALSE, sep='\t', row.names=FALSE)",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  a=paste(getwd(),'/RNASeqGUI_Projects/',Project,'/Plots/',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  outputName=paste(label1,'_',label2,'_',label3,'_',label4,'_VennDiagramDE.tiff',sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("  b=paste(a,outputName,sep='')",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("print(venn.diagram(x=Lists, filename = b,  col = 'transparent',fill=c('blue','red','green','pink'),",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("alpha = 0.50, cex = 1.5, fontfamily ='serif', fontface = 'bold', cat.cex = 1.2, cat.pos =0,",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("cat.dist = 0.07,cat.fontfamily = 'serif', rotation.degree = 270, margin = 0.2))",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    #write into the project report
    message=paste("The ",outputName," file has been saved in the ", Project,"/Plots folder!", sep="")
    print(message)
    res = message
    res

}


}

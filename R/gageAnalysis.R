gage_function <- function(fileName,geneSet1,geneSet2,Project,hsa,mmu,dme,dre,conversion,conversion2,conversion3,n){

 require(biomaRt)
 require(gage)
 require(pathview)
 require(lattice)

  if(Sys.info()[[1]]=="Windows"){
    just.name <- substring(fileName, max(gregexpr("\\", fileName, fixed=TRUE)[[1]])+1)
  } else {
    just.name <- substring(fileName, max(gregexpr("/", fileName, fixed=TRUE)[[1]])+1)
  }
  
  dbname <- paste(just.name,geneSet1,geneSet2,'gage_db',sep="_")
 db <- InitDb(db.name=dbname, db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))

 SaveInCache(db, fileName, "filename_key")
 SaveInCache(db, Project,  "project_key")
 SaveInCache(db, geneSet1, "geneset1_key")
 SaveInCache(db, geneSet2, "geneset2_key")
 SaveInCache(db, hsa, "hsa_key")
 SaveInCache(db, mmu, "mmu_key")
 SaveInCache(db, dme, "dme_key")
 SaveInCache(db, dre, "dre_key")
 SaveInCache(db, conversion, "conversion_key")
 SaveInCache(db, conversion2, "conversion2_key")
 SaveInCache(db, conversion3, "conversion3_key")
 SaveInCache(db, n, "n_key")

 res=NULL
 n = as.numeric(n)
 # fileName="/media/6b8a4404-f3e2-4fbc-9fc4-5a0ebc8d0635/Patriarca/LPro_vs_untreated.txt" 
 print("Expression data selected: ") 
 print(fileName)
 file.name=read.table(fileName, row.names=1, header=TRUE)
 #file.name=as.matrix(file.name) #to speed up computations
 print("dimensions:")
 print(dim(file.name))
 print(head(file.name))
 kegg.gs = NULL
 go.gs = NULL
 print("Loading Database. This step might take a while. PLEASE, WAIT!")
  
  if (geneSet1==TRUE) { # kegg.gs 
    if(hsa==TRUE){ks=kegg.gsets(species = "hsa", id.type = "entrez")}
    if(mmu==TRUE){ks=kegg.gsets(species = "mmu", id.type = "entrez")}
    if(dme==TRUE){ks=kegg.gsets(species = "dme", id.type = "entrez")}
    if(dre==TRUE){ks=kegg.gsets(species = "dre", id.type = "entrez")}
    kegg.gs=ks$kg.sets
    print("KEGG database loaded:")
    print(head(kegg.gs[1]))
  }
  
  if (geneSet2==TRUE) { # go.gs
    data(bods)
    if(hsa==TRUE){row=subset(bods, bods[,3] == "hsa")}
    if(mmu==TRUE){row=subset(bods, bods[,3] == "mmu")}
    if(dme==TRUE){row=subset(bods, bods[,3] == "dme")}
    if(dre==TRUE){row=subset(bods, bods[,3] == "dre")}
    go=go.gsets(species = row[2], id.type = "entrez")
    go.gs=go$go.sets
    print("GO database loaded:")
    print(head(go.gs[1]))
  }

  if(conversion==TRUE){ #ensembl gene ids to entrez genes
    print("Selecting a BioMart database and dataset. This step might take a while. PLEASE, WAIT!")
    if(hsa==TRUE){ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="hsapiens_gene_ensembl")} #to connect to BioMart
    if(mmu==TRUE){ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="mmusculus_gene_ensembl")} #to connect to a BioMart database   
    if(dme==TRUE){ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="dmelanogaster_gene_ensembl")} #to connect to a BioMart database
    if(dre==TRUE){ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="drerio_gene_ensembl")} #to connect to a BioMart database
    results <- getBM(attributes = c("ensembl_gene_id","entrezgene"), mart=ensembl)
    print("Conversion map:")
    print(head(results))
    print("Converting row names from 'ensembl_gene_id' to 'entrezgene'. PLEASE, WAIT!")
    file.name <- mol.sum(mol.data = file.name, id.map = results,sum.method = "mean") #conversion
    print("Expression data modified:")
    print(dim(file.name))
    print(head(file.name))
    SaveInCache(db, file.name, "filenameconverted_key")
  }

  if(conversion2==TRUE){ #gene names to entrez genes
    print("Selecting a BioMart database and dataset. This step might take a while. PLEASE, WAIT!")
    if(hsa==TRUE){
         #ensembl=useDataset('hsapiens_gene_ensembl',mart=useMart('ensembl'))
         ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='hsapiens_gene_ensembl')
         results <- getBM(attributes = c("hgnc_symbol","entrezgene"), mart=ensembl)
    }
    if(mmu==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="mmusculus_gene_ensembl")
         results <- getBM(attributes = c("mgi_symbol","entrezgene"), mart=ensembl)
    } 
    if(dme==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="dmelanogaster_gene_ensembl")
         results <- getBM(attributes = c("flybasename_gene","entrezgene"), mart=ensembl)
    } #to connect to a BioMart database
    if(dre==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="drerio_gene_ensembl")
         results <- getBM(attributes = c("zfin_symbol","entrezgene"), mart=ensembl)
    } #to connect to a BioMart database

    print("Conversion map:")
    print(head(results))
    print("Converting row names from 'gene names' to 'entrezgene'. PLEASE, WAIT!")
    file.name <- mol.sum(mol.data = file.name, id.map = results,sum.method = "mean") #conversion
    print("Expression data modified:")
    print(dim(file.name))
    print(head(file.name))
    SaveInCache(db, file.name, "filenameconverted_key")
  }

  if(conversion3==TRUE){ # gencode names to entrez genes
    print("Selecting a BioMart database and dataset. This step might take a while. PLEASE, WAIT!")
    if(hsa==TRUE){ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="hsapiens_gene_ensembl")} #to connect to BioMart
    if(mmu==TRUE){ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="mmusculus_gene_ensembl")} #to connect to a BioMart database   
    if(dme==TRUE){ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="dmelanogaster_gene_ensembl")} #to connect to a BioMart database
    if(dre==TRUE){ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="drerio_gene_ensembl")} #to connect to a BioMart database
    print("Converting GENCODEs to ensembl gene id")   
    newrownames = strsplit(rownames(file.name),"\\.")
    nrownames = NULL
    for (i in 1:length(newrownames) ){
        nrownames[i] = newrownames[[i]][1]
    }
    rownames(file.name) = nrownames
    print("GENCODEs rewritten as follows:")
    print(head(file.name))
    results <- getBM(attributes = c("ensembl_gene_id","entrezgene"), mart=ensembl)
    print("Conversion map:")
    print(head(results))
    print("Converting row names from 'ensembl_gene_id' to 'entrezgene'. PLEASE, WAIT!")
    file.name <- mol.sum(mol.data = file.name, id.map = results,sum.method = "mean") #conversion
    print("Expression data modified:")
    print(dim(file.name))
    print(head(file.name))
    SaveInCache(db, file.name, "filenameconverted_key")
  }
  
  out.suffix=NULL 
  fc=NULL
  
  # Do an automatic check whether is deseq , edger or noiseq

  if(length(colnames(file.name))==0){
    fc=file.name[,1]
    names(fc)=rownames(file.name)
    out.suffix="genelist"
    print("You are using a generic gene list.")
  }else if(colnames(file.name)[1] == "logFC"){  # you are using EdgeR
    fc=file.name[,'logFC']
    names(fc)=rownames(file.name)
    out.suffix="edger"
    print("You are using an edgeR result file.")
  }else if(colnames(file.name)[2] == "log2FoldChange") {  # you are using DESeq2
    fc=file.name[,'log2FoldChange']
    names(fc)=rownames(file.name)
    sum(is.infinite(fc)) #to manage the problem of inf
    fc[fc>10]=10
    fc[fc< -10]=-10
    out.suffix="deseq2"
    print("You are using an DESeq2 result file.")
  }else if(colnames(file.name)[5] == "log2FoldChange") { # you are using DESeq
    fc=file.name[,'log2FoldChange']
    names(fc)=rownames(file.name)
    sum(is.infinite(fc)) #to manage the problem of inf
    fc[fc>10]=10
    fc[fc< -10]=-10
    out.suffix="deseq"
    print("You are using an DESeq result file.") 
  }else if(colnames(file.name)[3] == "M"){  # you are using NOISeq
    fc=file.name[,'M']
    print(head(fc))
    names(fc)=rownames(file.name)
    sum(is.infinite(fc)) #to manage the problem of inf
    fc[fc>10]=10
    fc[fc< -10]=-10
    out.suffix="noiseq"
    print("You are using a NOISeq result file.")
  }else if(colnames(file.name)[5] == "log2FC"){ # you are using NOISeqBIO
    fc=file.name[,'log2FC']
    print(head(fc))
    names(fc)=rownames(file.name)
    sum(is.infinite(fc)) #to manage the problem of inf
    fc[fc>10]=10
    fc[fc< -10]=-10
    out.suffix="noiseqbio"
    print("You are using a NOISeqBIO result file.")
  }else{
    print("print('ERROR: unrecognized file type!!!')")
    return()
  }
  
  exp.fc=fc
  SaveInCache(db, exp.fc, "expfc_key")
  SaveInCache(db, out.suffix, "outsuffix_key")
  print("Performing gage analysis. PLEASE, WAIT!")
  
  if (geneSet1==TRUE){ # kegg.gs
      res <- gage(exp.fc, gsets = kegg.gs, ref = NULL, samp = NULL, same.dir = FALSE, saaTest = gs.zTest, use.fold = FALSE)
  }else{               # go.gs
      res <- gage(exp.fc, gsets = go.gs  , ref = NULL, samp = NULL, same.dir = FALSE, saaTest = gs.zTest, use.fold = FALSE)
  }

  SaveInCache(db, res, "res_key")

  print("First results:")
  print(head(res[[1]],10))
  
  sel <- res$greater[, "q.val"] < 0.05 & !is.na(res$greater[,"q.val"]) # UP
  path.ids <- rownames(res$greater)[sel] # UP
  sel.l <- res$less[, "q.val"] < 0.05 & !is.na(res$less[,"q.val"]) # DOWN
  path.ids.l <- rownames(res$less)[sel.l] # DOWN
  path.ids2 <- substr(c(path.ids, path.ids.l), 1, 8) #put greater and less together
  #print("First lines of the results for UP-regulated pathways:")
  #print(path.ids)
  #print("First lines of the results for DOWN-regulated pathways:")
  #print(path.ids.l)
  print("First lines of the results for UP AND DOWN-regulated pathways:")
  print(path.ids2)
  SaveInCache(db, path.ids2, "finalresults_key")

  if(Sys.info()[[1]]=="Windows"){
    prevwd=getwd()
    setwd(paste(getwd(),"RNASeqGUI_Projects", Project,"Plots", sep="\\"))
    if (geneSet1 == TRUE){ #kegg
       if(hsa==TRUE){pv.out.list <- sapply(path.ids2[1:20], function(pid) pathview( gene.data = exp.fc, pathway.id = pid, species = "hsa", out.suffix=out.suffix ))}
       if(mmu==TRUE){pv.out.list <- sapply(path.ids2[1:20], function(pid) pathview( gene.data = exp.fc, pathway.id = pid, species = "mmu", out.suffix=out.suffix ))}
       if(dme==TRUE){pv.out.list <- sapply(path.ids2[1:20], function(pid) pathview( gene.data = exp.fc, pathway.id = pid, species = "dme", out.suffix=out.suffix ))}
       if(dre==TRUE){pv.out.list <- sapply(path.ids2[1:20], function(pid) pathview( gene.data = exp.fc, pathway.id = pid, species = "dre", out.suffix=out.suffix ))}
    }
    setwd(prevwd)

  if (geneSet1 == TRUE){ #kegg
    write.table(res[[1]], file = paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\Pathway_result.txt",sep="") , quote=TRUE, sep="\t", row.names=TRUE)
    rel_res = subset(res[[1]], res[[1]][,4] < 0.05) # select significant 
    write.table(rel_res, file = paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\Significant_Pathways.txt",sep="") , quote=TRUE, sep="\t", row.names=TRUE)
    message=paste("Results of Pathway analysis have been saved in RNASeqGUI_Projects\\", Project,"\\Results folder!", sep="")
    print(message)
  }

  if (geneSet2 == TRUE){ #GO
    write.table(res[[1]], file = paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\GO_result.txt",sep="") , quote=TRUE, sep="\t", row.names=TRUE)
    rel_res = subset(res[[1]], res[[1]][,4] < 0.05) # select significant 
    write.table(rel_res, file = paste(getwd(),"\\RNASeqGUI_Projects\\",Project,"\\Results\\Significant_Pathways.txt",sep="") , quote=TRUE, sep="\t", row.names=TRUE)
    message=paste("Results of Gene Ontology Analysis have been saved in RNASeqGUI_Projects\\", Project,"\\Results folder!", sep="")
    print(message)
  }
  
    #write into the project report
    report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *Gene-Set/Pathway Interface*, you clicked the **GAGE** button at `", Sys.time(),"` and the GAGE_result.txt file has been saved in the `", Project,"\\Results` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste("You chose the following file:`",fileName,"`,geneSet1:`",geneSet1,"`,geneSet2:`",geneSet2,"`,Project:`",Project,"`,hsa:`",hsa,"`,mmu:`",mmu,"`,dme:`",dme,"`,dre:`",dme,
"`,conversion:`",conversion,"`,conversion2:`",conversion2,"`,conversion3:`",conversion3,"`,n:`",n,"`, ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" require(biomaRt)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" require(gage)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" require(pathview)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n") 
    message5 <- paste(" require(lattice)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste( "geneSet1 <- ",geneSet1,"", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "geneSet2 <-  ",geneSet2,"", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" cachedbname='", dbname,"'", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" db <- InitDb(db.name=cachedbname, db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" fileName <- LoadCachedObject(db, 'filename_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" Project <- LoadCachedObject(db, 'project_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" geneSet1 <- LoadCachedObject(db, 'geneset1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" geneSet2 <- LoadCachedObject(db, 'geneset2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" conversion <- LoadCachedObject(db, 'conversion_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" hsa <- LoadCachedObject(db, 'hsa_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" dme <- LoadCachedObject(db, 'dme_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" mmu <- LoadCachedObject(db, 'mmu_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" dre <- LoadCachedObject(db, 'dre_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" res=NULL ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" print('Expression data selected: ')",sep="\n") 
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" print('",fileName,"')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" file.name=read.table('",fileName,"', row.names=1, header=TRUE)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" print('dimensions:')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" print(dim(file.name))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" print(head(file.name))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" kegg.gs = NULL",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" go.gs = NULL",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if (geneSet1==TRUE){ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(hsa==TRUE){ks=kegg.gsets(species = 'hsa', id.type = 'entrez')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(mmu==TRUE){ks=kegg.gsets(species = 'mmu', id.type = 'entrez')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(dme==TRUE){ks=kegg.gsets(species = 'dme', id.type = 'entrez')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(dre==TRUE){ks=kegg.gsets(species = 'dre', id.type = 'entrez')}",sep="\n")
    message5 <- paste("  kegg.gs=ks$kg.sets}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if (geneSet2==TRUE){ data(bods)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(hsa==TRUE){row=subset(bods, bods[,3] == 'hsa')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(mmu==TRUE){row=subset(bods, bods[,3] == 'mmu')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(dme==TRUE){row=subset(bods, bods[,3] == 'dme')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(dre==TRUE){row=subset(bods, bods[,3] == 'dre')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("go=go.gsets(species = row[2], id.type = 'entrez')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("go.gs=go$go.sets}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(conversion==TRUE){ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("if(hsa==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='hsapiens_gene_ensembl')} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(mmu==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='mmusculus_gene_ensembl')} ",sep="\n")   
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(dme==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='dmelanogaster_gene_ensembl')} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(dre==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='drerio_gene_ensembl')}} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("#if(conversion2==TRUE){ #gene names to entrez genes",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("if(hsa==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='hsapiens_gene_ensembl')} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#     results <- getBM(attributes = c('hgnc_symbol','entrezgene'), mart=ensembl)}#to connect to a BioMart database",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(mmu==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='mmusculus_gene_ensembl')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#     results <- getBM(attributes = c('mgi_symbol','entrezgene') , mart=ensembl)} #to connect to a BioMart database",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(dme==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='dmelanogaster_gene_ensembl')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" #     results <- getBM(attributes = c('flybasename_gene','entrezgene'), mart=ensembl)} #to connect to a BioMart database",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(dre==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='drerio_gene_ensembl')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#     results <- getBM(attributes = c('zfin_symbol','entrezgene'), mart=ensembl)} #to connect to a BioMart database",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#file.name <- mol.sum(mol.data = file.name, id.map = results,sum.method = 'mean') }",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(conversion3==TRUE){ # gencode names to entrez genes",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("#if(hsa==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='hsapiens_gene_ensembl')} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(mmu==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='mmusculus_gene_ensembl')} #to connect to a BioMart database",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("#if(dme==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='dmelanogaster_gene_ensembl')} #to connect to a BioMartdatabase",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#if(dre==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='drerio_gene_ensembl')} #to connect to a BioMart database",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#newrownames = strsplit(rownames(file.name),'\\.')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#nrownames = NULL",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#for (i in 1:length(newrownames) ){nrownames[i] = newrownames[[i]][1]}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#rownames(file.name) = nrownames",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#results <- getBM(attributes = c('ensembl_gene_id','entrezgene'), mart=ensembl)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#file.name <- mol.sum(mol.data = file.name, id.map = results,sum.method = 'mean')  }",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("file.name <- LoadCachedObject(db, 'filenameconverted_key')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print('Expression data modified:')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print(dim(file.name))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print(head(file.name))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("out.suffix=NULL",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("fc=NULL",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("exp.fc <- LoadCachedObject(db, 'expfc_key')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("out.suffix <- LoadCachedObject(db, 'outsuffix_key')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if (colnames(file.name)[1] == 'log2FoldChange') {fc=file.name[,'log2FoldChange']",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#names(fc)=rownames(file.name)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#sum(is.infinite(fc)) #to manage the problem of inf",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#fc[fc>10]=10",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#fc[fc< -10]=-10",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#out.suffix='deseq' }",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(colnames(file.name)[1] == 'logFC'){ fc=file.name[,'logFC']",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#names(fc)=rownames(file.name)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#out.suffix='edger'}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(colnames(file.name)[3] == 'M'){ fc=file.name[,'M']",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#names(fc)=rownames(file.name)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#sum(is.infinite(fc)) #to manage the problem of inf",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#fc[fc>10]=10",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#fc[fc< -10]=-10",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#out.suffix='noiseq'}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#exp.fc=fc",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if (geneSet1==TRUE){res <- gage(exp.fc, gsets = kegg.gs, ref = NULL, samp = NULL, same.dir = FALSE, saaTest = gs.zTest, use.fold = FALSE)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#}else{res <- gage(exp.fc, gsets = go.gs  , ref = NULL, samp = NULL, same.dir = FALSE, saaTest = gs.zTest, use.fold = FALSE)}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("res <- LoadCachedObject(db, 'res_key')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("print('First lines of results:')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print(head(res[[1]],10))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#sel <- res$greater[, 'q.val'] < 0.01 & !is.na(res$greater[, 'q.val'])",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#path.ids <- rownames(res$greater)[sel]",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#sel.l <- res$less[, 'q.val'] < 0.01 & !is.na(res$less[,'q.val'])",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#path.ids.l <- rownames(res$less)[sel.l]",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#path.ids2 <- substr(c(path.ids, path.ids.l), 1, 8) #put greater and less together",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("path.ids2 <- LoadCachedObject(db, 'finalresults_key')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print('First lines of the results:')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print(head(path.ids2))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#pv.out.list <- sapply(path.ids2, function(pid){ pathview( gene.data = exp.fc, pathway.id = pid, species = specie, out.suffix=out.suffix)})", sep = "\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  }else{         # Linux

    the.file2 = strsplit(fileName,"/")    
    the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile   
    the.file2 = substring(the.file2,1,nchar(the.file2)-4)  # eliminates ".txt"

   gagepath <- paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",the.file2,"_Gage_Analysis/",sep="")
   dir.create(gagepath, showWarnings = TRUE, recursive = TRUE)
   print(paste("The ",gagepath," folder has been created to store the results of the 'Gage Interface' inside the ", getwd(),"/RNASeqGUI_Projects/",Project,"/Results folder.",sep="")) 


    prevwd=getwd()
    setwd(gagepath)
    if (geneSet1 == TRUE){ #kegg
       if(hsa==TRUE){pv.out.list <- sapply(path.ids2[1:20], function(pid) pathview( gene.data = exp.fc, pathway.id = pid, species = "hsa", out.suffix=out.suffix ))}
       if(mmu==TRUE){pv.out.list <- sapply(path.ids2[1:20], function(pid) pathview( gene.data = exp.fc, pathway.id = pid, species = "mmu", out.suffix=out.suffix ))}
       if(dme==TRUE){pv.out.list <- sapply(path.ids2[1:20], function(pid) pathview( gene.data = exp.fc, pathway.id = pid, species = "dme", out.suffix=out.suffix ))}
       if(dre==TRUE){pv.out.list <- sapply(path.ids2[1:20], function(pid) pathview( gene.data = exp.fc, pathway.id = pid, species = "dre", out.suffix=out.suffix ))}
    }
    setwd(prevwd)

   rel_res=NULL

    if (geneSet1 == TRUE){ #kegg
        resultsfilename <- paste(gagepath,the.file2,"_Pathway_result.txt",sep="")
       write.table(res[[1]], file = resultsfilename , quote=TRUE, sep="\t", row.names=TRUE)
       rnames=(row.names(res[[1]]))
       namesres = cbind(rnames,res[[1]])
       write.table(namesres, file = paste(gagepath,the.file2,"_Pathway_result.tsv",sep="") , quote=FALSE, sep="\t", row.names=FALSE)
       rel_res = subset(res[[1]], res[[1]][,4] < 0.05) # select significant 
       significant.pathways <- paste(gagepath,the.file2,"_Significant_Pathways.txt",sep="")
       write.table(rel_res, file = significant.pathways , quote=TRUE, sep="\t", row.names=TRUE)
    }

    if (geneSet2 == TRUE){ #GO
       write.table(res[[1]], file = paste(getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",the.file2,"_GO_result.txt",sep="") , quote=TRUE, sep="\t", row.names=TRUE)
       write.table(res[[1]], file = paste(gagepath,"/",the.file2,"_GO_result.txt",sep="") , quote=TRUE, sep="\t", row.names=TRUE)
       rnames=(row.names(res[[1]]))
       namesres = cbind(rnames,res[[1]])
       write.table(namesres, file = paste(gagepath,"/",the.file2,"_GO_result.tsv",sep="") , quote=FALSE, sep="\t", row.names=FALSE)

       rel_res = subset(res[[1]], res[[1]][,4] < 0.05) # select significant 
       write.table(rel_res, file = paste(gagepath,"/",the.file2,"_Significant_GOs.txt",sep="") , quote=TRUE, sep="\t", row.names=TRUE)
    }
    
    #write into the project report
    report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message2 <- paste(" * In the *Gene-Set/Pathway Interface*, you clicked the **GAGE** button at `", Sys.time(),"` and the GAGE_result.txt file has been saved in the `", Project,"/Results` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message2 <- paste("You chose the following file:`",fileName,"`,geneSet1:`",geneSet1,"`,geneSet2:`",geneSet2,"`,Project:`",Project,"`,hsa:`",hsa,"`,mmu:`",mmu,"`,dme:`",dme,"`,dre:`",dme,
"`,conversion:`",conversion,"`,conversion2:`",conversion2,"`,conversion3:`",conversion3,"`,n:`",n,"`, ",sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n") 
    message5 <- paste(" require(biomaRt)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" require(gage)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" require(pathview)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")  
    message5 <- paste(" require(lattice)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")     
    message5 <- paste( "geneSet1 <- ",geneSet1,"", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "geneSet2 <-  ",geneSet2,"", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste(" cachedbname='", dbname,"'", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("db <- InitDb(db.name=cachedbname, db.path='cache')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("fileName <- LoadCachedObject(db, 'filename_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("Project <- LoadCachedObject(db, 'project_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("geneSet1 <- LoadCachedObject(db, 'geneset1_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("geneSet2 <- LoadCachedObject(db, 'geneset2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("hsa <- LoadCachedObject(db, 'hsa_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("dme <- LoadCachedObject(db, 'dme_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("mmu <- LoadCachedObject(db, 'mmu_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("dre <- LoadCachedObject(db, 'dre_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("conversion <- LoadCachedObject(db, 'conversion_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("conversion2 <- LoadCachedObject(db, 'conversion2_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("conversion3 <- LoadCachedObject(db, 'conversion3_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("n <- LoadCachedObject(db, 'n_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("res=NULL ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print('Expression data selected: ')",sep="\n") 
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print('",fileName,"')",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("file.name=read.table('",fileName,"', row.names=1, header=TRUE)",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print('dimensions:')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print(dim(file.name))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print(head(file.name))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("kegg.gs = NULL",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("go.gs = NULL",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if (geneSet1==TRUE){ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(hsa==TRUE){ks=kegg.gsets(species = 'hsa', id.type = 'entrez')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(mmu==TRUE){ks=kegg.gsets(species = 'mmu', id.type = 'entrez')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(dme==TRUE){ks=kegg.gsets(species = 'dme', id.type = 'entrez')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(dre==TRUE){ks=kegg.gsets(species = 'dre', id.type = 'entrez')}",sep="\n")
    message5 <- paste("kegg.gs=ks$kg.sets}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if (geneSet2==TRUE){ data(bods)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(hsa==TRUE){row=subset(bods, bods[,3] == 'hsa')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(mmu==TRUE){row=subset(bods, bods[,3] == 'mmu')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(dme==TRUE){row=subset(bods, bods[,3] == 'dme')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(dre==TRUE){row=subset(bods, bods[,3] == 'dre')}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("go=go.gsets(species = row[2], id.type = 'entrez')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("go.gs=go$go.sets}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(conversion==TRUE){ #ensembl gene ids to entrez genes",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("if(hsa==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='hsapiens_gene_ensembl')} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(mmu==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='mmusculus_gene_ensembl')} ",sep="\n")   
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(dme==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='dmelanogaster_gene_ensembl')} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("if(dre==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='drerio_gene_ensembl')}} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("#if(conversion2==TRUE){ #gene names to entrez genes",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("#if(hsa==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='hsapiens_gene_ensembl')} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#     results <- getBM(attributes = c('hgnc_symbol','entrezgene'), mart=ensembl)}#to connect to a BioMart database",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(mmu==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='mmusculus_gene_ensembl')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#     results <- getBM(attributes = c('mgi_symbol','entrezgene') , mart=ensembl)} #to connect to a BioMart database",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(dme==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='dmelanogaster_gene_ensembl')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste(" #     results <- getBM(attributes = c('flybasename_gene','entrezgene'), mart=ensembl)} #to connect to a BioMart database",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(dre==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='drerio_gene_ensembl')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#     results <- getBM(attributes = c('zfin_symbol','entrezgene'), mart=ensembl)} #to connect to a BioMart database",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#file.name <- mol.sum(mol.data = file.name, id.map = results,sum.method = 'mean') }",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(conversion3==TRUE){ # gencode names to entrez genes",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("#if(hsa==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='hsapiens_gene_ensembl')} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(mmu==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='mmusculus_gene_ensembl')} #to connect to a BioMart database",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste("#if(dme==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='dmelanogaster_gene_ensembl')} #to connect to a BioMartdatabase",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#if(dre==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='drerio_gene_ensembl')} #to connect to a BioMart database",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#newrownames = strsplit(rownames(file.name),'\\.')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#nrownames = NULL",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#for (i in 1:length(newrownames) ){nrownames[i] = newrownames[[i]][1]}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#rownames(file.name) = nrownames",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#results <- getBM(attributes = c('ensembl_gene_id','entrezgene'), mart=ensembl)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#file.name <- mol.sum(mol.data = file.name, id.map = results,sum.method = 'mean')  }",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")


    message5 <- paste("file.name <- LoadCachedObject(db, 'filenameconverted_key')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print('Expression data modified:')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print(dim(file.name))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print(head(file.name))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("out.suffix=NULL",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("fc=NULL",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("exp.fc <- LoadCachedObject(db, 'expfc_key')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("out.suffix <- LoadCachedObject(db, 'outsuffix_key')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if (colnames(file.name)[1] == 'log2FoldChange') {fc=file.name[,'log2FoldChange']",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#names(fc)=rownames(file.name)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#sum(is.infinite(fc)) #to manage the problem of inf",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#fc[fc>10]=10",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#fc[fc< -10]=-10",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#out.suffix='deseq' }",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(colnames(file.name)[1] == 'logFC'){ fc=file.name[,'logFC']",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#names(fc)=rownames(file.name)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#out.suffix='edger'}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if(colnames(file.name)[3] == 'M'){ fc=file.name[,'M']",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#names(fc)=rownames(file.name)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#sum(is.infinite(fc)) #to manage the problem of inf",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#fc[fc>10]=10",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#fc[fc< -10]=-10",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#out.suffix='noiseq'}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#exp.fc=fc",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#if (geneSet1==TRUE){res <- gage(exp.fc, gsets = kegg.gs, ref = NULL, samp = NULL, same.dir = FALSE, saaTest = gs.zTest, use.fold = FALSE)",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#}else{res <- gage(exp.fc, gsets = go.gs  , ref = NULL, samp = NULL, same.dir = FALSE, saaTest = gs.zTest, use.fold = FALSE)}",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("res <- LoadCachedObject(db, 'res_key')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("print('First lines of results:')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print(head(res[[1]],10))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#sel <- res$greater[, 'q.val'] < 0.01 & !is.na(res$greater[, 'q.val'])",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#path.ids <- rownames(res$greater)[sel]",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#sel.l <- res$less[, 'q.val'] < 0.01 & !is.na(res$less[,'q.val'])",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#path.ids.l <- rownames(res$less)[sel.l]",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("#path.ids2 <- substr(c(path.ids, path.ids.l), 1, 8) #put greater and less together",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("path.ids2 <- LoadCachedObject(db, 'finalresults_key')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print('First lines of the results:')",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("print(head(path.ids2))",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("#pv.out.list <- sapply(path.ids2, function(pid){ pathview( gene.data = exp.fc, pathway.id = pid, species = specie, out.suffix=out.suffix)})", sep = "\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  }
  
  print('FINISHED!')
  
  res

}

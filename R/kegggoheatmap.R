kegggoheatmap_function <- function(re,geneSet1,geneSet2,Project,hsa,mmu,dme,dre,conversion,conversion3,num_of_path,countFile,control,treated){

#re = "/media/6b8a4404-f3e2-4fbc-9fc4-5a0ebc8d0635/Minchiotti/RNASeqGUI_Projects/pippo/Results/d110_d77_counts_UQUA.txt_results_DESeq_Pathway_result.txt"
#geneSet1=TRUE
#num_of_path=1
#countFile="/media/6b8a4404-f3e2-4fbc-9fc4-5a0ebc8d0635/Minchiotti/RNASeqGUI_Projects/min2/Results/d110_d77_counts_UQUA.txt"
#control=c(1,2)
#treated=c(1,2)

 require(biomaRt)
 require(gage)
 require(pathview)

 db <- InitDb(db.name = paste(geneSet1,geneSet2,'kegggoheatmap_db',sep="_"), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))

 SaveInCache(db, re, "res_key")
 SaveInCache(db, geneSet1, "geneset1_key")
 SaveInCache(db, geneSet2, "geneset2_key")
 SaveInCache(db, Project,  "project_key")
 SaveInCache(db, hsa, "hsa_key")
 SaveInCache(db, mmu, "mmu_key")
 SaveInCache(db, dme, "dme_key")
 SaveInCache(db, dre, "dre_key")
 SaveInCache(db, conversion, "conversion_key")
 SaveInCache(db, conversion3, "conversion3_key")
 SaveInCache(db, countFile,"countFile_key")

  res=read.table(re, row.names=1, header=TRUE, sep="\t")
  counts=read.table(countFile, row.names=1, header=TRUE)

  num_of_path = as.numeric(num_of_path)
  SaveInCache(db, num_of_path,"num_of_path_key")
  print("Paths loaded:")
  print(num_of_path)
  print("control:")
  control = as.numeric(control)
  print(control)
  print("treated:")
  treated = as.numeric(treated)
  print(treated)
  SaveInCache(db, control,"control_key")
  SaveInCache(db, treated,"treated_key")
  print("Pathway list selected:")
  print(head(res,10))
  print("Count file selected:")
  print(head(counts))
  print("Selecting a BioMart database and dataset. This step might take a while. PLEASE, WAIT!")

  if(conversion==TRUE){ #ensembl gene ids 
    if(hsa==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="hsapiens_gene_ensembl") #to connect to a BioMart database
         results <- getBM(attributes = c("ensembl_gene_id","hgnc_symbol"), mart=ensembl)
    }
    if(mmu==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="mmusculus_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","mgi_symbol"), mart=ensembl)
    } 
    if(dme==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="dmelanogaster_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","bdgp_symbol"), mart=ensembl)
    } #to connect to a BioMart database
    if(dre==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="drerio_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","zfin_symbol"), mart=ensembl)
    } #to connect to a BioMart database
    SaveInCache(db, ensembl,"ensembl_key")
    print("Conversion map:")
    print(head(results))
    message <- paste("Conversion from ENSEMBL gene ids to gene names in the COUNT file. PLEASE WAIT!", sep="")
    print(message)
    counts <- mol.sum(mol.data = counts, id.map = results,sum.method = "mean") #conversion
  }

  if(conversion3==TRUE){ # gencode names 
    print("Converting gene codes to ensembl gene ids")   
    newrownames = strsplit(rownames(counts),"\\.")
    nrownames = NULL
    for (i in 1:length(newrownames) ){
        nrownames[i] = newrownames[[i]][1]
    }
    rownames(counts) = nrownames
    print("genecode rewritten as follows:")
    print(head(counts))
    if(hsa==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="hsapiens_gene_ensembl") #to connect to a BioMart database
         results <- getBM(attributes = c("ensembl_gene_id","hgnc_symbol"), mart=ensembl)
    }
    if(mmu==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="mmusculus_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","mgi_symbol"), mart=ensembl)
    } 
    if(dme==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="dmelanogaster_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","bdgp_symbol"), mart=ensembl)
    } #to connect to a BioMart database
    if(dre==TRUE){
         ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset="drerio_gene_ensembl")
         results <- getBM(attributes = c("ensembl_gene_id","zfin_symbol"), mart=ensembl)
    } #to connect to a BioMart database

    print("Conversion map:")
    print(head(results))
    print("Conversion from ENSEMBL gene ids to gene names in the COUNT file. PLEASE WAIT!")
    counts <- mol.sum(mol.data = counts, id.map = results,sum.method = "mean") #conversion
  }

  SaveInCache(db, counts,"counts_key") 
  print("Count file converted:")
  print(head(counts))


  
  if(Sys.info()[[1]]=="Windows"){
    prevwd=getwd()
    setwd(paste(getwd(),"RNASeqGUI_Projects", Project,"Plots", sep="\\"))
    if (geneSet1 == TRUE){ #kegg
       if(hsa==TRUE){ks=kegg.gsets(species = "hsa", id.type = "entrez")}
       if(mmu==TRUE){ks=kegg.gsets(species = "mmu", id.type = "entrez")}
       if(dme==TRUE){ks=kegg.gsets(species = "dme", id.type = "entrez")}
       if(dre==TRUE){ks=kegg.gsets(species = "dre", id.type = "entrez")}
       kegg.gs=ks$kg.sets
       print("KEGG database loaded:")
       gs = rownames(res)[num_of_path]
       print("Path selected:")
       print(gs)
       outname = gsub(" |:|/", "_", substr(gs, 0, 100))
       if(hsa==TRUE){results1 <- getBM(attributes = c("entrezgene","hgnc_symbol"), mart=ensembl)}
       if(mmu==TRUE){results1 <- getBM(attributes = c("entrezgene","mgi_symbol"), mart=ensembl)}
       if(dme==TRUE){results1 <- getBM(attributes = c("entrezgene","bdgp_symbol"), mart=ensembl)}
       if(dre==TRUE){results1 <- getBM(attributes = c("entrezgene","zfin_symbol"), mart=ensembl)}         
       kegg.gs[[gs]] <- mol.sum(mol.data = kegg.gs[[gs]], id.map = results1, sum.method = "mean") 
       length(kegg.gs[[gs]])
       rownames(kegg.gs[[gs]])
       print("Producing the heatmap. If it returns an error please close the R console and restart RNASeqGUI.")
       gage::geneData(genes = rownames(kegg.gs[[gs]]), exprs = log(counts + 1), ref = control, samp = treated, outname = outname, txt = TRUE, heatmap = TRUE, limit = 3, scatterplot = TRUE, dendrogram="none")

    }
    if (geneSet2 == TRUE){ #GO
       data(bods)
       if(hsa==TRUE){row=subset(bods, bods[,3] == "hsa")}
       if(mmu==TRUE){row=subset(bods, bods[,3] == "mmu")}
       if(dme==TRUE){row=subset(bods, bods[,3] == "dme")}
       if(dre==TRUE){row=subset(bods, bods[,3] == "dre")}
       go=go.gsets(species = row[2], id.type = "entrez")
       go.gs=go$go.sets
       print("GO database loaded:")
       gs = rownames(res)[num_of_path]
       print("Path selected:")
       print(gs)
       outname = gsub(" |:|/", "_", substr(gs, 12, 100))
       if(hsa==TRUE){results1 <- getBM(attributes = c("entrezgene","hgnc_symbol"), mart=ensembl)}
       if(mmu==TRUE){results1 <- getBM(attributes = c("entrezgene","mgi_symbol"), mart=ensembl)}  
       if(dme==TRUE){results1 <- getBM(attributes = c("entrezgene","bdgp_symbol"), mart=ensembl)}
       if(dre==TRUE){results1 <- getBM(attributes = c("entrezgene","zfin_symbol"), mart=ensembl)}  
       go.gs[[gs]] <- mol.sum(mol.data = go.gs[[gs]], id.map = results1, sum.method = "mean")
       length(go.gs[[gs]])
       rownames(go.gs[[gs]])
       print("Producing the heatmap. If it returns an error please close the R console and restart RNASeqGUI.")
       gage::geneData(genes = rownames(go.gs[[gs]]), exprs = log(counts + 1), ref = control, samp = treated, outname = outname, txt = TRUE, heatmap = TRUE, limit = 3, scatterplot = TRUE, dendrogram="none")

    }
    setwd(prevwd)
    message=paste("The Heatmap has been saved in RNASeqGUI_Projects\\", Project,"\\Plots folder!", sep="")
    print(message)
  
    #write into the project report
    report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    
    message2 <- paste(" * In the *Gene-Set\\Pathway Interface*, you clicked the **GAGE** button at `", Sys.time(),"` and the GAGE_result.txt file has been saved in the `", Project,"\\Results` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    #write into the project report
    report=paste("RNASeqGUI_Projects\\",Project,"\\Logs\\report.Rmd",sep="")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message2 <- paste(" * In the *Gene-Set/Pathway Interface*, you clicked the **Heatmap** button at `", Sys.time(),"` and the GAGE_result.txt file has been saved in the `", Project,"\\Results` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "geneSet1 <- ",geneSet1,"", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "geneSet2 <-  ",geneSet2,"", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("db <- InitDb(db.name = paste(geneSet1,geneSet2,'kegggoheatmap_db',sep='_'), db.path=file.path('cache'))", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "res <- LoadCachedObject(db, 'res_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "Project <- LoadCachedObject(db, 'project_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "hsa <- LoadCachedObject(db, 'hsa_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "mmu <- LoadCachedObject(db, 'mmu_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "dme <- LoadCachedObject(db, 'dme_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "dre <- LoadCachedObject(db, 'dre_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "conversion <- LoadCachedObject(db, 'conversion_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "num_of_path <- LoadCachedObject(db, 'num_of_path_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "countFile <- LoadCachedObject(db, 'countFile_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "control <- LoadCachedObject(db, 'control_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "treated <- LoadCachedObject(db, 'treated_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "res=read.table(res, row.names=1, header=TRUE, sep='')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "counts=read.table(countFile, row.names=1, header=TRUE)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "print('Pathway list selected:')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "print(head(res,10))", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "print('Count file selected:')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "print(head(counts))", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n") 
    message5 <- paste( "#if(conversion==TRUE){ ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(hsa==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='hsapiens_gene_ensembl') ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','hgnc_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(mmu==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='mmusculus_gene_ensembl')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','mgi_symbol'), mart=ensembl)} ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(dme==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='dmelanogaster_gene_ensembl')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','bdgp_symbol'), mart=ensembl)} ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(dre==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='drerio_gene_ensembl')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','zfin_symbol'), mart=ensembl)} ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#counts <- mol.sum(mol.data = counts, id.map = results,sum.method = 'mean') }", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#if(conversion3==TRUE){newrownames = strsplit(rownames(counts),'\\.')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  nrownames = NULL", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  for (i in 1:length(newrownames) ){ nrownames[i] = newrownames[[i]][1]}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  rownames(counts) = nrownames", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(hsa==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='hsapiens_gene_ensembl') ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','hgnc_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(mmu==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='mmusculus_gene_ensembl')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','mgi_symbol'), mart=ensembl)} ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(dme==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='dmelanogaster_gene_ensembl')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','bdgp_symbol'), mart=ensembl) }", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(dre==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='drerio_gene_ensembl')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','zfin_symbol'), mart=ensembl)} ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#counts <- mol.sum(mol.data = counts, id.map = results,sum.method = 'mean')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "counts <- LoadCachedObject(db, 'counts_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if (geneSet1 == TRUE){ #kegg", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(hsa==TRUE){ks=kegg.gsets(species = 'hsa', id.type = 'entrez')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(mmu==TRUE){ks=kegg.gsets(species = 'mmu', id.type = 'entrez')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dme==TRUE){ks=kegg.gsets(species = 'dme', id.type = 'entrez')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dre==TRUE){ks=kegg.gsets(species = 'dre', id.type = 'entrez')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "kegg.gs=ks$kg.sets", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "gs = rownames(res)[num_of_path]", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "print('Path selected:')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "print(gs)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "outname = gsub(' |:|/', '_', substr(gs, 0, 100))", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(hsa==TRUE){results1 <- getBM(attributes = c('entrezgene','hgnc_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(mmu==TRUE){results1 <- getBM(attributes = c('entrezgene','mgi_symbol') , mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dme==TRUE){results1 <- getBM(attributes = c('entrezgene','bdgp_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dre==TRUE){results1 <- getBM(attributes = c('entrezgene','zfin_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "kegg.gs[[gs]] <- mol.sum(mol.data = kegg.gs[[gs]], id.map = results1, sum.method = 'mean')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#gage::geneData(genes=rownames(kegg.gs[[gs]]),exprs=log(counts+1),ref=control,samp=treated,", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste( "outname=outname,txt=T,heatmap=T,limit=3,scatterplot=T)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste( "}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if (geneSet2 == TRUE){ #GO", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "data(bods)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(hsa==TRUE){row=subset(bods, bods[,3] == 'hsa')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(mmu==TRUE){row=subset(bods, bods[,3] == 'mmu')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dme==TRUE){row=subset(bods, bods[,3] == 'dme')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dre==TRUE){row=subset(bods, bods[,3] == 'dre')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "go=go.gsets(species = row[2], id.type = 'entrez')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "go.gs=go$go.sets", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "gs = rownames(res)[num_of_path]", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "outname = gsub(' |:|/', '_', substr(gs, 12, 100))", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(hsa==TRUE){results1 <- getBM(attributes = c('entrezgene','hgnc_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(mmu==TRUE){results1 <- getBM(attributes = c('entrezgene','mgi_symbol') , mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dme==TRUE){results1 <- getBM(attributes = c('entrezgene','bdgp_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dre==TRUE){results1 <- getBM(attributes = c('entrezgene','zfin_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "go.gs[[gs]] <- mol.sum(mol.data = go.gs[[gs]], id.map = results1, sum.method = 'mean')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#gage::geneData(genes = rownames(go.gs[[gs]]), exprs = log(counts + 1), ref = control, samp = treated, outname = outname, txt = TRUE, heatmap = TRUE, limit = 3, scatterplot = TRUE)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  }else{         # Linux
    prevwd=getwd()
    plot.path <- file.path(getwd(),"RNASeqGUI_Projects", Project,"Plots")
    #print(plot.path)
    setwd(plot.path)
    print("Loading kegg database. PLEASE, WAIT!")
    if (geneSet1 == TRUE){ #kegg
       if(hsa==TRUE){ks=kegg.gsets(species = "hsa", id.type = "entrez")}
       if(mmu==TRUE){ks=kegg.gsets(species = "mmu", id.type = "entrez")}
       if(dme==TRUE){ks=kegg.gsets(species = "dme", id.type = "entrez")}
       if(dre==TRUE){ks=kegg.gsets(species = "dre", id.type = "entrez")}
       kegg.gs=ks$kg.sets
       print("KEGG database loaded:")
       gs = rownames(res)[num_of_path]
       print("Path selected:")
       print(gs)
       print("    ")
       print("Genes in the path:")
       print((kegg.gs[[gs]]))
       outname = gsub(" |:|/", "_", substr(gs, 0, 100))
       results1 = NULL
       if(hsa==TRUE){results1 <- getBM(attributes = c("entrezgene","hgnc_symbol"), mart=ensembl)}#select the map
       if(mmu==TRUE){results1 <- getBM(attributes = c("entrezgene","mgi_symbol"), mart=ensembl)}  
       if(dme==TRUE){results1 <- getBM(attributes = c("entrezgene","bdgp_symbol"), mart=ensembl)}
       if(dre==TRUE){results1 <- getBM(attributes = c("entrezgene","zfin_symbol"), mart=ensembl)}  
       print("Conversion of gene names. PLEASE WAIT!")
       #kegg.gs[[gs]] <- mol.sum(mol.data = kegg.gs[[gs]], id.map = results1, sum.method = "mean")
       #Qui sopra non mi effettua la conversione!

id=NULL
 for(i in 1:length(kegg.gs[[gs]])){
   if ( is.element(kegg.gs[[gs]][i], results1[,1])==TRUE ) {
      sub_results1 = subset( results1, results1[,1] == kegg.gs[[gs]][i] )
      if (sub_results1[,2] != ""){
         id[i] = sub_results1[,2]
      }else{ id[i] = kegg.gs[[gs]][i] }
   }else{

      id[i] = kegg.gs[[gs]][i]
   }
 }

       print("Names of the genes in the path:")
       print(length(id))
       print(id)
       print("Producing the heatmap. If it returns an error please close the R console and restart RNASeqGUI.")
       gage::geneData(genes = id, exprs = log(counts + 1), ref = control, samp = treated, outname = outname, txt = TRUE, heatmap = TRUE, 
                      cexRow=1.5, margins = c(10, 10), limit = 3, scatterplot = TRUE, dendrogram="none", 
                      pdf.size = c(21,30), key=FALSE)

       write.table(id, file = "genes_in_the_path.txt" , quote=FALSE, sep="\t", row.names=TRUE)

    }
    if (geneSet2 == TRUE){ #GO
       data(bods)
       if(hsa==TRUE){row=subset(bods, bods[,3] == "hsa")}
       if(mmu==TRUE){row=subset(bods, bods[,3] == "mmu")}
       if(dme==TRUE){row=subset(bods, bods[,3] == "dme")}
       if(dre==TRUE){row=subset(bods, bods[,3] == "dre")}
       go=go.gsets(species = row[2], id.type = "entrez")
       go.gs=go$go.sets
       print("GO database loaded:")
       print(head(go.gs[1]))
       gs = rownames(res)[num_of_path]
       print("Path selected:")
       print(gs)
       outname = gsub(" |:|/", "_", substr(gs, 12, 100))
       results1 = NULL
       if(hsa==TRUE){results1 <- getBM(attributes = c("entrezgene","hgnc_symbol"), mart=ensembl)}
       if(mmu==TRUE){results1 <- getBM(attributes = c("entrezgene","mgi_symbol"), mart=ensembl)}  
       if(dme==TRUE){results1 <- getBM(attributes = c("entrezgene","bdgp_symbol"), mart=ensembl)}
       if(dre==TRUE){results1 <- getBM(attributes = c("entrezgene","zfin_symbol"), mart=ensembl)}  
       go.gs[[gs]] <- mol.sum(mol.data = go.gs[[gs]], id.map = results1, sum.method = "mean")
       length(go.gs[[gs]])
       rownames(go.gs[[gs]])
       print("Producing the heatmap. If it returns an error please close the R console and restart RNASeqGUI.")
       gage::geneData(genes = rownames(go.gs[[gs]]), exprs = log(counts + 1), ref = control, samp = treated, outname = outname, txt = TRUE, heatmap = TRUE, limit = 3, scatterplot = TRUE)

    }
    setwd(prevwd)

    message=paste("The Heatmap has been saved in RNASeqGUI_Projects/", Project,"/Plots folder!", sep="")
    print(message)
    
    #write into the project report
    report=paste("RNASeqGUI_Projects/",Project,"/Logs/report.Rmd",sep="")
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message2 <- paste(" * In the *Gene-Set/Pathway Interface*, you clicked the **Heatmap** button at `", Sys.time(),"` and the GAGE_result.txt file has been saved in the `", Project,"/Results` folder.", sep="")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message2 <- paste("You chose the following Pathway/GO file:`",re,"`,geneSet1:`",geneSet1,"`,geneSet2:`",geneSet2,"`,Project:`",Project,"`,hsa:`",hsa,"`,mmu:`",mmu,"`,dme:`",dme,"`,dre:`",dme,"`,conversion:`",conversion,"`, num_of_path:`",num_of_path,"`,countFile:`",countFile, "` ", sep="\n")
    write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")  
 
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" This R code has been run:",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste("  ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")   
    message5 <- paste(" ```{r} ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "geneSet1 <- ",geneSet1,"", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "geneSet2 <-  ",geneSet2,"", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste("db <- InitDb(db.name = paste(geneSet1,geneSet2,'kegggoheatmap_db',sep='_'), db.path=file.path('cache'))", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "res <- LoadCachedObject(db, 'res_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "Project <- LoadCachedObject(db, 'project_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "hsa <- LoadCachedObject(db, 'hsa_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "mmu <- LoadCachedObject(db, 'mmu_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "dme <- LoadCachedObject(db, 'dme_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "dre <- LoadCachedObject(db, 'dre_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "conversion <- LoadCachedObject(db, 'conversion_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "num_of_path <- LoadCachedObject(db, 'num_of_path_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "countFile <- LoadCachedObject(db, 'countFile_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "control <- LoadCachedObject(db, 'control_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "treated <- LoadCachedObject(db, 'treated_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "res=read.table(res, row.names=1, header=TRUE, sep='')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "counts=read.table(countFile, row.names=1, header=TRUE)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "print('Pathway list selected:')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "print(head(res,10))", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "print('Count file selected:')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "print(head(counts))", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n") 
    message5 <- paste( "#if(conversion==TRUE){ ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(hsa==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='hsapiens_gene_ensembl') ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','hgnc_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(mmu==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='mmusculus_gene_ensembl')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','mgi_symbol'), mart=ensembl)} ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(dme==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='dmelanogaster_gene_ensembl')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','bdgp_symbol'), mart=ensembl)} ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(dre==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='drerio_gene_ensembl')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','zfin_symbol'), mart=ensembl)} ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#counts <- mol.sum(mol.data = counts, id.map = results,sum.method = 'mean') }", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#if(conversion3==TRUE){newrownames = strsplit(rownames(counts),'\\.')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  nrownames = NULL", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  for (i in 1:length(newrownames) ){ nrownames[i] = newrownames[[i]][1]}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  rownames(counts) = nrownames", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(hsa==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='hsapiens_gene_ensembl') ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','hgnc_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(mmu==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='mmusculus_gene_ensembl')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','mgi_symbol'), mart=ensembl)} ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(dme==TRUE){ ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='dmelanogaster_gene_ensembl')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','bdgp_symbol'), mart=ensembl) }", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#  if(dre==TRUE){ensembl=useMart('ENSEMBL_MART_ENSEMBL', host='www.ensembl.org', dataset='drerio_gene_ensembl')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#     results <- getBM(attributes = c('ensembl_gene_id','zfin_symbol'), mart=ensembl)} ", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#counts <- mol.sum(mol.data = counts, id.map = results,sum.method = 'mean')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "counts <- LoadCachedObject(db, 'counts_key')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if (geneSet1 == TRUE){ #kegg", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(hsa==TRUE){ks=kegg.gsets(species = 'hsa', id.type = 'entrez')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(mmu==TRUE){ks=kegg.gsets(species = 'mmu', id.type = 'entrez')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dme==TRUE){ks=kegg.gsets(species = 'dme', id.type = 'entrez')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dre==TRUE){ks=kegg.gsets(species = 'dre', id.type = 'entrez')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "kegg.gs=ks$kg.sets", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "gs = rownames(res)[num_of_path]", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "print('Path selected:')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "print(gs)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "outname = gsub(' |:|/', '_', substr(gs, 0, 100))", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(hsa==TRUE){results1 <- getBM(attributes = c('entrezgene','hgnc_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(mmu==TRUE){results1 <- getBM(attributes = c('entrezgene','mgi_symbol') , mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dme==TRUE){results1 <- getBM(attributes = c('entrezgene','bdgp_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dre==TRUE){results1 <- getBM(attributes = c('entrezgene','zfin_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "kegg.gs[[gs]] <- mol.sum(mol.data = kegg.gs[[gs]], id.map = results1, sum.method = 'mean')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#gage::geneData(genes=rownames(kegg.gs[[gs]]),exprs=log(counts+1),ref=control,samp=treated,", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#outname=outname,txt=T,heatmap=T,limit=3,scatterplot=T)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if (geneSet2 == TRUE){ #GO", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "data(bods)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(hsa==TRUE){row=subset(bods, bods[,3] == 'hsa')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(mmu==TRUE){row=subset(bods, bods[,3] == 'mmu')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dme==TRUE){row=subset(bods, bods[,3] == 'dme')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dre==TRUE){row=subset(bods, bods[,3] == 'dre')}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "go=go.gsets(species = row[2], id.type = 'entrez')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "go.gs=go$go.sets", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "gs = rownames(res)[num_of_path]", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "outname = gsub(' |:|/', '_', substr(gs, 12, 100))", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(hsa==TRUE){results1 <- getBM(attributes = c('entrezgene','hgnc_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(mmu==TRUE){results1 <- getBM(attributes = c('entrezgene','mgi_symbol') , mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dme==TRUE){results1 <- getBM(attributes = c('entrezgene','bdgp_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "if(dre==TRUE){results1 <- getBM(attributes = c('entrezgene','zfin_symbol'), mart=ensembl)}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "go.gs[[gs]] <- mol.sum(mol.data = go.gs[[gs]], id.map = results1, sum.method = 'mean')", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message5 <- paste( "#gage::geneData(genes=rownames(go.gs[[gs]]),exprs=log(counts+1),ref=control,samp=treated,", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste( "#outname=outname,txt=T,heatmap=T,limit=3,scatterplot=T)", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste( "}", sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
    message8 <- paste(" ``` ",sep="\n")
    write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")    
    message5 <- paste(" _______________________________________________________________________ ",sep="\n")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  }
  
  print('FINISHED!')
  
  res

}

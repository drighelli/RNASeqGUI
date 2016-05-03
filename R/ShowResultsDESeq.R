DESeqShowresults <- function(Project){

  require(ReportingTools)
  library(hwriter)
  res <- NULL

  if(Sys.info()[[1]]=="Windows"){

      folder = paste("RNASeqGUI_Projects\\",Project,"\\Results",sep="")
      result_file <- list.files(folder,pattern="_results_DESeq.txt$",full.names=TRUE)
      htmlRep <- HTMLReport(shortName = result_file)
      res.table <- read.table(paste(".\\",result_file,sep=""),header=TRUE,row.names=NULL)
      res.table$ncbi    <- hwrite(as.character(res.table$id), link = paste("http://www.ncbi.nlm.nih.gov/gene/?term=", as.character(res.table$id), sep =''), table=FALSE)
      res.table$ensembl <- hwrite(as.character(res.table$id), link = paste("http://ensembl.org/Gene/Summary?g=", as.character(res.table$id), sep =''), table=FALSE)
      publish(res.table, htmlRep)
      finish(htmlRep)
      browseURL(paste(".\\",result_file,".html",sep=""))           		
      res

	 }else{ #Linux

      #  http://ensembl.org/Gene/Summary?g=FBgn0000018
      #  http://www.ncbi.nlm.nih.gov/gene/?term=FBgn0000018

      folder = paste("RNASeqGUI_Projects/",Project,"/Results",sep="")
      result_file <- list.files(folder,pattern="_results_DESeq.txt$",full.names=TRUE)
      htmlRep <- HTMLReport(shortName = result_file)
      res.table <- read.table(paste("./",result_file,sep=""),header=TRUE,row.names=NULL)
      res.table$ncbi    <- hwrite(as.character(res.table$id), link = paste("http://www.ncbi.nlm.nih.gov/gene/?term=", as.character(res.table$id), sep =''), table=FALSE)
      res.table$ensembl <- hwrite(as.character(res.table$id), link = paste("http://ensembl.org/Gene/Summary?g=", as.character(res.table$id), sep =''), table=FALSE)
      publish(res.table, htmlRep)
      finish(htmlRep)
      browseURL(paste("./",result_file,".html",sep=""))           		
      res

  }

}

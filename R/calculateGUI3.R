calculateGUI3 <-
function(project_name) {

 Project <- project_name
 message=paste("You are using the project: ",Project,sep="")
 print(message)


	openFile <- function(button,user.data) {
		dialog <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
		dialog$setCurrentFolder(paste("~/RNASeqGUI_Projects/",Project,"/Results/",sep=""))
		if (dialog$run() == GtkResponseType["accept"]) {
			the.sel.file <- dialog$getFilename()
			filename$setText(the.sel.file)
			dialog$destroy()
		} else {
			dialog$destroy()
		}
	}

 noifun <- function(button, user.data) { 
 # Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_NoiSeq Analysis")
	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

	# button
	buttonNOISeq <- gtkButtonNewWithMnemonic("Plot FC", show = TRUE)
	gSignalConnect(buttonNOISeq, "clicked", performStatistics_plotFC_NoiSeq)
	the.buttons$packStart(buttonNOISeq,fill=FALSE)

 # button
	ProbHist <- gtkButtonNewWithMnemonic("Prob Hist", show = TRUE)
	gSignalConnect(ProbHist, "clicked", performStatistics_probHist)
	the.buttons$packStart(ProbHist,fill=FALSE)

 # button
	buttonVolcanoNoiSeq <- gtkButtonNewWithMnemonic("Volcano Plot", show = TRUE)
	gSignalConnect(buttonVolcanoNoiSeq, "clicked", performStatistics_Volcano_NoiSeq)
	the.buttons$packStart(buttonVolcanoNoiSeq,fill=FALSE)
 }



 deseqfun <- function(button, user.data) {

	# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_DESeq Analysis")
	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

 # button
	plotFC <- gtkButtonNewWithMnemonic("Plot FC", show = TRUE)
	gSignalConnect(plotFC, "clicked", performStatistics_plotFC)
	the.buttons$packStart(plotFC,fill=FALSE)

 # button
	Pvalues <- gtkButtonNewWithMnemonic("P-value Hist", show = TRUE)
	gSignalConnect(Pvalues, "clicked", performStatistics_Pvalues)
	the.buttons$packStart(Pvalues,fill=FALSE)

 # button
	buttonVolcanoDESeq <- gtkButtonNewWithMnemonic("Volcano Plot", show = TRUE)
	gSignalConnect(buttonVolcanoDESeq, "clicked", performStatistics_Volcano_DESeq)
	the.buttons$packStart(buttonVolcanoDESeq,fill=FALSE)
 }


 deseqComplexDesignfun <- function(button, user.data) {

	# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_DESeq Complex Design")
	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

 # button
	Pvalues <- gtkButtonNewWithMnemonic("P-value Hist", show = TRUE)
	gSignalConnect(Pvalues, "clicked", performStatistics_Pvalues)
	the.buttons$packStart(Pvalues,fill=FALSE)

 }

deseq2fun <- function(button, user.data) {

	# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_DESeq2 Analysis")
	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)


 # button
	plotFC2 <- gtkButtonNewWithMnemonic("Plot FC", show = TRUE)
	gSignalConnect(plotFC2, "clicked", performStatistics_plotFC2)
	the.buttons$packStart(plotFC2,fill=FALSE)

 # button
	Pvalues2 <- gtkButtonNewWithMnemonic("P-value Hist", show = TRUE)
	gSignalConnect(Pvalues2, "clicked", performStatistics_Pvalues2)
	the.buttons$packStart(Pvalues2,fill=FALSE)

 # button
	buttonVolcanoDESeq2 <- gtkButtonNewWithMnemonic("Volcano Plot", show = TRUE)
	gSignalConnect(buttonVolcanoDESeq2, "clicked", performStatistics_Volcano_DESeq2)
	the.buttons$packStart(buttonVolcanoDESeq2,fill=FALSE)
 }


deseq2ComplexDesignfun <- function(button, user.data) {

	# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_DESeq2 Complex Design ")
	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

 # button
	Pvalues2 <- gtkButtonNewWithMnemonic("P-value Hist", show = TRUE)
	gSignalConnect(Pvalues2, "clicked", performStatistics_Pvalues2ComplexDesign)
	the.buttons$packStart(Pvalues2,fill=FALSE)

 }



 bayseqfun <- function(button, user.data) {

	# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_BaySeq Analysis")
	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

	# button
	buttonBaySeq <- gtkButtonNewWithMnemonic("FDR Hist", show = TRUE)
	gSignalConnect(buttonBaySeq, "clicked", performStatistics_FDRHistBaySeq)
	the.buttons$packStart(buttonBaySeq,fill=FALSE)

	# button
	buttonLhist <- gtkButtonNewWithMnemonic("Likelihood Hist", show = TRUE)
	gSignalConnect(buttonLhist, "clicked", performStatistics_LikelihoodHistBaySeq)
	the.buttons$packStart(buttonLhist,fill=FALSE)
 }

 edgeRfun <- function(button, user.data) {

	# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_EdgeR Exact Test Analysis")
	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

 # button
	plotFC3 <- gtkButtonNewWithMnemonic("Plot FC", show = TRUE)
	gSignalConnect(plotFC3, "clicked", performStatistics_plotFCEdgeR)
	the.buttons$packStart(plotFC3,fill=FALSE)

 # button
	FDRhist <- gtkButtonNewWithMnemonic("FDR Hist", show = TRUE)
	gSignalConnect(FDRhist, "clicked", performStatistics_FDRhist)
	the.buttons$packStart(FDRhist,fill=FALSE)

 # button
	buttonVolcano <- gtkButtonNewWithMnemonic("Volcano Plot", show = TRUE)
	gSignalConnect(buttonVolcano, "clicked", performStatistics_Volcano_EdgeR)
	the.buttons$packStart(buttonVolcano,fill=FALSE)
 }

 edgeRComplexDesignfun <- function(button, user.data) {

	# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_EdgeR GLM for Multi Factor")
	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

 # button
	plotFC3 <- gtkButtonNewWithMnemonic("Plot FC", show = TRUE)
	gSignalConnect(plotFC3, "clicked", performStatistics_plotFCEdgeR)
	the.buttons$packStart(plotFC3,fill=FALSE)

 # button
	FDRhist <- gtkButtonNewWithMnemonic("FDR Hist", show = TRUE)
	gSignalConnect(FDRhist, "clicked", performStatistics_FDRhist)
	the.buttons$packStart(FDRhist,fill=FALSE)

 # button
	buttonVolcano <- gtkButtonNewWithMnemonic("Volcano Plot", show = TRUE)
	gSignalConnect(buttonVolcano, "clicked", performStatistics_Volcano_EdgeR)
	the.buttons$packStart(buttonVolcano,fill=FALSE)
 }


performStatistics_plotFC <- function(button, user.data) {
		res <- NULL
		# Get the information about data and the file
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
  d <- read.table(the.file,sep=the.sep,header=the.headers, row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = PlotFC(d,the.file, Project)                
  }
 rm(list = ls())
	} #End of function

performStatistics_plotFC2 <- function(button, user.data) {
		res <- NULL
		# Get the information about data and the file
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
  d <- read.table(the.file,sep=the.sep,header=the.headers, row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = PlotFC2(d,the.file, Project)                   
  }
 rm(list = ls())
	} #End of function

performStatistics_plotFC_NoiSeq <- function(button, user.data) {
		res <- NULL
		# Get the information about data and the file
  the.gene <- gene$getText()
  the.prob  <- prob$getText()
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
  d <- read.table(the.file,sep=the.sep,header=the.headers )
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = plotFC_NoiSeq(the.gene,the.prob,d,the.file, Project)                  
  }
 rm(list = ls())
	} #End of function

performStatistics_plotFCEdgeR <- function(button, user.data) {
		res <- NULL
		# Get the information about data and the file
  the.gene <- gene$getText()
  the.fdr  <- fdr$getText()
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers, row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = PlotFC3(the.gene,the.fdr,d,the.file, Project)                 
  }
 rm(list = ls())
	} #End of function


performStatistics_Pvalues <- function(button, user.data) {
		res <- NULL
		# Get the information about data and the file
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
  d <- read.table(the.file,sep=the.sep,header=the.headers, row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = Pvalues(d,the.file, Project)                 
  }
 rm(list = ls())              
	} #End of function


performStatistics_Pvalues2 <- function(button, user.data) {
		res <- NULL
		# Get the information about data and the file
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
  d <- read.table(the.file,sep=the.sep,header=the.headers, row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = Pvalues2(d,the.file, Project)                  
  }
 rm(list = ls())              
	} #End of function

performStatistics_Pvalues2ComplexDesign <- function(button, user.data) {
		res <- NULL
		# Get the information about data and the file
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
  d <- read.table(the.file,sep=the.sep,header=the.headers, row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = Pvalues2ComplexDesign(d,the.file, Project)                  
  }
 rm(list = ls())              
	} #End of function

performStatistics_FDRhist <- function(button, user.data) {
		res <- NULL
		# Get the information about data and the file
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
  d <- read.table(the.file,sep=the.sep,header=the.headers, row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = FDRhist(d,the.file, Project)                   
  }
 rm(list = ls())              
	} #End of function

performStatistics_FDRHistBaySeq <- function(button, user.data) {
		res <- NULL
		# Get the information about data and the file
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
  d <- read.table(the.file,sep=the.sep,header=the.headers, row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = FDRhistBaySeq(d,the.file, Project)                  
  }
 rm(list = ls())              
	} #End of function

performStatistics_probHist <- function(button, user.data) {
		res <- NULL
		# Get the information about data and the file
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
  d <- read.table(the.file,sep=the.sep,header=the.headers, row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = probHist(d,the.file, Project)                 
  }
 rm(list = ls())              
	} #End of function

performStatistics_LikelihoodHistBaySeq <- function(button, user.data) {
	res <- NULL
		# Get the information about data and the file
  the.gene <- gene$getText()
  the.padj  <- padj$getText()
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers, row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = LikelihoodHistBaySeq(d,the.file, Project)                   
  }
 rm(list = ls())
	} #End of function

performStatistics_Volcano_NoiSeq <- function(button, user.data) {
		res <- NULL
		# Get the information about data and the file
  the.gene <- gene$getText()
  the.prob  <- prob$getText()
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = VolcanoNoiSeq(the.gene,the.prob,d,the.file, Project)                   
  }
 rm(list = ls())
	} #End of function

performStatistics_Volcano_EdgeR <- function(button, user.data) {
		res <- NULL
		# Get the information about data and the file
  the.gene <- gene$getText()
  the.fdr  <- fdr$getText()
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers, row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = VolcanoEdgeR(the.gene,the.fdr,d,the.file, Project)                   
  }
 rm(list = ls())
	} #End of function


performStatistics_Volcano_DESeq <- function(button, user.data) {
		res <- NULL
		# Get the information about data and the file
  the.gene <- gene$getText()
  the.padj  <- padj$getText()
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers, row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = VolcanoDESeq(the.gene,the.padj,d,the.file, Project)                   
  }
 rm(list = ls())
	} #End of function

performStatistics_Volcano_DESeq2 <- function(button, user.data) {
	res <- NULL
		# Get the information about data and the file
  the.gene <- gene$getText()
  the.padj  <- padj$getText()
  the.file <- filename$getText()
		the.sep  <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers, row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = VolcanoDESeq2(the.gene,the.padj,d,the.file, Project)                   
  }
 rm(list = ls())
	} #End of function

infoFun <- function(button, user.data) {
			      ########## Start dialog...
			      # Open a dialog box to print results
         message=paste("Vignette",sep="")
			      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
        	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		      	# Create vertical container for file name entry
	    	  	vbox <- gtkVBoxNew(FALSE, 8)
		      	vbox$setBorderWidth(24)
	    	  	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
   label1 <- gtkLabelNewWithMnemonic("Select a result file by clicking on the Open button.")
   label2 <- gtkLabelNewWithMnemonic("Click the button of the method that produced the file you have just selected.")
   label3 <- gtkLabelNewWithMnemonic("If you want to diplay a particular gene, write the corresponding gene id in the Gene Id? field")
   label4 <- gtkLabelNewWithMnemonic("Choose an FDR (for EdgeR and BaySeq), a Padj (for DESeq and DESeq2) or a Prob (for NoiSeq) level to select the genes you want to display in the plot")
   label5 <- gtkLabelNewWithMnemonic("Finally, click the button of the plot you want to generate. ")
   label6 <- gtkLabelNewWithMnemonic("For further information, see http://bioinfo.na.iac.cnr.it/RNASeqGUI/manual.pdf")

         vbox$packStart(label1,FALSE,FALSE,0)
         vbox$packStart(label2,FALSE,FALSE,0)
        	vbox$packStart(label3,FALSE,FALSE,0)
         vbox$packStart(label4,FALSE,FALSE,0)
        	vbox$packStart(label5,FALSE,FALSE,0)
         vbox$packStart(label6,FALSE,FALSE,0)
		       response <- dialog$run()
		       response <- dialog$run()
		      	# Return to previous window
			      if (GtkResponseType["ok"]) { dialog$destroy() }			 
}


############################################################################################

		
	# Create window
	window <- gtkWindow()
	# Add title
	window["title"] <- "RNASeqGUI"

	# Add a frame
 title=paste("   \n    Result Inspection Interface is ready to work on ",Project, " project.", sep="")
	frame <- gtkFrameNew(title)
	window$add(frame)

	# Create vertical container for file name entry
	vbox <- gtkVBoxNew(FALSE, 8)
	vbox$setBorderWidth(24)
	frame$add(vbox)

#########################
# Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add label in first column
	label <- gtkLabelNewWithMnemonic("_Select a result file")
	hbox$packStart(label,FALSE,FALSE,0)
	# Add entry in the second column; named "filename"
	filename <- gtkEntryNew()
	filename$setWidthChars(50)
	label$setMnemonicWidget(filename)
	hbox$packStart(filename,FALSE,FALSE,0)
	buttonOpen <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen, "clicked", openFile)
	hbox$packStart(buttonOpen,FALSE,FALSE,0)

	# are headers included in the file?
	label <- gtkLabelNewWithMnemonic("_Headers?")
	hbox$packStart(label,FALSE,FALSE,0)
	headersEntry <- gtkCheckButton()
	headersEntry$active <- TRUE
	hbox$packStart(headersEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(headersEntry)
	
	# what's the character used for column separator?
	label <- gtkLabelNewWithMnemonic("_Column Separator?")
	hbox$packStart(label,FALSE,FALSE,0)
	sepEntry <- gtkEntryNew()
	sepEntry$setWidthChars(2)
	sepEntry$setText("")
	hbox$packStart(sepEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(sepEntry)

	# Add one horizontal container
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)
	
 # Gene Id
 label <- gtkLabelNewWithMnemonic("_Gene Id?")
	hbox$packStart(label,FALSE,FALSE,0)
	gene <- gtkEntryNew()
	gene$setWidthChars(16)
	gene$setText("")
	hbox$packStart(gene,FALSE,FALSE,0)
	label$setMnemonicWidget(gene)

 # FDR
 label <- gtkLabelNewWithMnemonic("_FDR?")
	hbox$packStart(label,FALSE,FALSE,0)
	fdr <- gtkEntryNew()
	fdr$setWidthChars(6)
	fdr$setText("0.05")
	hbox$packStart(fdr,FALSE,FALSE,0)
	label$setMnemonicWidget(fdr)

 # Padj
 label <- gtkLabelNewWithMnemonic("_Padj?")
	hbox$packStart(label,FALSE,FALSE,0)
	padj <- gtkEntryNew()
	padj$setWidthChars(6)
	padj$setText("0.05")
	hbox$packStart(padj,FALSE,FALSE,0)
	label$setMnemonicWidget(padj)

 # Prob
 label <- gtkLabelNewWithMnemonic("_Prob?")
	hbox$packStart(label,FALSE,FALSE,0)
	prob <- gtkEntryNew()
	prob$setWidthChars(6)
	prob$setText("0.7")
	hbox$packStart(prob,FALSE,FALSE,0)
	label$setMnemonicWidget(prob)

####################################################################################################################################
####################################################################################################################################

	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

 # button
	noi <- gtkButtonNewWithMnemonic("How to use this Interface", show = TRUE)
	gSignalConnect(noi, "clicked", infoFun)
	the.buttons$packStart(noi,fill=FALSE)

 # Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

 # button
	edge <- gtkButtonNewWithMnemonic("_EdgeR Exact Test", show = TRUE)
	gSignalConnect(edge, "clicked", edgeRfun)
	the.buttons$packStart(edge,fill=FALSE)

 # button
	deseq <- gtkButtonNewWithMnemonic("_DESeq", show = TRUE)
	gSignalConnect(deseq, "clicked", deseqfun)
	the.buttons$packStart(deseq,fill=FALSE)

 # button
	deseq2 <- gtkButtonNewWithMnemonic("_DESeq2", show = TRUE)
	gSignalConnect(deseq2, "clicked", deseq2fun)
	the.buttons$packStart(deseq2,fill=FALSE)

# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

 # button
	edgeComplexDesign <- gtkButtonNewWithMnemonic("_EdgeR GLM for Multi Factor", show = TRUE)
	gSignalConnect(edgeComplexDesign, "clicked", edgeRComplexDesignfun)
	the.buttons$packStart(edgeComplexDesign,fill=FALSE)

 # button
	deseqComplexDesign <- gtkButtonNewWithMnemonic("_DESeq for Complex Design", show = TRUE)
	gSignalConnect(deseqComplexDesign, "clicked", deseqComplexDesignfun)
	the.buttons$packStart(deseqComplexDesign,fill=FALSE)

 # button
	deseq2ComplexDesign <- gtkButtonNewWithMnemonic("_DESeq2 for Complex Design", show = TRUE)
	gSignalConnect(deseq2ComplexDesign, "clicked", deseq2ComplexDesignfun)
	the.buttons$packStart(deseq2ComplexDesign,fill=FALSE)

# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

 # button
	bay <- gtkButtonNewWithMnemonic("_BaySeq", show = TRUE)
	gSignalConnect(bay, "clicked", bayseqfun)
	the.buttons$packStart(bay,fill=FALSE)

 # button
	noi <- gtkButtonNewWithMnemonic("_NoiSeq", show = TRUE)
	gSignalConnect(noi, "clicked", noifun)
	the.buttons$packStart(noi,fill=FALSE)



}

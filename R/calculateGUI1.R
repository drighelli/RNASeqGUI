calculateGUI1 <-
function(project_name) {

 Project <- project_name
 message=paste("You are using the project: ",Project,sep="")
 print(message)

	openFile <- function(button,user.data) {
		dialog <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
		dialog$setCurrentFolder(paste("~/",sep=""))
		if (dialog$run() == GtkResponseType["accept"]) {
			the.sel.file <- dialog$getFilename()
			filename$setText(the.sel.file)
			dialog$destroy()
		} else {
			dialog$destroy()
		}
	}

	openCount <- function(button,user.data) {
		dialog <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
		dialog$setCurrentFolder("~/")
		if (dialog$run() == GtkResponseType["accept"]) {
			the.sel.count <- dialog$getFilename()
			countFile$setText(the.sel.count)
			dialog$destroy()
		} else {
			dialog$destroy()
		}
	}


performQPlotDensity<- function(button, user.data) {
	 res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
  the.log <- logEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = QPlotDensity(d,the.file,the.log,Project)                   
			}
 rm(list = ls())
	} #End of function

performQPlotHistogram <- function(button, user.data) {
	 res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
  the.log <- logEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = QPlotHistogram(d,the.file,the.log,Project)  
  }                 
 rm(list = ls())
	} #End of function


performStatistics_PCA <- function(button, user.data) {  

  res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
  the.legend <- legend$GetActiveText()

		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)

  #factors <- factorsEntry$getText()
  #conditions=unlist(strsplit(factors, ","))

		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = PCAfun(d,the.file,the.legend,Project)                   			
		}
 rm(list = ls())
	} #End of function


performPCA2 <- function(button, user.data) {
	  res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
  the.legend <- legend$GetActiveText()

		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)

  #factors <- factorsEntry$getText()
  #conditions=unlist(strsplit(factors, ","))

		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = PCA2fun(d,the.file,the.legend,Project) 
  }                  
 rm(list = ls())
	} #End of function

ComHistFun <- function(button, user.data) {
		res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = ComponentHist(d,the.file,Project)                    
  }
 rm(list = ls())
	} #End of function

performStatistics_Heatmap <- function(button, user.data) {
		res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
  the.headers2 <- headers2Entry$active
  the.log <- logEntry$active
  the.countFile <- countFile$getText()
		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)
  countFile <- scan(the.countFile, what="", sep="\n")
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = Heatmap(d,the.file,countFile,the.log,Project) 
		}
 rm(list = ls())
	} #End of function

performStatistics_PlotCounts <- function(button, user.data) {
		res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
  column1 = 	column1Entry$getText()
  column2 =  column2Entry$getText()
  the.log <- logEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = PlotCounts(d[,numVar],column1,column2,the.file,the.log,Project)                     
		}
 rm(list = ls())
	} #End of function

performStatistics_density <- function(button, user.data) {
		res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
  column <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
  column = 	column1Entry$getText()
  the.log <- logEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = densityFun(d[,numVar],column,the.file,the.log,Project)                    
		}
 rm(list = ls())
	} #End of function

performStatistics_PlotAllCounts <- function(button, user.data) {
		res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = PlotAllCounts(d[,numVar],the.file,Project)                      
		}
 rm(list = ls())
	} #End of function

performStatistics_PlotSPMCounts <- function(button, user.data) {
		res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = PlotSPM(d[,numVar],the.file,Project)                      
		}
 rm(list = ls())
	} #End of function

performStatistics_MDPlot <- function(button, user.data) {
		res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)
  column1 = 	column1Entry$getText()
  column2 =  column2Entry$getText()
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = MeanDiffPlot(d,column1,column2,the.file,Project)                    
		}
 rm(list = ls())
	} #End of function

performStatistics_MeanVarPlot <- function(button, user.data) {
		res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = meanVariancePlot(d,the.file,Project)                 
		}
 rm(list = ls())
	} #End of function

performStatistics_countDistr <- function(button, user.data) {
		res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
  the.log <- logEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = countDistr(d,the.file,the.log,Project)                   
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
   label1 <- gtkLabelNewWithMnemonic("Select a count file by clicking on the corresponding Open button.")
   label2 <- gtkLabelNewWithMnemonic("The usage of each button is explained below.")
   label3 <- gtkLabelNewWithMnemonic("Plot Pairs of Counts: use 'Column1' and 'Column2' fields to select the number of the columns you want to plot.")
   label4 <- gtkLabelNewWithMnemonic("Plot All Counts: just click the button.")
   label5 <- gtkLabelNewWithMnemonic("Count Distr: just click the button.") 
   label6 <- gtkLabelNewWithMnemonic("Density: use 'Column1' field to select the number of the column you want to plot.")
   label7 <- gtkLabelNewWithMnemonic("MDPlot: use 'Column1' and 'Column'2 fields to select the columns you want to plot.")
   label8 <- gtkLabelNewWithMnemonic("MeanVarPlot: just click the button.")
   label9 <- gtkLabelNewWithMnemonic("Heatmap: load a list of gene names as well to create an heatmap of those genes.") 
   label10 <- gtkLabelNewWithMnemonic("Density: use 'Column1' field to select the column you want to plot.")
   label11 <- gtkLabelNewWithMnemonic("PCA: use 'Legend Position in PCA' field to specify the position of the legend in the plot.")
   label13 <- gtkLabelNewWithMnemonic("PCA 3D: use 'Legend Position in PCA' field to specify the position of the legend in the plot.")
   label15 <- gtkLabelNewWithMnemonic("Component Histogram: just click the button.")
   label16 <- gtkLabelNewWithMnemonic("QPlot Histogram: just click the button.")
   label17 <- gtkLabelNewWithMnemonic("QPlot Density: just click the button.")
   label18 <- gtkLabelNewWithMnemonic("For further information, see http://bioinfo.na.iac.cnr.it/RNASeqGUI/manual.pdf")

         vbox$packStart(label1,FALSE,FALSE,0)
         vbox$packStart(label2,FALSE,FALSE,0)
        	vbox$packStart(label3,FALSE,FALSE,0)
         vbox$packStart(label4,FALSE,FALSE,0)
        	vbox$packStart(label5,FALSE,FALSE,0)
        	vbox$packStart(label6,FALSE,FALSE,0)
         vbox$packStart(label7,FALSE,FALSE,0)
         vbox$packStart(label8,FALSE,FALSE,0)
        	vbox$packStart(label9,FALSE,FALSE,0)
        	vbox$packStart(label10,FALSE,FALSE,0)
         vbox$packStart(label11,FALSE,FALSE,0)
         vbox$packStart(label12,FALSE,FALSE,0)
        	vbox$packStart(label13,FALSE,FALSE,0)
         vbox$packStart(label14,FALSE,FALSE,0)
         vbox$packStart(label15,FALSE,FALSE,0)
         vbox$packStart(label16,FALSE,FALSE,0)
         vbox$packStart(label17,FALSE,FALSE,0)
         vbox$packStart(label18,FALSE,FALSE,0)
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
 title=paste("    \n\n   Data Exploration Interface is ready to work on ",Project, " project.", sep="")
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
	label <- gtkLabelNewWithMnemonic("_Select a count file")
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
	label <- gtkLabelNewWithMnemonic("Column _Separator")
	hbox$packStart(label,FALSE,FALSE,0)
	sepEntry <- gtkEntryNew()
	sepEntry$setWidthChars(1)
	sepEntry$setText("")
	hbox$packStart(sepEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(sepEntry)

# Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add label in first column
	label <- gtkLabelNewWithMnemonic("_Select a list of genes for the Heatmap")
	hbox$packStart(label,FALSE,FALSE,0)
	# Add entry in the second column; named "countFile"
	countFile <- gtkEntryNew()
	countFile$setWidthChars(50)
	label$setMnemonicWidget(countFile)
	hbox$packStart(countFile,FALSE,FALSE,0)
	buttonOpen <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen, "clicked", openCount)
	hbox$packStart(buttonOpen,FALSE,FALSE,0)

	# are headers included in the file?
	label <- gtkLabelNewWithMnemonic("_Headers?")
	hbox$packStart(label,FALSE,FALSE,0)
	headers2Entry <- gtkCheckButton()
	headers2Entry$active <- TRUE
	hbox$packStart(headers2Entry,FALSE,FALSE,0)
	label$setMnemonicWidget(headers2Entry)


	# Add an horizontal container to specify input file options
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

	# Conditions
	#label <- gtkLabelNewWithMnemonic("_PCA Factors?")
	#hbox$packStart(label,FALSE,FALSE,0)
	#factorsEntry <- gtkEntryNew()
	#factorsEntry$setWidthChars(18)
	#factorsEntry$setText("")
	#hbox$packStart(factorsEntry,FALSE,FALSE,0)
	#label$setMnemonicWidget(factorsEntry)

  # Add label in first column
  label <- gtkLabelNewWithMnemonic("_Column1:")
  hbox$packStart(label,FALSE,FALSE,0)
  # Add entry in the second column1Entry; named "filename"
  column1Entry <- gtkEntryNew() ##check the entry to numbers
  column1Entry$setWidthChars(6)
  column1Entry$setMaxLength(3) 
  
  gSignalConnect(column1Entry, "changed", 
                 function(entry) {
                   text <- entry$getText()
                   if(nzchar(gsub("[0-9]", "", text))) {
                     entry$setIconFromStock("primary", "gtk-no")
                     
                   } else {
                     entry$setIconFromStock("primary", "gtk-yes")
                     entry$setIconTooltipText("primary", NULL)
                   }
                 })
  column1Entry$setIconFromStock("primary", "gtk-yes")
  column1Entry$setIconTooltipText("primary", "Only numbers are allowed" )
  label$setMnemonicWidget(column1Entry)
  hbox$packStart(column1Entry,FALSE,FALSE,0)

  # Add label in first column
  label <- gtkLabelNewWithMnemonic("_Column2:")
  hbox$packStart(label,FALSE,FALSE,0)
  # Add entry in the second column2Entry; named "filename"
  column2Entry <- gtkEntryNew() ##check the entry to numbers
  column2Entry$setWidthChars(6)
  column2Entry$setMaxLength(3) 
  
  gSignalConnect(column2Entry, "changed", 
                 function(entry) {
                   text <- entry$getText()
                   if(nzchar(gsub("[0-9]", "", text))) {
                     entry$setIconFromStock("primary", "gtk-no")
                     
                   } else {
                     entry$setIconFromStock("primary", "gtk-yes")
                     entry$setIconTooltipText("primary", NULL)
                   }
                 })
  column2Entry$setIconFromStock("primary", "gtk-yes")
  column2Entry$setIconTooltipText("primary", "Only numbers are allowed" )
  label$setMnemonicWidget(column2Entry)
  hbox$packStart(column2Entry,FALSE,FALSE,0)

 #log
	label <- gtkLabelNewWithMnemonic("_log?")
	hbox$packStart(label,FALSE,FALSE,0)
 logEntry <- gtkCheckButton()
	logEntry$active <- TRUE
	hbox$packStart(logEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(logEntry)

	# legend
  label <- gtkLabelNewWithMnemonic("_Legend position in PCA:")
  hbox$packStart(label,FALSE,FALSE,0)
  legend <- gtkComboBoxNewText()
  
  positions <- c("topright", "topleft", "bottomright", "bottomleft")
  sapply(positions, legend$appendText)
  gtkComboBoxSetActive(legend,0)
  label$setMnemonicWidget(legend)
  hbox$packStart(legend, FALSE, FALSE, 0)


###################################################

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

	label <- gtkLabelNewWithMnemonic("_Look at BAM files")
	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

 # button
	PlotCounts <- gtkButtonNewWithMnemonic("Plot Pairs of Counts", show = TRUE)
	gSignalConnect(PlotCounts, "clicked", performStatistics_PlotCounts)
	the.buttons$packStart(PlotCounts,fill=FALSE)

 # button
	PlotSPMCounts <- gtkButtonNewWithMnemonic("Plot All Counts", show = TRUE)
	gSignalConnect(PlotSPMCounts, "clicked", performStatistics_PlotSPMCounts)
	the.buttons$packStart(PlotSPMCounts,fill=FALSE)

 # button
	countDistr <- gtkButtonNewWithMnemonic("Count Distr", show = TRUE)
	gSignalConnect(countDistr, "clicked", performStatistics_countDistr)
	the.buttons$packStart(countDistr,fill=FALSE) 

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

 # button
	density <- gtkButtonNewWithMnemonic("Density", show = TRUE)
	gSignalConnect(density, "clicked", performStatistics_density)
	the.buttons$packStart(density,fill=FALSE) 

 # button
 MDPlot <- gtkButtonNewWithMnemonic("MDPlot", show = TRUE)
	gSignalConnect(MDPlot, "clicked", performStatistics_MDPlot)
	the.buttons$packStart(MDPlot,fill=FALSE)

 # button
	MeanVarPlot <- gtkButtonNewWithMnemonic("MeanVarPlot", show = TRUE)
	gSignalConnect(MeanVarPlot, "clicked", performStatistics_MeanVarPlot)
	the.buttons$packStart(MeanVarPlot,fill=FALSE) 

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

 # button
	Heatmap <- gtkButtonNewWithMnemonic("Heatmap", show = TRUE)
	gSignalConnect(Heatmap, "clicked", performStatistics_Heatmap)
	the.buttons$packStart(Heatmap,fill=FALSE)

 # button
	PCA <- gtkButtonNewWithMnemonic("PCA", show = TRUE)
	gSignalConnect(PCA, "clicked", performStatistics_PCA)
	the.buttons$packStart(PCA,fill=FALSE)

 # button
	PCA2 <- gtkButtonNewWithMnemonic("PCA3D", show = TRUE)
	gSignalConnect(PCA2, "clicked", performPCA2)
	the.buttons$packStart(PCA2,fill=FALSE)

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

 # button
	ComHist <- gtkButtonNewWithMnemonic("Component Histogram", show = TRUE)
	gSignalConnect(ComHist, "clicked", ComHistFun)
	the.buttons$packStart(ComHist,fill=FALSE)

 # button
	qplot <- gtkButtonNewWithMnemonic("QPlot Histogram", show = TRUE)
	gSignalConnect(qplot, "clicked", performQPlotHistogram)
	the.buttons$packStart(qplot,fill=FALSE)

 # button
	qplotDensity <- gtkButtonNewWithMnemonic("QPlot Density", show = TRUE)
	gSignalConnect(qplotDensity, "clicked", performQPlotDensity)
	the.buttons$packStart(qplotDensity,fill=FALSE)

}

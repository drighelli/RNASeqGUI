calculate_DESeq <- function(project_name) {

 Project <- project_name
 message=paste("You are using the project: ",Project,sep="")
 print(message)

	openFile <- function(button,user.data) {
		dialog1 <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
		dialog1$setCurrentFolder(paste("~/",sep=""))
		if (dialog1$run() == GtkResponseType["accept"]) {
			the.sel.file1 <- dialog1$getFilename()
			filename$setText(the.sel.file1)
			dialog1$destroy()
		} else {
			dialog1$destroy()
		}
	}

	openFile2 <- function(button,user.data) {
		dialog2 <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
		dialog2$setCurrentFolder(paste("~/",sep=""))
		if (dialog2$run() == GtkResponseType["accept"]) {
			the.sel.file2 <- dialog2$getFilename()
			filename2$setText(the.sel.file2)
			dialog2$destroy()
		} else {
			dialog2$destroy()
		}
	}


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
   label1 <- gtkLabelNewWithMnemonic(paste0("As a usage example, you can look at the dataset shown in the manual at http://bioinfo.na.iac.cnr.it/RNASeqGUI/manual.pdf.\n",
                                            "In this example, there are 7 samples: 3 treated and 4 untreated. Here You want to compare the treated samples\n",
                                            "(represented by T) against the untreated ones (U).\n",
                                            "To this purpose, Select a count file by clicking on the corresponding 'open' button.\n",
                                            "Then, select a design file, written in a tab delimited format, by clicking on the corresponding 'open' button.\n",
                                            "A design file must be in the following format: "))
   label9 <- gtkLabelNewWithMnemonic("-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
   label10 <- gtkLabelNewWithMnemonic("				T1				T2				T3				U1				U2				U3				U4")
   label11 <- gtkLabelNewWithMnemonic("				T				T				T				U				U				U				U")
   label12 <- gtkLabelNewWithMnemonic("\t\tsingle-end\t\tpaired-end\tpaired-end\tsingle-end\t\tpaired-end\tpaired-end\tsingle-end")
   label13 <- gtkLabelNewWithMnemonic("-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
   label14 <- gtkLabelNewWithMnemonic(paste0("In the example above, the design file describes four samples: two untreated ones and two treated ones.\n", 
"The third line of the design file describes an extra feature or an extra covariate that can be\n",
"a batch effect, a gender, a type of reads (single or paired), etc.\n",
"According with the design file above, please specify which is the 'Treated' (in this case 'T')\n",
"and the 'Control' ('U') columns in the corresponding fields.\n",
"Then, specify a 'Padj' value in the corresponding field.\n",
"Finally, click on the 'Run DESeq' button.\n",
"For further information, see www.bioconductor.org/packages/release/bioc/vignettes/DESeq/inst/doc/DESeq.pdf."))

         vbox$packStart(label1)
         vbox$packStart(label9,FALSE,FALSE,0)
         vbox$packStart(label10,FALSE,FALSE,0)       	
        	vbox$packStart(label11,FALSE,FALSE,0)
         vbox$packStart(label12,FALSE,FALSE,0)
        	vbox$packStart(label13,FALSE,FALSE,0)
        	vbox$packStart(label14,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
}

 performStatistics_DESeq <- function(button, user.data) {
		res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
  #factors <- factorsEntry$getText()
  #conditions=unlist(strsplit(factors, ","))
  #libTypes <- libTypesEntry$getText()
  #libTypes=unlist(strsplit(libTypes, ","))
  design.file <- filename2$getText()
		design=read.table(design.file,header=T,sep="\t",row.names=NULL)   ###### 	LOAD THE DESIGN #########
  conditions <- design[1,]
  conditions <- c(as.character(unlist(conditions)))
  libTypes <- design[2,]
  libTypes <- c(as.character(unlist(libTypes))) 
  first <- firstEntry$getText()
  treated=unlist(strsplit(first, ","))
  second <- secondEntry$getText()
  control=unlist(strsplit(second, ","))
  padj = padj$getText()
		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)   ###### 	LOAD THE FILE #########
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   res = DESeq(d,conditions,libTypes,treated,control,the.file,padj,Project) 
			# Get saving options and save if needed
			isToSave <- toSave$active
			exportName <- exportfilename$getText()
			if ((isToSave)&(exportName=="")) {
				warning <- "Invalid user data: a file name must be specified if you want to save results."
			} else {
				where.sep <- gregexpr("/",the.file)[[1]]
				where.sep <- max(where.sep)
				save.dir <- substr(the.file,1,where.sep)
				write.table(res,file=paste("RNASeqGUI_Projects/",Project,"/Results/",exportName,".csv",sep=""))
			}
		}
		if (!is.null(res)) {
			########## Start dialog...
			# Open a dialog box to print results
			dialog <- gtkDialogNewWithButtons("DESeq finished!",window, "modal","gtk-ok", GtkResponseType["ok"],"gtk-quit", GtkResponseType["cancel"])
		
			# Create vertical container for file name entry
			vbox <- gtkVBoxNew(FALSE, 8)
			vbox$setBorderWidth(24)
			dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
			
			# Add warning...
			if (!is.null(warning)) {
				hbox <- gtkHBoxNew()
				vbox$packStart(hbox,FALSE,FALSE,0)
				label <- gtkLabel(warning)
				hbox$packStart(label,FALSE,FALSE,0)
			}


			# If results are saved, print information about it
			if ((isToSave)&(exportName!="")) {
				hbox <- gtkHBoxNew(FALSE,20)
				vbox$packStart(hbox, FALSE, FALSE, 0)
				label <- gtkLabelNew(paste("The ", exportName,".csv file has been saved in RNASeqGUI_Projects/",Project,"/Results/", sep=""))
				hbox$packStart(label,FALSE,FALSE,0)
			}

			# If results are NOT saved, print information about it
			if (exportName=="") {
				hbox <- gtkHBoxNew(FALSE,20)
				vbox$packStart(hbox, FALSE, FALSE, 0)
				label <- gtkLabelNew(paste("Results have been saved in RNASeqGUI_Projects/",Project,"/Results/", sep=""))
				hbox$packStart(label,FALSE,FALSE,0)
			}
			
			
			response <- dialog$run()
			# Return to previous window
			if (response == GtkResponseType["ok"]) {
				dialog$destroy()
			}
			# Quit all windows
			if (response == GtkResponseType["cancel"]) {
				dialog$destroy()
				window$destroy()
			}
		} else {
			dialog <- gtkMessageDialogNew(window, c("modal", "destroy-with-parent"), "info", "ok",error)
			dialog$run()
			dialog$destroy()
		}
 rm(list = ls())
	} #End of function

perform_showresults <- function(button, user.data) {
		res <- NULL
  res = DESeqShowresults(Project) # MAIN FUNCTION HERE
  res
  rm(list = ls())
	} #End of function


##################################################################################
# Create window
	window <- gtkWindow()
	# Add title
	window["title"] <- "RNASeqGUI"

	# Add a frame
 title=paste("       DESeq Interface is ready to work on ",Project, " project.", sep="")
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
	label <- gtkLabelNewWithMnemonic("Column Sep.")
	hbox$packStart(label,FALSE,FALSE,0)
	sepEntry <- gtkEntryNew()
	sepEntry$setWidthChars(1)
	sepEntry$setText("")
	hbox$packStart(sepEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(sepEntry)

	# Add an horizontal container to specify input file options
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add label in first column
	label2 <- gtkLabelNewWithMnemonic("_Select a design file in tsv format")
	hbox$packStart(label2,FALSE,FALSE,0)
	# Add entry in the second column; named "filename"
	filename2 <- gtkEntryNew()
	filename2$setWidthChars(45)
	label2$setMnemonicWidget(filename2)
	hbox$packStart(filename2,FALSE,FALSE,0)
	buttonOpen2 <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen2, "clicked", openFile2)
	hbox$packStart(buttonOpen2,FALSE,FALSE,0)

	# Add an horizontal container to specify input file options
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

	# first
	label <- gtkLabelNewWithMnemonic("_Treated")
	hbox$packStart(label,FALSE,FALSE,0)
	firstEntry <- gtkEntryNew()
	firstEntry$setWidthChars(8)
	firstEntry$setText("")
	hbox$packStart(firstEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(firstEntry)

	# second
	label <- gtkLabelNewWithMnemonic("_Control")
	hbox$packStart(label,FALSE,FALSE,0)
	secondEntry <- gtkEntryNew()
	secondEntry$setWidthChars(8)
	secondEntry$setText("")
	hbox$packStart(secondEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(secondEntry)

 # Padj
 label <- gtkLabelNewWithMnemonic("_Padj")
	hbox$packStart(label,FALSE,FALSE,0)
	padj <- gtkEntryNew()
	padj$setWidthChars(6)
	padj$setText("0.05")
	hbox$packStart(padj,FALSE,FALSE,0)
	label$setMnemonicWidget(padj)

# Add an horizontal container to specify input file options
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)


	label <- gtkLabelNewWithMnemonic("_Save outputs")
	vbox$packStart(label,FALSE,FALSE,0)
	
	# Add two horizontal containers to check if the results has to be exported in a file and if so, to specify file named
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)
	label <- gtkLabelNewWithMnemonic("Save _Results?")
	hbox$packStart(label,FALSE,FALSE,0)
	toSave <- gtkCheckButton()
	hbox$packStart(toSave,FALSE,FALSE,0)
	label$setMnemonicWidget(toSave)
	label <- gtkLabelNewWithMnemonic("_Name?")
	hbox$packStart(label,FALSE,FALSE,0)
	exportfilename <- gtkEntryNew()
	exportfilename$setWidthChars(50)
	exportfilename$setText("")
	hbox$packStart(exportfilename,FALSE,FALSE,0)
	label$setMnemonicWidget(exportfilename)
	label <- gtkLabel(".csv")
	hbox$packStart(label,FALSE,FALSE,0)

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
	noi <- gtkButtonNewWithMnemonic("How to use this Interface", show = TRUE)
	gSignalConnect(noi, "clicked", infoFun)
	the.buttons$packStart(noi,fill=FALSE)

 # button
	deseq <- gtkButtonNewWithMnemonic("Run DESeq", show = TRUE)
	gSignalConnect(deseq, "clicked", performStatistics_DESeq)
	the.buttons$packStart(deseq,fill=FALSE)

 # button
	#deseqComplexDesign <- gtkButtonNewWithMnemonic("Run DESeqComplexDesign", show = TRUE)
	#gSignalConnect(deseqComplexDesign, "clicked", performStatistics_DESeqComplexDesign)
	#the.buttons$packStart(deseqComplexDesign,fill=FALSE)

 # button
 showresults <- gtkButtonNewWithMnemonic("Show Results", show = TRUE)
	gSignalConnect(showresults, "clicked", perform_showresults)
	the.buttons$packStart(showresults,fill=FALSE)

}

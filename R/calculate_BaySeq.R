calculate_BaySeq <-
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
   label1 <- gtkLabelNewWithMnemonic("As a usage example, you can look at the dataset shown at page 27 in the")
   label2 <- gtkLabelNewWithMnemonic("manual available at http://bioinfo.na.iac.cnr.it/RNASeqGUI/manual.pdf.")
   label3 <- gtkLabelNewWithMnemonic("In this example, there are 7 samples: 3 treated and 4 untreated.") 
   label12 <- gtkLabelNewWithMnemonic("If you want to compare the treated samples (represented by T) against the") 
   label4 <- gtkLabelNewWithMnemonic("untreated ones (U) you have to fill the fields in the following way.")
  	label5 <- gtkLabelNewWithMnemonic("Factors?: T, T, T, U, U, U, U ")
  	label8 <- gtkLabelNewWithMnemonic("NDE?: 1,1,1,1,1,1,1")
   label9 <- gtkLabelNewWithMnemonic("DE?:  1,1,1,2,2,2,2")
   label6 <- gtkLabelNewWithMnemonic("Estimation Type?:  quantile")
  	label7 <- gtkLabelNewWithMnemonic("Sample Size?: 1000")
  	label11 <- gtkLabelNewWithMnemonic("FDR?: 0.05")
  	label13 <- gtkLabelNewWithMnemonic("SamplesA?: T")
  	label14 <- gtkLabelNewWithMnemonic("SamplesB?: U")
   label15 <- gtkLabelNewWithMnemonic("Finally, click on the Run BaySeq with NB button.")
   label16 <- gtkLabelNewWithMnemonic("For further information, see www.bioconductor.org/packages/release/bioc/vignettes/baySeq/inst/doc/baySeq.pdf")

         vbox$packStart(label1,FALSE,FALSE,0)
         vbox$packStart(label2,FALSE,FALSE,0)
        	vbox$packStart(label3,FALSE,FALSE,0)
         vbox$packStart(label12,FALSE,FALSE,0)
         vbox$packStart(label4,FALSE,FALSE,0)
        	vbox$packStart(label5,FALSE,FALSE,0)
         vbox$packStart(label8,FALSE,FALSE,0)
        	vbox$packStart(label9,FALSE,FALSE,0)
         vbox$packStart(label6,FALSE,FALSE,0)
        	vbox$packStart(label7,FALSE,FALSE,0)
        	vbox$packStart(label11,FALSE,FALSE,0)
         vbox$packStart(label13,FALSE,FALSE,0)
        	vbox$packStart(label14,FALSE,FALSE,0)
         vbox$packStart(label15,FALSE,FALSE,0)
         vbox$packStart(label16,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
}

 performStatistics_BaySeqNB <- function(button, user.data) {
		res <- NULL
		d <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1) 
  factors <- factorsEntry$getText()
  conditions=unlist(strsplit(factors, ","))
  list_nde <- list_ndeEntry$getText()
  list_nde=unlist(strsplit(list_nde, ","))
  list_de <- list_deEntry$getText()
  list_de=unlist(strsplit(list_de, ","))
  estType <- estTypeEntry$getText()
  samsize <- samsizeEntry$getText()
  the.sA <- sA$getText()
  the.sB <- sB$getText()
  FDR <- fdr$getText()
  print(FDR)
		# Select numerical variables
		numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
		if (sum(numVar)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
   #res = BaySeq(d,conditions,list_nde,list_de,estType,samsize)     
   res = BaySeqNB(d,conditions,list_nde,list_de,estType,samsize,the.sA,the.sB,the.file,FDR,Project)                 
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
			dialog <- gtkDialogNewWithButtons("BaySeqNB Finished!",window, "modal","gtk-ok", GtkResponseType["ok"],"gtk-quit", GtkResponseType["cancel"])
		
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
  res = BayseqShowresults(Project) # MAIN FUNCTION HERE
  res
  rm(list = ls())
	} #End of function

##################################################################################
# Create window
	window <- gtkWindow()
	# Add title
	window["title"] <- "RNASeqGUI"

	# Add a frame
 title=paste("       BaySeq Interface is ready to work on ",Project, " project.", sep="")
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
	label <- gtkLabelNewWithMnemonic("What _is _the _Column _Separator?")
	hbox$packStart(label,FALSE,FALSE,0)
	sepEntry <- gtkEntryNew()
	sepEntry$setWidthChars(1)
	sepEntry$setText("")
	hbox$packStart(sepEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(sepEntry)


	# Add an horizontal container to specify input file options
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

	# Conditions
	label <- gtkLabelNewWithMnemonic("_Factors?")
	hbox$packStart(label,FALSE,FALSE,0)
	factorsEntry <- gtkEntryNew()
	factorsEntry$setWidthChars(30)
	factorsEntry$setText("")
	hbox$packStart(factorsEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(factorsEntry)

	# list_nde
	label <- gtkLabelNewWithMnemonic("_NDE?")
	hbox$packStart(label,FALSE,FALSE,0)
	list_ndeEntry <- gtkEntryNew()
	list_ndeEntry$setWidthChars(8)
	list_ndeEntry$setText("")
	hbox$packStart(list_ndeEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(list_ndeEntry)

	# list_de
	label <- gtkLabelNewWithMnemonic("_DE?")
	hbox$packStart(label,FALSE,FALSE,0)
	list_deEntry <- gtkEntryNew()
	list_deEntry$setWidthChars(8)
	list_deEntry$setText("")
	hbox$packStart(list_deEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(list_deEntry)

	# estType  
	label <- gtkLabelNewWithMnemonic("_Estimation Type?")
	hbox$packStart(label,FALSE,FALSE,0)
	estTypeEntry <- gtkEntryNew()
	estTypeEntry$setWidthChars(10)
	estTypeEntry$setText("quantile")
	hbox$packStart(estTypeEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(estTypeEntry)

 # samsize
	label <- gtkLabelNewWithMnemonic("_Sample Size?")
	hbox$packStart(label,FALSE,FALSE,0)
	samsizeEntry <- gtkEntryNew()
	samsizeEntry$setWidthChars(6)
	samsizeEntry$setText("1000")
	hbox$packStart(samsizeEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(samsizeEntry)

	# Add an horizontal container to specify input file options
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # FDR
 label <- gtkLabelNewWithMnemonic("_FDR?")
	hbox$packStart(label,FALSE,FALSE,0)
	fdr <- gtkEntryNew()
	fdr$setWidthChars(6)
	fdr$setText("0.05")
	hbox$packStart(fdr,FALSE,FALSE,0)
	label$setMnemonicWidget(fdr)

 # SampleA
 label <- gtkLabelNewWithMnemonic("_Sample A?")
	hbox$packStart(label,FALSE,FALSE,0)
	sA <- gtkEntryNew()
	sA$setWidthChars(9)
	sA$setText("")
	hbox$packStart(sA,FALSE,FALSE,0)
	label$setMnemonicWidget(sA)

 # SampleB
 label <- gtkLabelNewWithMnemonic("_Sample B?")
	hbox$packStart(label,FALSE,FALSE,0)
	sB <- gtkEntryNew()
	sB$setWidthChars(9)
	sB$setText("")
	hbox$packStart(sB,FALSE,FALSE,0)
	label$setMnemonicWidget(sB)


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
	bay <- gtkButtonNewWithMnemonic("Run BaySeq with NB", show = TRUE)
	gSignalConnect(bay, "clicked", performStatistics_BaySeqNB)
	the.buttons$packStart(bay,fill=FALSE)

 # button
	#bay <- gtkButtonNewWithMnemonic("Run BaySeq with BB", show = TRUE)
	#gSignalConnect(bay, "clicked", performStatistics_BaySeqBB)
	#the.buttons$packStart(bay,fill=FALSE)

 # button
 showresults <- gtkButtonNewWithMnemonic("Show Results", show = TRUE)
	gSignalConnect(showresults, "clicked", perform_showresults)
	the.buttons$packStart(showresults,fill=FALSE)


}

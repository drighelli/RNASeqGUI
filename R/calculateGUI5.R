calculateGUI5 <-
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

performRPKM <- function(button, user.data) {
		d <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)
  res = RPKM(d,the.file,Project)  
  rm(list = ls())                  
	} #End of function


performUqua <- function(button, user.data) {
		d <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)
  res = UQUA(d,the.file,Project)   
  rm(list = ls())                
	} #End of function


performTMM <- function(button, user.data) {
		d <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)
  res = TMM(d,the.file,Project)  
  rm(list = ls())                  
	} #End of function


performFqua <- function(button, user.data) {
		d <- NULL
		# Get the information about data and the file
		the.file <- filename$getText()
		the.sep <- sepEntry$getText()
		the.headers <- headersEntry$active
		d <- read.table(the.file,sep=the.sep,header=the.headers,row.names=1)
  res = FQUA(d,the.file,Project)      
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
   label2 <- gtkLabelNewWithMnemonic("Then, click on the normalization button you want to use.")
   label3 <- gtkLabelNewWithMnemonic("For further information, see http://bioinfo.na.iac.cnr.it/RNASeqGUI/manual.pdf")

         vbox$packStart(label1,FALSE,FALSE,0)
         vbox$packStart(label2,FALSE,FALSE,0)
        	vbox$packStart(label3,FALSE,FALSE,0)
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
 title=paste("   \n\n    Normalization Interface is ready to work on ",Project, " project.", sep="")
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


#####################################################################

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

	label <- gtkLabelNewWithMnemonic("_Normalization Procedures")
	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

	# button
	buttonRPKM <- gtkButtonNewWithMnemonic("CPM", show = TRUE)
	gSignalConnect(buttonRPKM, "clicked", performRPKM)
	the.buttons$packStart(buttonRPKM,fill=FALSE)

	# button
	buttonUqua <- gtkButtonNewWithMnemonic("Upper Quartile", show = TRUE)
	gSignalConnect(buttonUqua, "clicked", performUqua)
	the.buttons$packStart(buttonUqua,fill=FALSE)

	# button
	buttonTMM <- gtkButtonNewWithMnemonic("TMM", show = TRUE)
	gSignalConnect(buttonTMM, "clicked", performTMM)
	the.buttons$packStart(buttonTMM,fill=FALSE)

	# button
	buttonFqua <- gtkButtonNewWithMnemonic("Full Quantile", show = TRUE)
	gSignalConnect(buttonFqua, "clicked", performFqua)
	the.buttons$packStart(buttonFqua,fill=FALSE)



}

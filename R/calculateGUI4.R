calculateGUI4 <-
function(project_name) {

 Project <- project_name
 message=paste("You are using the project: ",Project,sep="")
 print(message)

	openFile1 <- function(button,user.data) {
		dialog1 <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
		dialog1$setCurrentFolder(paste("~/RNASeqGUI_Projects/",Project,"/Results/",sep=""))
		if (dialog1$run() == GtkResponseType["accept"]) {
			the.sel.file1 <- dialog1$getFilename()
			filename1$setText(the.sel.file1)
			dialog1$destroy()
		} else {
			dialog1$destroy()
		}
	}

	openFile2 <- function(button,user.data) {
		dialog2 <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
		dialog2$setCurrentFolder(paste("~/RNASeqGUI_Projects/",Project,"/Results/",sep=""))
		if (dialog2$run() == GtkResponseType["accept"]) {
			the.sel.file2 <- dialog2$getFilename()
			filename2$setText(the.sel.file2)
			dialog2$destroy()
		} else {
			dialog2$destroy()
		}
	}

	openFile3 <- function(button,user.data) {
		dialog3 <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
		dialog3$setCurrentFolder(paste("~/RNASeqGUI_Projects/",Project,"/Results/",sep=""))
		if (dialog3$run() == GtkResponseType["accept"]) {
			the.sel.file3 <- dialog3$getFilename()
			filename3$setText(the.sel.file3)
			dialog3$destroy()
		} else {
			dialog3$destroy()
		}
	}

openFile4 <- function(button,user.data) {
		dialog4 <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
		dialog4$setCurrentFolder(paste("~/RNASeqGUI_Projects/",Project,"/Results/",sep=""))
		if (dialog4$run() == GtkResponseType["accept"]) {
			the.sel.file4 <- dialog4$getFilename()
			filename4$setText(the.sel.file4)
			dialog4$destroy()
		} else {
			dialog4$destroy()
		}
	}

 vennfun <- function(button, user.data) { 
 # Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_Venn Diagrams")
	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

 # button
	buttonVenn2DE <- gtkButtonNewWithMnemonic("VennDiagram 2 sets DE", show = TRUE)
	gSignalConnect(buttonVenn2DE, "clicked", Venn2DE)
	the.buttons$packStart(buttonVenn2DE,fill=F)

 # button
	buttonVenn3DE <- gtkButtonNewWithMnemonic("VennDiagram 3 sets DE", show = TRUE)
	gSignalConnect(buttonVenn3DE, "clicked", Venn3DE)
	the.buttons$packStart(buttonVenn3DE,fill=F)

 # button
	buttonVenn4DE <- gtkButtonNewWithMnemonic("VennDiagram 4 sets DE", show = TRUE)
	gSignalConnect(buttonVenn4DE, "clicked", Venn4DE)
	the.buttons$packStart(buttonVenn4DE,fill=F)

 }

	Venn2DE <- function(button, user.data) {
		res <- NULL
		d1 <- NULL
  d2 <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file1 <- filename1$getText()
		the.sep1 <- sepEntry1$getText()
		the.headers1 <- headersEntry1$active
		d1 <- read.table(the.file1,sep=the.sep1,header=the.headers1,row.names=1)

		the.file2 <- filename2$getText()
		the.sep2 <- sepEntry2$getText()
		the.headers2 <- headersEntry2$active
		d2 <- read.table(the.file2,sep=the.sep2,header=the.headers2,row.names=1)

  label1 <- labelEntry1$getText()
  label2 <- labelEntry2$getText()

		# Select numerical variables
		numVar1 <- sapply(1:ncol(d1),function(x){is.numeric(d1[,x])})
  numVar2 <- sapply(1:ncol(d2),function(x){is.numeric(d2[,x])})
		if (sum(numVar1)==0 || sum(numVar2)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
			# Run 'Venn' on numerical variables  
   res = Venn2de(d1,d2,the.file1,the.file2,label1,label2,Project)                   
		}

 rm(list = ls())
	} #End of function

	Venn3DE <- function(button, user.data) {
  res <- NULL
		d1 <- NULL
  d2 <- NULL
  d3 <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file1 <- filename1$getText()
		the.sep1 <- sepEntry1$getText()
		the.headers1 <- headersEntry1$active
		d1 <- read.table(the.file1,sep=the.sep1,header=the.headers1,row.names=1)

		the.file2 <- filename2$getText()
		the.sep2 <- sepEntry2$getText()
		the.headers2 <- headersEntry2$active
		d2 <- read.table(the.file2,sep=the.sep2,header=the.headers2,row.names=1)

		the.file3 <- filename3$getText()
		the.sep3 <- sepEntry3$getText()
		the.headers3 <- headersEntry3$active
		d3 <- read.table(the.file3,sep=the.sep3,header=the.headers3,row.names=1)

  label1 <- labelEntry1$getText()
  label2 <- labelEntry2$getText()
  label3 <- labelEntry3$getText()

		# Select numerical variables
		numVar1 <- sapply(1:ncol(d1),function(x){is.numeric(d1[,x])})
  numVar2 <- sapply(1:ncol(d2),function(x){is.numeric(d2[,x])})
  numVar3 <- sapply(1:ncol(d3),function(x){is.numeric(d3[,x])})
		if (sum(numVar1)==0 || sum(numVar2)==0 ||  sum(numVar3)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
			# Run 'Venn3de' on numerical variables  
   res = Venn3de(d1,d2,d3,the.file1,the.file2,the.file3,label1,label2,label3,Project)                   
		}
 rm(list = ls())		
	} #End of function

Venn4DE <- function(button, user.data) {
  res <- NULL
		d1 <- NULL
  d2 <- NULL
  d3 <- NULL
  d4 <- NULL
		error <- NULL
		warning <- NULL
		# Get the information about data and the file
		the.file1 <- filename1$getText()
		the.sep1 <- sepEntry1$getText()
		the.headers1 <- headersEntry1$active
		d1 <- read.table(the.file1,sep=the.sep1,header=the.headers1,row.names=1)

		the.file2 <- filename2$getText()
		the.sep2 <- sepEntry2$getText()
		the.headers2 <- headersEntry2$active
		d2 <- read.table(the.file2,sep=the.sep2,header=the.headers2,row.names=1)

		the.file3 <- filename3$getText()
		the.sep3 <- sepEntry3$getText()
		the.headers3 <- headersEntry3$active
		d3 <- read.table(the.file3,sep=the.sep3,header=the.headers3,row.names=1)

		the.file4 <- filename4$getText()
		the.sep4 <- sepEntry4$getText()
		the.headers4 <- headersEntry4$active
		d4 <- read.table(the.file4,sep=the.sep4,header=the.headers4,row.names=1)

  label1 <- labelEntry1$getText()
  label2 <- labelEntry2$getText()
  label3 <- labelEntry3$getText()
  label4 <- labelEntry4$getText()

		# Select numerical variables
		numVar1 <- sapply(1:ncol(d1),function(x){is.numeric(d1[,x])})
  numVar2 <- sapply(1:ncol(d2),function(x){is.numeric(d2[,x])})
  numVar3 <- sapply(1:ncol(d3),function(x){is.numeric(d3[,x])})
  numVar4 <- sapply(1:ncol(d4),function(x){is.numeric(d4[,x])})
		if (sum(numVar1)==0 || sum(numVar2)==0 ||  sum(numVar3)==0 || sum(numVar4)==0) {
			error <- "ERROR: No numerical variables in the data!"
		} else {
			# Run 'Venn4de' on numerical variables  
   res = Venn4de(d1,d2,d3,d4,the.file1,the.file2,the.file3,the.file4,label1,label2,label3,label4,Project)                   
		}


if (!is.null(res)) {
			########## Start dialog...
			# Open a dialog box to print results
			dialog <- gtkDialogNewWithButtons("VennDiagram 4 sets DE Generated!",window, "modal","gtk-ok", GtkResponseType["ok"],"gtk-quit", GtkResponseType["cancel"])
		
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

				hbox <- gtkHBoxNew(FALSE,20)
				vbox$packStart(hbox, FALSE, FALSE, 0)
				label <- gtkLabelNew(res)
				hbox$packStart(label,FALSE,FALSE,0)
			
			
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
   label1 <- gtkLabelNewWithMnemonic("Select the result files to compare by clicking on the corresponding Open buttons.")
   label2 <- gtkLabelNewWithMnemonic("Use the First label, Second label (and/or Third and/or Fourth) label fields to label the chosen files.")
   label3 <- gtkLabelNewWithMnemonic("Click on the Venn Diagrams button.")
   label4 <- gtkLabelNewWithMnemonic("If you selected 2 files then click VennDiagram 2 sets DE button.")
   label5 <- gtkLabelNewWithMnemonic("If you selected 3 files then click VennDiagram 3 sets DE button.")
   label5 <- gtkLabelNewWithMnemonic("If you selected 4 files then click VennDiagram 4 sets DE button.")
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
 title=paste("   \n\n    Result Comparison Interface is ready to work on ",Project, " project.", sep="")
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
	label1 <- gtkLabelNewWithMnemonic("_Select the First       file to Compare")
	hbox$packStart(label1,FALSE,FALSE,0)
	# Add entry in the second column; named "filename"
	filename1 <- gtkEntryNew()
	filename1$setWidthChars(30)
	label1$setMnemonicWidget(filename1)
	hbox$packStart(filename1,FALSE,FALSE,0)
	buttonOpen1 <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen1, "clicked", openFile1)
	hbox$packStart(buttonOpen1,FALSE,FALSE,0)


	# are headers included in the file?
	label <- gtkLabelNewWithMnemonic("_Headers?")
	hbox$packStart(label,FALSE,FALSE,0)
	headersEntry1 <- gtkCheckButton()
	headersEntry1$active <- TRUE
	hbox$packStart(headersEntry1,FALSE,FALSE,0)
	label$setMnemonicWidget(headersEntry1)
	
	# what's the character used for column separator?
	label <- gtkLabelNewWithMnemonic("_Col Sep")
	hbox$packStart(label,FALSE,FALSE,0)
	sepEntry1 <- gtkEntryNew()
	sepEntry1$setWidthChars(2)
	sepEntry1$setText("")
	hbox$packStart(sepEntry1,FALSE,FALSE,0)
	label$setMnemonicWidget(sepEntry1)

	# what's the label1?
	label <- gtkLabelNewWithMnemonic("_First label?  ")
	hbox$packStart(label,FALSE,FALSE,0)
	labelEntry1 <- gtkEntryNew()
	labelEntry1$setWidthChars(10)
	labelEntry1$setText("")
	hbox$packStart(labelEntry1,FALSE,FALSE,0)
	label$setMnemonicWidget(labelEntry1)

	# Add one horizontal container
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add label in first column
	label2 <- gtkLabelNewWithMnemonic("_Select the Second file to Compare")
	hbox$packStart(label2,FALSE,FALSE,0)
	# Add entry in the second column; named "filename"
	filename2 <- gtkEntryNew()
	filename2$setWidthChars(30)
	label2$setMnemonicWidget(filename2)
	hbox$packStart(filename2,FALSE,FALSE,0)
	buttonOpen2 <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen2, "clicked", openFile2)
	hbox$packStart(buttonOpen2,FALSE,FALSE,0)

	# are headers included in the file?
	label <- gtkLabelNewWithMnemonic("_Headers?")
	hbox$packStart(label,FALSE,FALSE,0)
	headersEntry2 <- gtkCheckButton()
	headersEntry2$active <- TRUE
	hbox$packStart(headersEntry2,FALSE,FALSE,0)
	label$setMnemonicWidget(headersEntry2)
	
	# what's the character used for column separator?
	label <- gtkLabelNewWithMnemonic("_Col Sep")
	hbox$packStart(label,FALSE,FALSE,0)
	sepEntry2 <- gtkEntryNew()
	sepEntry2$setWidthChars(2)
	sepEntry2$setText("")
	hbox$packStart(sepEntry2,FALSE,FALSE,0)
	label$setMnemonicWidget(sepEntry2)

	# what's the label2?
	label <- gtkLabelNewWithMnemonic("_Second label?")
	hbox$packStart(label,FALSE,FALSE,0)
	labelEntry2 <- gtkEntryNew()
	labelEntry2$setWidthChars(10)
	labelEntry2$setText("")
	hbox$packStart(labelEntry2,FALSE,FALSE,0)
	label$setMnemonicWidget(labelEntry2)

	# Add one horizontal container
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add label in first column
	label3 <- gtkLabelNewWithMnemonic("_Select the Third      file to Compare")
	hbox$packStart(label3,FALSE,FALSE,0)
	# Add entry in the second column; named "filename"
	filename3 <- gtkEntryNew()
	filename3$setWidthChars(30)
	label3$setMnemonicWidget(filename3)
	hbox$packStart(filename3,FALSE,FALSE,0)
	buttonOpen3 <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen3, "clicked", openFile3)
	hbox$packStart(buttonOpen3,FALSE,FALSE,0)

	# are headers included in the file?
	label <- gtkLabelNewWithMnemonic("_Headers?")
	hbox$packStart(label,FALSE,FALSE,0)
	headersEntry3 <- gtkCheckButton()
	headersEntry3$active <- TRUE
	hbox$packStart(headersEntry3,FALSE,FALSE,0)
	label$setMnemonicWidget(headersEntry3)
	
	# what's the character used for column separator?
	label <- gtkLabelNewWithMnemonic("_Col Sep")
	hbox$packStart(label,FALSE,FALSE,0)
	sepEntry3 <- gtkEntryNew()
	sepEntry3$setWidthChars(2)
	sepEntry3$setText("")
	hbox$packStart(sepEntry3,FALSE,FALSE,0)
	label$setMnemonicWidget(sepEntry3)

	# what's the label3?
	label <- gtkLabelNewWithMnemonic("_Third label?  ")
	hbox$packStart(label,FALSE,FALSE,0)
	labelEntry3 <- gtkEntryNew()
	labelEntry3$setWidthChars(10)
	labelEntry3$setText("")
	hbox$packStart(labelEntry3,FALSE,FALSE,0)
	label$setMnemonicWidget(labelEntry3)

# Add one horizontal container
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add label in first column
	label4 <- gtkLabelNewWithMnemonic("_Select the Fourth   file to Compare")
	hbox$packStart(label4,FALSE,FALSE,0)
	# Add entry in the second column; named "filename"
	filename4 <- gtkEntryNew()
	filename4$setWidthChars(30)
	label4$setMnemonicWidget(filename4)
	hbox$packStart(filename4,FALSE,FALSE,0)
	buttonOpen4 <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen4, "clicked", openFile4)
	hbox$packStart(buttonOpen4,FALSE,FALSE,0)

	# are headers included in the file?
	label <- gtkLabelNewWithMnemonic("_Headers?")
	hbox$packStart(label,FALSE,FALSE,0)
	headersEntry4 <- gtkCheckButton()
	headersEntry4$active <- TRUE
	hbox$packStart(headersEntry4,FALSE,FALSE,0)
	label$setMnemonicWidget(headersEntry4)
	
	# what's the character used for column separator?
	label <- gtkLabelNewWithMnemonic("_Col Sep")
	hbox$packStart(label,FALSE,FALSE,0)
	sepEntry4 <- gtkEntryNew()
	sepEntry4$setWidthChars(2)
	sepEntry4$setText("")
	hbox$packStart(sepEntry4,FALSE,FALSE,0)
	label$setMnemonicWidget(sepEntry4)

	# what's the label4?
	label <- gtkLabelNewWithMnemonic("_Fourth label?")
	hbox$packStart(label,FALSE,FALSE,0)
	labelEntry4 <- gtkEntryNew()
	labelEntry4$setWidthChars(10)
	labelEntry4$setText("")
	hbox$packStart(labelEntry4,FALSE,FALSE,0)
	label$setMnemonicWidget(labelEntry4)


####################################################################################################################################
####################################################################################################################################

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

 # button
	noi <- gtkButtonNewWithMnemonic("How to use this Interface", show = TRUE)
	gSignalConnect(noi, "clicked", infoFun)
	the.buttons$packStart(noi,fill=F)

 # button
	venn <- gtkButtonNewWithMnemonic("_Venn Diagrams", show = TRUE)
	gSignalConnect(venn, "clicked", vennfun)
	the.buttons$packStart(venn,fill=F)

}

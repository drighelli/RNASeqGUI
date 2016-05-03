calculateGUI12 <- function(project_name) {

  Project <- project_name
  message=paste("You are using the project: ",Project,sep="")
  print(message)
  
  openFile <- function(button,user.data) {
    dialog <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
    dialog$setCurrentFolder("~/")
    if (dialog$run() == GtkResponseType["accept"]) {
      the.sel.file <- dialog$getFilename()
      filename$setText(the.sel.file)
      dialog$destroy()
    } else {
      dialog$destroy()
    }
  }
  

  filtering <- function(button, user.data) {
    
    res <- NULL
    d <- NULL
		  error <- NULL
  		warning <- NULL

	  	the.sep <- sepEntry$getText()
		  the.headers <- headersEntry$active
    file.name <- filename$getText()
    type1 <- set1$active
    type2 <- set2$active
    type3 <- set3$active
    norm <- normCheck$active
    cpm <- cpmEntry$getText()
    factors <- factorsEntry$getText()
    conditions=unlist(strsplit(factors, ","))
    cv.cutoff <- cv.cutoffEntry$getText()
    d <- read.table( file.name,sep=the.sep,header=the.headers,row.names=1) 

    if(file.name=='')
    {
      errorstring <-'ERROR: filename empty, enter valid filename!'
      print(errorstring)
      dialog <- gtkDialogNewWithButtons(" ERROR!",window, "modal","gtk-ok", GtkResponseType["ok"])
      dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
      
      # Create vertical container for file name entry
      vbox <- gtkVBoxNew(FALSE, 8)
      vbox$setBorderWidth(24)
      dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
      label1 <- gtkLabelNewWithMnemonic(" filename empty, enter valid filename!") 
      vbox$packStart(label1,FALSE,FALSE,0)
      response <- dialog$run()
      # Return to previous window
      if (response == GtkResponseType["ok"]) {dialog$destroy()}	
      return()
    }
    print("expression data selected: ")
    print(file.name)

	  	# Select numerical variables
	  	numVar <- sapply(1:ncol(d),function(x){is.numeric(d[,x])})
	  	if (sum(numVar)==0) {
		   	error <- "ERROR: No numerical variables in the data!"
	  	} else {
       res <- filteringfunction(d,file.name,conditions,type1,type2,type3,cv.cutoff,norm,cpm,Project)
        #PrintFiltering(d,the.file=file.name,conditions,type1,type2,type3,cv.cutoff,norm,cpm,Project)
			# Get saving options and save if needed
			isToSave <- toSave$active
			exportName <- exportfilename$getText()
			if ((isToSave)&(exportName=="")) {
				warning <- "Invalid user data: a file name must be specified if you want to save results."
			} else {
				where.sep <- gregexpr("/", file.name)[[1]]
				where.sep <- max(where.sep)
				save.dir <- substr( file.name,1,where.sep)
				write.table(res,file=paste("RNASeqGUI_Projects/",Project,"/Results/",exportName,".csv",sep=""))
			}
		}
		if (!is.null(res)) {
			########## Start dialog...
			# Open a dialog box to print results
			dialog <- gtkDialogNewWithButtons("Filtering finished!",window, "modal","gtk-ok", GtkResponseType["ok"],"gtk-quit", GtkResponseType["cancel"])
		
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
  res = NoiseqShowresults(Project) # MAIN FUNCTION HERE
  res
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
     label1 <- gtkLabelNewWithMnemonic("Select expression data by clicking on the corresponding 'Open' button.")
     label2 <- gtkLabelNewWithMnemonic("In the 'Factors?' field, specify the factors of the expression data separated by columns.")
     label3 <- gtkLabelNewWithMnemonic("Choose a 'cv.cutoff' that is a cutoff for the coefficient of variation per condition to") 
     label4 <- gtkLabelNewWithMnemonic("eliminate features with inconsistent expression values. To be used only for CPM method.") 
     label5 <- gtkLabelNewWithMnemonic("Choose a 'cpm' (counts per million)  under which a feature is considered to have low counts in a sample.")
     label6 <- gtkLabelNewWithMnemonic("The 'cpm' for a condition with s samples is cpm x s. To be used only for CPM method and the Proportion test method.") 
     label7 <- gtkLabelNewWithMnemonic("Check the norm checkbox to specify if the data have been normalized or not.")
     label8 <- gtkLabelNewWithMnemonic("Select with the radio button the method to be used: CPM, Wilcoxon test or Proportional test.")
     label9 <- gtkLabelNewWithMnemonic("Finally, click on 'Filter' button.")
     label10 <- gtkLabelNewWithMnemonic("For further information, see http://www.bioconductor.org/packages/release/bioc/vignettes/NOISeq/inst/doc/NOISeq.pdf at page 14.")
     
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
    response <- dialog$run()
    # Return to previous window
    if (GtkResponseType["ok"]) { dialog$destroy() }			 
  }
  
  
  
  ######################################################
  
  # Create window
  window <- gtkWindow()
  # Add title
  window["title"] <- "RNASeqGUI"
  
  # Add a frame
  title=paste("    \n Filtering Interface is ready to work on ",Project, " project.", sep="")
  frame <- gtkFrameNew(title)
  window$add(frame)
  
  # Create vertical container for file name entry
  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)
  
  ###########################################################################

  # Add horizontal container for every widget line
  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)
  
  # Add label in first column
  label <- gtkLabelNewWithMnemonic("_Select expression data")
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
  label <- gtkLabelNewWithMnemonic(" Choose the options for the filtering ")
  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  # factors
  label <- gtkLabelNewWithMnemonic("_Factors?")
  hbox$packStart(label,FALSE,FALSE,0)
  factorsEntry <- gtkEntryNew()
  factorsEntry$setWidthChars(60)
  factorsEntry$setText('')
  hbox$packStart(factorsEntry,FALSE,FALSE,0)
  label$setMnemonicWidget(factorsEntry)

  # cv.cutoff
  label <- gtkLabelNewWithMnemonic("_cv.cutoff?")
  hbox$packStart(label,FALSE,FALSE,0)
  cv.cutoffEntry <- gtkEntryNew()
  cv.cutoffEntry$setWidthChars(6)
  cv.cutoffEntry$setText('100')
  hbox$packStart(cv.cutoffEntry,FALSE,FALSE,0)
  label$setMnemonicWidget(cv.cutoffEntry)

  # cpm
  label <- gtkLabelNewWithMnemonic("_cpm?")
  hbox$packStart(label,FALSE,FALSE,0)
  cpmEntry <- gtkEntryNew()
  cpmEntry$setWidthChars(4)
  cpmEntry$setText("0.5")
  hbox$packStart(cpmEntry,FALSE,FALSE,0)
  label$setMnemonicWidget(cpmEntry)

  #norm
  label <- gtkLabelNewWithMnemonic("_norm?")
  hbox$packStart(label,FALSE,FALSE,0)
  normCheck <- gtkCheckButton()
  hbox$packStart(normCheck,FALSE,FALSE,0)
  
  # Add horizontal container for every widget line
  hbox <- gtkHBoxNew(FALSE, 8)
  label <- gtkLabelNewWithMnemonic(" Select the type of Filtering ")
  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)
  
  ## Create a radio button with a GtkEntry widget 
  set1 <- gtkRadioButtonNewWithLabel(group = NULL,"CPM", show = TRUE)
  #The two non-default options should only be used when rank.test = FALSE.
  ## Create a radio button with a label 
  set2 <- gtkRadioButtonNewWithLabelFromWidget(set1,"Wilcoxon test")
  ## Create a radio button with a label 
  set3 <- gtkRadioButtonNewWithLabelFromWidget(set2,"Proportion test")

  
  ## Pack them into a box, then show all the widgets
  hbox$packStart(set1, TRUE, TRUE, 2)
  hbox$packStart(set2, TRUE, TRUE, 2)
  hbox$packStart(set3, TRUE, TRUE, 2)
  label$setMnemonicWidget(set1)  
  label$setMnemonicWidget(set2) 
  label$setMnemonicWidget(set3) 

  # Add an horizontal container 
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

 
  # Add separator
  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)
  
  label <- gtkLabelNewWithMnemonic("_Start Filtering Process")
  vbox$packStart(label,FALSE,FALSE,0)
  
  # Add button
  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)
  
  # button
  info <- gtkButtonNewWithMnemonic("How to use this Interface", show = TRUE)
  gSignalConnect(info, "clicked", infoFun)
  the.buttons$packStart(info,fill=F)
  
  # button
  filterbut <- gtkButtonNewWithMnemonic("Filter", show = TRUE)
  gSignalConnect(filterbut, "clicked", filtering)
  the.buttons$packStart(filterbut,fill=F)
  
}

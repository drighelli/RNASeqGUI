calculateGUI9 <- function(project_name) {

 Project <- project_name
 message=paste("You are using the project: ",Project,sep="")
 print(message)

	openFile <- function(button,user.data) {
		dialog <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
		dialog$setCurrentFolder( paste( getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",sep="") )
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

	openRes <- function(button,user.data) {
		dialog <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
		dialog$setCurrentFolder( paste( getwd(),"/RNASeqGUI_Projects/",Project,"/Results/",sep="") )
		if (dialog$run() == GtkResponseType["accept"]) {
			the.sel.res <- dialog$getFilename()
			resFile$setText(the.sel.res)
			dialog$destroy()
		} else {
			dialog$destroy()
		}
	}


gageAnalysis <- function(button, user.data) {
		res <- NULL
  file.name <- NULL
	 file.name <- filename$getText()
  print("expression data selected: ")
  print(file.name)
  hsa <-  radio1$active
  mmu <-  radio2$active
  dme <-  radio3$active
  dre <-  radio4$active
  geneSet1 <- set1$active
  geneSet2 <- set2$active
  Convers <- Convers$active
  Convers3 <- Convers3$active
  Convers4 <- Convers4$active
  pathn <- pathnum$getText()

 if(Sys.info()[[1]]=="Windows"){
  
    # I need this to get "C:\\Users\\utente\\Downloads\\example_RNASeqGUI\\demo\\" from 
    # "C:\\Users\\utente\\Downloads\\example_RNASeqGUI\\demo\\2L_1.bam"
  
	   res = gage_function(file.name,geneSet1,geneSet2,Project,hsa,mmu,dme,dre,Convers,Convers3,Convers4,pathn)
      
		  # Get saving options and save if needed
		  isToSave <- toSave$active
		  exportName <- exportfilename$getText()
		  if ((isToSave)&(exportName=="")) {
		    warning <- "Invalid user data: a file name must be specified if you want to save results."
		  } else {
		    write.table(res,file=paste("RNASeqGUI_Projects\\",Project,"\\Results\\",exportName,".csv",sep=""))
		  }
		  if (!is.null(res)) {
		    ########## Start dialog...
		    # Open a dialog box to print results
		    dialog <- gtkDialogNewWithButtons("Done !",window, "modal","gtk-ok", GtkResponseType["ok"],"gtk-quit", GtkResponseType["cancel"])
		    
		    # Create vertical container for file name entry
		    vbox <- gtkVBoxNew(FALSE, 8)
		    vbox$setBorderWidth(24)
		    dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
		    
		    # If results are saved, print information about it
		    if ((isToSave)&(exportName!="")) {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The ", exportName,".csv file has been saved in RNASeqGUI_Projects\\",Project,"\\Results\\ \n \n and several plots have been saved in the 'Plots' folder.", sep=""))
		      hbox$packStart(label,FALSE,FALSE,0)
		    }
		    
		    # If results are NOT saved, print information about it
		    if (exportName=="") {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The result file has been produced and saved in RNASeqGUI_Projects\\",Project,"\\Results\\ \n \n and several plots have been saved in the 'Plots' folder.", sep=""))
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
      
 }else{

	  res = gage_function(file.name,geneSet1,geneSet2,Project,hsa,mmu,dme,dre,Convers,Convers3,Convers4,pathn)
  
  	# Get saving options and save if needed
			isToSave <- toSave$active
			exportName <- exportfilename$getText()
			if ((isToSave)&(exportName=="")) {
				warning <- "Invalid user data: a file name must be specified if you want to save results."
			} else {
				write.table(res,file=paste("RNASeqGUI_Projects/",Project,"/Results/",exportName,".csv",sep=""))
			}
	 	if (!is.null(res)) {
			########## Start dialog...
			# Open a dialog box to print results
			dialog <- gtkDialogNewWithButtons("Done!!!",window, "modal","gtk-ok", GtkResponseType["ok"],"gtk-quit", GtkResponseType["cancel"])

      # Create vertical container for file name entry
		    vbox <- gtkVBoxNew(FALSE, 8)
		    vbox$setBorderWidth(24)
		    dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)

		    # If results are saved, print information about it
		    if ((isToSave)&(exportName!="")) {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The ", exportName,".csv file has been saved in RNASeqGUI_Projects/",Project,"/Results/ \n and several plots have been saved in the 'Plots' folder.", sep=""))
		      hbox$packStart(label,FALSE,FALSE,0)
		    }	
		    
		    # If results are NOT saved, print information about it
		    if (exportName=="") {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The result file has been produced and saved in RNASeqGUI_Projects/",Project,"/Results/ \n and several plots have been saved in the 'Plots' folder.", sep=""))
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
 }
 #rm(list = ls())
	} #End of function


heatmapkeggfun <- function(button, user.data) {

  res <- NULL
	 res <- resFile$getText()
  print("Res data selected: ")
  print(res)
  hsa <-  radio1$active
  mmu <-  radio2$active
  dme <-  radio3$active
  dre <-  radio4$active
  geneSet1 <- set1$active
  geneSet2 <- set2$active
  conversion <- conversion$active
  conversion3 <- conversion3$active
  num_of_path  <- num_of_path$getText()
  countFile  <- countFile$getText()
  control <- controlEntry$getText()
  control  <- unlist(strsplit(control, ","))
  treated <- treatedEntry$getText()
  treated  <- unlist(strsplit(treated, ","))

if(Sys.info()[[1]]=="Windows"){
  
  # I need this to get "C:\\Users\\utente\\Downloads\\example_RNASeqGUI\\demo\\" from 
  # "C:\\Users\\utente\\Downloads\\example_RNASeqGUI\\demo\\2L_1.bam"
  
	  res = kegggoheatmap_function(res,geneSet1,geneSet2,Project,hsa,mmu,dme,dre,conversion,conversion3,num_of_path,countFile,control,treated)
      
		  # Get saving options and save if needed
		  isToSave <- toSave$active
		  exportName <- exportfilename$getText()
		  if ((isToSave)&(exportName=="")) {
		    warning <- "Invalid user data: a file name must be specified if you want to save results."
		  } else {
		    write.table(res,file=paste("RNASeqGUI_Projects\\",Project,"\\Results\\",exportName,".csv",sep=""))
		  }
		  if (!is.null(res)) {
		    ########## Start dialog...
		    # Open a dialog box to print results
		    dialog <- gtkDialogNewWithButtons("Done !",window, "modal","gtk-ok", GtkResponseType["ok"],"gtk-quit", GtkResponseType["cancel"])
		    
		    # Create vertical container for file name entry
		    vbox <- gtkVBoxNew(FALSE, 8)
		    vbox$setBorderWidth(24)
		    dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
		    
		    # If results are saved, print information about it
		    if ((isToSave)&(exportName!="")) {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The ", exportName,".csv file has been saved in RNASeqGUI_Projects\\",Project,"\\Results\\ \n \n and several plots have been saved in the 'Plots' folder.", sep=""))
		      hbox$packStart(label,FALSE,FALSE,0)
		    }
		    
		    # If results are NOT saved, print information about it
		    if (exportName=="") {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The result file has been produced and saved in RNASeqGUI_Projects\\",Project,"\\Results\\ \n \n and several plots have been saved in the 'Plots' folder.", sep=""))
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
      
}else{

	  res = kegggoheatmap_function(res,geneSet1,geneSet2,Project,hsa,mmu,dme,dre,conversion,conversion3,num_of_path,countFile,control,treated)
  
	# Get saving options and save if needed
			isToSave <- toSave$active
			exportName <- exportfilename$getText()
			if ((isToSave)&(exportName=="")) {
				warning <- "Invalid user data: a file name must be specified if you want to save results."
			} else {
				write.table(res,file=paste("RNASeqGUI_Projects/",Project,"/Results/",exportName,".csv",sep=""))
			}
		if (!is.null(res)) {
			########## Start dialog...
			# Open a dialog box to print results
			dialog <- gtkDialogNewWithButtons("Done!!!",window, "modal","gtk-ok", GtkResponseType["ok"],"gtk-quit", GtkResponseType["cancel"])

      # Create vertical container for file name entry
		    vbox <- gtkVBoxNew(FALSE, 8)
		    vbox$setBorderWidth(24)
		    dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)

		    # If results are saved, print information about it
		    if ((isToSave)&(exportName!="")) {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The ", exportName,".csv file has been saved in RNASeqGUI_Projects/",Project,"/Results/ \n and several plots have been saved in the 'Plots' folder.", sep=""))
		      hbox$packStart(label,FALSE,FALSE,0)
		    }	
		    
		    # If results are NOT saved, print information about it
		    if (exportName=="") {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The result file has been produced and saved in RNASeqGUI_Projects/",Project,"/Results/ \n and several plots have been saved in the 'Plots' folder.", sep=""))
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
}
 rm(list = ls())
	} #End of function

barplotpathwayfun <- function(button, user.data) {
		res <- NULL
  file.name <- NULL
	 file.name <- resFile$getText()
  print("expression data selected: ")
  print(file.name)
  pathn <- pathnum$getText()

 if(Sys.info()[[1]]=="Windows"){
  
  # I need this to get "C:\\Users\\utente\\Downloads\\example_RNASeqGUI\\demo\\" from 
  # "C:\\Users\\utente\\Downloads\\example_RNASeqGUI\\demo\\2L_1.bam"
  
	  res = barplotpathway(file.name,Project,pathn)
      
		  # Get saving options and save if needed
		  isToSave <- toSave$active
		  exportName <- exportfilename$getText()
		  if ((isToSave)&(exportName=="")) {
		    warning <- "Invalid user data: a file name must be specified if you want to save results."
		  } else {
		    write.table(res,file=paste("RNASeqGUI_Projects\\",Project,"\\Results\\",exportName,".csv",sep=""))
		  }
		  if (!is.null(res)) {
		    ########## Start dialog...
		    # Open a dialog box to print results
		    dialog <- gtkDialogNewWithButtons("Done !",window, "modal","gtk-ok", GtkResponseType["ok"],"gtk-quit", GtkResponseType["cancel"])
		    
		    # Create vertical container for file name entry
		    vbox <- gtkVBoxNew(FALSE, 8)
		    vbox$setBorderWidth(24)
		    dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
		    
		    # If results are saved, print information about it
		    if ((isToSave)&(exportName!="")) {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The ", exportName,".csv file has been saved in RNASeqGUI_Projects\\",Project,"\\Results\\ \n \n and several plots have been saved in the 'Plots' folder.", sep=""))
		      hbox$packStart(label,FALSE,FALSE,0)
		    }
		    
		    # If results are NOT saved, print information about it
		    if (exportName=="") {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The result file has been produced and saved in RNASeqGUI_Projects\\",Project,"\\Results\\ \n \n and several plots have been saved in the 'Plots' folder.", sep=""))
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
      
 }else{

	  res = barplotpathway(file.name,Project,pathn)
  
	# Get saving options and save if needed
			isToSave <- toSave$active
			exportName <- exportfilename$getText()
			if ((isToSave)&(exportName=="")) {
				warning <- "Invalid user data: a file name must be specified if you want to save results."
			} else {
				write.table(res,file=paste("RNASeqGUI_Projects/",Project,"/Results/",exportName,".csv",sep=""))
			}
		if (!is.null(res)) {
			########## Start dialog...
			# Open a dialog box to print results
			dialog <- gtkDialogNewWithButtons("Done!!!",window, "modal","gtk-ok", GtkResponseType["ok"],"gtk-quit", GtkResponseType["cancel"])

      # Create vertical container for file name entry
		    vbox <- gtkVBoxNew(FALSE, 8)
		    vbox$setBorderWidth(24)
		    dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)

		    # If results are saved, print information about it
		    if ((isToSave)&(exportName!="")) {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The ", exportName,".csv file has been saved in RNASeqGUI_Projects/",Project,"/Results/ \n and several plots have been saved in the 'Plots' folder.", sep=""))
		      hbox$packStart(label,FALSE,FALSE,0)
		    }	
		    
		    # If results are NOT saved, print information about it
		    if (exportName=="") {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The result file has been produced and saved in RNASeqGUI_Projects/",Project,"/Results/ \n and several plots have been saved in the 'Plots' folder.", sep=""))
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
 }
 #rm(list = ls())
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
   label1 <- gtkLabelNewWithMnemonic("Select a FULL expression file by clicking on the corresponding 'Open' button.")
   label2 <- gtkLabelNewWithMnemonic("Select either Pathway Analysis or GO Analysis.")
   label3 <- gtkLabelNewWithMnemonic("Select the organism you are investigating on.")
   label5 <- gtkLabelNewWithMnemonic("Specify the gene id type you have in the expression file.")
   label6 <- gtkLabelNewWithMnemonic("Finally, click on 'GAGE' button.")
   label7 <- gtkLabelNewWithMnemonic("For further information, see http://bioconductor.org/packages/release/bioc/html/gage.html .")
   label8 <- gtkLabelNewWithMnemonic("If you want to produce an Heatmap, select")
   label2 <- gtkLabelNewWithMnemonic("Select either Pathway Analysis or GO Analysis.")
   label3 <- gtkLabelNewWithMnemonic("Select the organism you are investigating on.")
   label9 <- gtkLabelNewWithMnemonic("the starting count file and the PATHWAY/GO result file.")
   label10 <- gtkLabelNewWithMnemonic("Specify the gene id type you have in the count file.")
   label11 <- gtkLabelNewWithMnemonic("Specify which columns are the treated and which are the control ones in the count file.")
   label12 <- gtkLabelNewWithMnemonic("Specify the number of the Path/GOterm for which you want to create an heatmap.")
   label13 <- gtkLabelNewWithMnemonic("Finally, click on the 'Heatmap' button.")
   label14 <- gtkLabelNewWithMnemonic("If you want to produce a Barplot, select a PATHWAY/GO result file.")
   label15 <- gtkLabelNewWithMnemonic("Specify from which Path/GO term you want to create a Barplot.")
   label16 <- gtkLabelNewWithMnemonic("by choosing a starting point in the 'Barplot from path number' field.")
   label17 <- gtkLabelNewWithMnemonic("Finally, click on the 'Barplot' button.")

         vbox$packStart(label1,FALSE,FALSE,0)
         vbox$packStart(label2,FALSE,FALSE,0)
        	vbox$packStart(label3,FALSE,FALSE,0)
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
		       response <- dialog$run()
		      	# Return to previous window
			      if (GtkResponseType["ok"]) { dialog$destroy() }			 
} #End of function

######################################################

# Create window
	window <- gtkWindow()
	# Add title
	window["title"] <- "RNASeqGUI"

	# Add a frame
 title=paste("    \n Gage Interface is ready to work on ",Project, " project.", sep="")
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
	label <- gtkLabelNewWithMnemonic("_Select a FULL expression file")
	hbox$packStart(label,FALSE,FALSE,0)
	# Add entry in the second column; named "filename"
	filename <- gtkEntryNew()
	filename$setWidthChars(50)
	label$setMnemonicWidget(filename)
	hbox$packStart(filename,FALSE,FALSE,0)
	buttonOpen <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen, "clicked", openFile)
	hbox$packStart(buttonOpen,FALSE,FALSE,0)

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	label <- gtkLabelNewWithMnemonic("Select the analysis you want to perform")
	vbox$packStart(label,FALSE,FALSE,0)
	vbox$packStart(hbox, FALSE, FALSE, 0)
 
  ## Create a radio button with a GtkEntry widget 
  set1 <- gtkRadioButtonNewWithLabel(group = NULL,"Pathway Analysis (KEGG)", show = TRUE)
  #The two non-default options should only be used when rank.test = FALSE.
  ## Create a radio button with a label 
  set2 <- gtkRadioButtonNewWithLabelFromWidget(set1,"Gene Ontology Analysis (GO)")
  
  ## Pack them into a box, then show all the widgets
  hbox$packStart(set1, TRUE, TRUE, 2)
  hbox$packStart(set2, TRUE, TRUE, 2)
	 label$setMnemonicWidget(set1)  
	 label$setMnemonicWidget(set2) 

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	label <- gtkLabelNewWithMnemonic(" Select the organism ")
	vbox$packStart(label,FALSE,FALSE,0)
	vbox$packStart(hbox, FALSE, FALSE, 0)

  ## Create a radio button with a GtkEntry widget 
  radio1 <- gtkRadioButtonNewWithLabel(group = NULL,"hsapiens", show = TRUE)
  #The two non-default options should only be used when rank.test = FALSE.
  ## Create a radio button with a label 
  radio2 <- gtkRadioButtonNewWithLabelFromWidget(radio1,"mmusculus")
  ## Create a radio button with a label
  radio3 <- gtkRadioButtonNewWithLabelFromWidget(radio2,"dmelanogaster")
  ## Create a radio button with a label 
  radio4 <- gtkRadioButtonNewWithLabelFromWidget(radio3,"drerio")
  
  ## Pack them into a box, then show all the widgets
  hbox$packStart(radio1, TRUE, TRUE, 2)
  hbox$packStart(radio2, TRUE, TRUE, 2)
  hbox$packStart(radio3, TRUE, TRUE, 2)
  hbox$packStart(radio4, TRUE, TRUE, 2)
	 label$setMnemonicWidget(radio1)  
	 label$setMnemonicWidget(radio2) 
	 label$setMnemonicWidget(radio3)  
	 label$setMnemonicWidget(radio4) 

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Conversion
	label <- gtkLabelNewWithMnemonic("_Check if you have ENSEMBL ids in your file")
	hbox$packStart(label,FALSE,FALSE,0)
	Convers <- gtkCheckButton()
	Convers$active <- TRUE
	hbox$packStart(Convers,FALSE,FALSE,0)
	label$setMnemonicWidget(Convers) 

 # Conversion3
	label <- gtkLabelNewWithMnemonic("_Check if you have gene names in your file")
	hbox$packStart(label,FALSE,FALSE,0)
	Convers3 <- gtkCheckButton()
	Convers3$active <- FALSE
	hbox$packStart(Convers3,FALSE,FALSE,0)
	label$setMnemonicWidget(Convers3)  

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Conversion4
	label <- gtkLabelNewWithMnemonic("_Check if you have GENCODEs in your file")
	hbox$packStart(label,FALSE,FALSE,0)
	Convers4 <- gtkCheckButton()
	Convers4$active <- FALSE
	hbox$packStart(Convers4,FALSE,FALSE,0)
	label$setMnemonicWidget(Convers4)    

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
	gagebut <- gtkButtonNewWithMnemonic("_GAGE", show = TRUE)
	gSignalConnect(gagebut, "clicked", gageAnalysis)
	the.buttons$packStart(gagebut,fill=F)

###########################################################################

	# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_Create Heatmaps and Barplots")
	vbox$packStart(label,FALSE,FALSE,0)

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add label in first column
	label <- gtkLabelNewWithMnemonic("_Select the COUNT data file")
	hbox$packStart(label,FALSE,FALSE,0)
	# Add entry in the second column; named "countFile"
	countFile <- gtkEntryNew()
	countFile$setWidthChars(50)
	label$setMnemonicWidget(countFile)
	hbox$packStart(countFile,FALSE,FALSE,0)
	buttonOpen <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen, "clicked", openCount)
	hbox$packStart(buttonOpen,FALSE,FALSE,0)

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add label in first column
	label <- gtkLabelNewWithMnemonic("_Select the Pathway/GO result file")
	hbox$packStart(label,FALSE,FALSE,0)
	# Add entry in the second column; named "countFile"
	resFile <- gtkEntryNew()
	resFile$setWidthChars(50)
	label$setMnemonicWidget(resFile)
	hbox$packStart(resFile,FALSE,FALSE,0)
	buttonOpen <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen, "clicked", openRes)
	hbox$packStart(buttonOpen,FALSE,FALSE,0)

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # conversion
	label <- gtkLabelNewWithMnemonic("_Check if you have ENSEMBL ids in the COUNT file")
	hbox$packStart(label,FALSE,FALSE,0)
	conversion <- gtkCheckButton()
	conversion$active <- TRUE
	hbox$packStart(conversion,FALSE,FALSE,0)
	label$setMnemonicWidget(conversion) 

 # conversion3
	label <- gtkLabelNewWithMnemonic("_Check if you have GENCODEs in the COUNT file")
	hbox$packStart(label,FALSE,FALSE,0)
	conversion3 <- gtkCheckButton()
	conversion3$active <- FALSE
	hbox$packStart(conversion3,FALSE,FALSE,0)
	label$setMnemonicWidget(conversion3) 

 # Add an horizontal container 
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)
	
	# Add two horizontal containers to check if the results has to be exported in a file and if so, to specify file named

	# treated
	label <- gtkLabelNewWithMnemonic("_Treated?")
	hbox$packStart(label,FALSE,FALSE,0)
	treatedEntry <- gtkEntryNew()
	treatedEntry$setWidthChars(5)
	treatedEntry$setText("3,4")
	hbox$packStart(treatedEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(treatedEntry)

	# control
	label <- gtkLabelNewWithMnemonic("_Control?")
	hbox$packStart(label,FALSE,FALSE,0)
	controlEntry <- gtkEntryNew()
	controlEntry$setWidthChars(5)
	controlEntry$setText("1,2")
	hbox$packStart(controlEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(controlEntry)

 # num_of_path
 label <- gtkLabelNewWithMnemonic("_Number of Path/GOterm?")
	hbox$packStart(label,FALSE,FALSE,0)
	num_of_path <- gtkEntryNew()
	num_of_path$setWidthChars(3)
	num_of_path$setText("1")
	hbox$packStart(num_of_path,FALSE,FALSE,0)
	label$setMnemonicWidget(num_of_path)

	# pathnum
	label <- gtkLabelNewWithMnemonic("_Barplot from path number:")
	hbox$packStart(label,FALSE,FALSE,0)
	pathnum <- gtkEntryNew()
	pathnum$setWidthChars(3)
	pathnum$setText("1")
	hbox$packStart(pathnum,FALSE,FALSE,0)
	label$setMnemonicWidget(pathnum) 

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

 # button
	heatbut <- gtkButtonNewWithMnemonic("_Heatmap", show = TRUE)
	gSignalConnect(heatbut, "clicked", heatmapkeggfun)
	the.buttons$packStart(heatbut,fill=F)

 # button
	barbut <- gtkButtonNewWithMnemonic("_Barplot", show = TRUE)
	gSignalConnect(barbut, "clicked", barplotpathwayfun)
	the.buttons$packStart(barbut,fill=F)


}

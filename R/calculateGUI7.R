calculateGUI7 <-
function(project_name) {

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

	openBam <- function(button,user.data) {
		dialog <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
		dialog$setCurrentFolder("~/")
		if (dialog$run() == GtkResponseType["accept"]) {
			the.sel.Bam <- dialog$getFilename()
			BamFolder$setText(the.sel.Bam)
			dialog$destroy()
		} else {
			dialog$destroy()
		}
	}

 counting_fun <- function(button, user.data) {
		res <- NULL
  d <- NULL
		error <- NULL
		warning <- NULL
  gtf <- NULL
	 gtf <- filename$getText()
  print("GTF selected: ")
  print(	gtf)
  Bam.Folder <- BamFolder$getText()
  Paired <- PairedEndOv$active #logical
  union <-  radio111$active
  IntersectionStrict <-  radio222$active
  IntersectionNonEmpty <- radio333$active
  print("Mode selected: ")
   if (union==TRUE){  # union
      print("Union")
   }
   if (IntersectionStrict==TRUE){  # IntersectionStrict
      print("Intersection Strict")
   }
   if (IntersectionNonEmpty==TRUE){             # IntersectionNonEmpty
      print("Intersection Not Empty")
   }

if(Sys.info()[[1]]=="Windows"){
  
  # I need this to get "C:\\Users\\utente\\Downloads\\example_RNASeqGUI\\demo\\" from 
  # "C:\\Users\\utente\\Downloads\\example_RNASeqGUI\\demo\\2L_1.bam"
  
  print("Bam Folder selected: ")
  print(  Bam.Folder)

  Bam.Folder = strsplit(Bam.Folder,"\\\\")
  Bam.FolderNew = Bam.Folder[[1]][2]
  Bam.FolderNew = paste("C:\\",Bam.FolderNew,sep="")
  Bam.FolderNew
  
  for( i in 4:length(Bam.Folder[[1]])-1)
  {
    Bam.FolderNew = paste(Bam.FolderNew,Bam.Folder[[1]][i],sep="\\")
  }
  print("Bam Folder selected: ")
  print(  Bam.FolderNew)
  print("PLEASE WAIT! ")
  Single <- !Paired
       if (Single == TRUE){print("summarizeOverlaps is working on single-end reads.")
                          }else{print("summarizeOverlaps is working on paired-end reads.")
       }
		  
		  Strand     <- StrandEntry$active
		  
		  res = countingReads(gtf,Bam.FolderNew,union,IntersectionStrict,IntersectionNonEmpty,Strand,Project,Paired)
      
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
		      label <- gtkLabelNew(paste("The ", exportName,".csv file has been saved in RNASeqGUI_Projects\\",Project,"\\Results\\", sep=""))
		      hbox$packStart(label,FALSE,FALSE,0)
		    }
		    
		    # If results are NOT saved, print information about it
		    if (exportName=="") {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("A file counts.txt has been produced and saved in RNASeqGUI_Projects\\",Project,"\\Results\\", sep=""))
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
      
}else{ #Linux
    
  Bam.Folder = strsplit(Bam.Folder,"/")
  Bam.FolderNew = Bam.Folder[[1]][2]
  Bam.FolderNew = paste("/",Bam.FolderNew,sep="")
  Bam.FolderNew

  for( i in 4:length(Bam.Folder[[1]])-1)
  {
    Bam.FolderNew = paste(Bam.FolderNew,Bam.Folder[[1]][i],sep="/")
  }
  print("Bam Folder selected: ")
  print(	Bam.FolderNew)
  print("PLEASE WAIT! ")
  Single <- !Paired
       if (Single == TRUE){print("summarizeOverlaps is working on single-end reads.")
                          }else{print("summarizeOverlaps is working on paired-end reads.")
       }
		  

  Strand     <- StrandEntry$active

   res = countingReads(gtf,Bam.FolderNew,union,IntersectionStrict,IntersectionNonEmpty,Strand,Project,Paired)
  
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
			dialog <- gtkDialogNewWithButtons("Done !",window, "modal","gtk-ok", GtkResponseType["ok"],"gtk-quit", GtkResponseType["cancel"])
		
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
				label <- gtkLabelNew(paste("A file counts.txt has been produced and saved in RNASeqGUI_Projects/",Project,"/Results/", sep=""))
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

counting_fun2 <- function(button, user.data) {
		res <- NULL
  d <- NULL
		error <- NULL
		warning <- NULL
  gtf <- NULL
	 gtf <- filename$getText()
  print("GTF selected: ")
  print(	gtf)
  Bam.Folder <- BamFolder$getText()
		Paired <- PairedEnd$active #logical
  unstr <-  radio1$active
  pstr <-  radio2$active
  rstr <-  radio3$active
  threads2 <-  radio11$active
  threads4 <-  radio22$active
  threads6 <-  radio33$active
  threads8 <-  radio44$active
  threads16 <- radio55$active

if(Sys.info()[[1]]=="Windows"){
  
  # I need this to get "C:\\Users\\utente\\Downloads\\example_RNASeqGUI\\demo\\" from 
  # "C:\\Users\\utente\\Downloads\\example_RNASeqGUI\\demo\\2L_1.bam"
  
  print("Bam Folder selected: ")
  print(  Bam.Folder)
  print("PLEASE WAIT! ")

  Bam.Folder = strsplit(Bam.Folder,"\\\\")
  Bam.FolderNew = Bam.Folder[[1]][2]
  Bam.FolderNew = paste("C:\\",Bam.FolderNew,sep="")
  
  for( i in 4:length(Bam.Folder[[1]])-1)
  {
    Bam.FolderNew = paste(Bam.FolderNew,Bam.Folder[[1]][i],sep="\\")
  }
  print("Bam Folder selected: ")
  print(  Bam.FolderNew)
		  
		  res = countingFeatures(gtf,Bam.FolderNew,Paired,unstr,pstr,rstr,Project,threads2,threads4,threads6,threads8,threads16)
      
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
		      label <- gtkLabelNew(paste("The ", exportName,".csv file has been saved in RNASeqGUI_Projects\\",Project,"\\Results\\", sep=""))
		      hbox$packStart(label,FALSE,FALSE,0)
		    }
		    
		    # If results are NOT saved, print information about it
		    if (exportName=="") {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("A file counts.txt has been produced and saved in RNASeqGUI_Projects\\",Project,"\\Results\\", sep=""))
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
    
  Bam.Folder = strsplit(Bam.Folder,"/")
  Bam.FolderNew = Bam.Folder[[1]][2]
  Bam.FolderNew = paste("/",Bam.FolderNew,sep="")
  Bam.FolderNew

  for( i in 4:length(Bam.Folder[[1]])-1)
  {
    Bam.FolderNew = paste(Bam.FolderNew,Bam.Folder[[1]][i],sep="/")
  }
  print("Bam Folder selected for countingFeatures: ")
  print(	Bam.FolderNew)
  print("PLEASE WAIT! ")

		 res = countingFeatures(gtf,Bam.FolderNew,Paired,unstr,pstr,rstr,Project,threads2,threads4,threads6,threads8,threads16)
  
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
			dialog <- gtkDialogNewWithButtons("Done !",window, "modal","gtk-ok", GtkResponseType["ok"],"gtk-quit", GtkResponseType["cancel"])
		
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
				label <- gtkLabelNew(paste("A file counts.txt has been produced and saved in RNASeqGUI_Projects/",Project,"/Results/", sep=""))
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
   label1 <- gtkLabelNewWithMnemonic("Select a bam folder by clicking on the corresponding 'Open' button.")
   label2 <- gtkLabelNewWithMnemonic("To select the entire bam folder, select just one bam file inside the bam folder you want to use.")
   label3 <- gtkLabelNewWithMnemonic("The entire folder will be loaded!")
   label4 <- gtkLabelNewWithMnemonic("Provide a GTF file by clicking on the corresponding 'Open' button.")
   label5 <- gtkLabelNewWithMnemonic("--------------------------------------------------------------------------------------------------------------------------------------")
   label6 <- gtkLabelNewWithMnemonic("For 'SummarizeOverlaps'")
   label7 <- gtkLabelNewWithMnemonic("In 'Count Mode' radio button field, choose one of the 3 modes: 'Union', 'IntersectionStrict' and 'IntersectionNotEmpty'") 
   label8 <- gtkLabelNewWithMnemonic("In 'Ignore Strand?' check-box, you can decide whether the counting procedure must ignore the strand or not.")
   label9 <- gtkLabelNewWithMnemonic("In 'Paired End' check-box, you can indicate whether paired-end reads are used.")
   label10 <- gtkLabelNewWithMnemonic("Finally, click on 'SummarizeOverlaps' button.")
   label11 <- gtkLabelNewWithMnemonic("For further information, see www.bioconductor.org/packages/release/bioc/html/GenomicRanges.html")
   label12 <- gtkLabelNewWithMnemonic("--------------------------------------------------------------------------------------------------------------------------------------")
   label13 <- gtkLabelNewWithMnemonic("For 'FeatureCounts'")
   label14 <- gtkLabelNewWithMnemonic("In the radio button field, you must specify in which mode FeatureCounts must work.")
   label15 <- gtkLabelNewWithMnemonic("There are three possible modes: 'unstranded', 'positive strand' and 'reverse strand'.") 
   label16 <- gtkLabelNewWithMnemonic("In 'Number of threads' radio button field, you must check the correct number of cores possessed by your processor.")
   label17 <- gtkLabelNewWithMnemonic("Otherwise, you will get an error.")
   label18 <- gtkLabelNewWithMnemonic("If your processor possesses only one core, you cannot use FeatureCounts.")
   label19 <- gtkLabelNewWithMnemonic("In 'Paired End' check-box, you can indicate whether paired-end reads are used.")
   label20 <- gtkLabelNewWithMnemonic("Finally, click on 'FeatureCounts' button.")
   label21 <- gtkLabelNewWithMnemonic("--------------------------------------------------------------------------------------------------------------------------------------")
   label22 <- gtkLabelNewWithMnemonic("Please, BE SURE there are at least TWO bam files inside the bam folder.")
   label23 <- gtkLabelNewWithMnemonic("Otherwise, you will get an error.")
   label24 <- gtkLabelNewWithMnemonic("For further information, see http://www.bioconductor.org/packages/release/bioc/html/Rsubread.html")


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
         vbox$packStart(label19,FALSE,FALSE,0)
         vbox$packStart(label20,FALSE,FALSE,0)
         vbox$packStart(label21,FALSE,FALSE,0)
         vbox$packStart(label22,FALSE,FALSE,0)
         vbox$packStart(label23,FALSE,FALSE,0)
         vbox$packStart(label24,FALSE,FALSE,0)
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
 title=paste("   \n    Read Count Interface is ready to work on ",Project, " project.", sep="")
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
	label <- gtkLabelNewWithMnemonic("_Select a bam folder")
	hbox$packStart(label,FALSE,FALSE,0)
	BamFolder <- gtkEntryNew()
	BamFolder$setWidthChars(60)
	label$setMnemonicWidget(BamFolder)
	hbox$packStart(BamFolder,FALSE,FALSE,0)
	buttonOpen <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen, "clicked", openBam)
	hbox$packStart(buttonOpen,FALSE,FALSE,0)

# BamFolder$setText("")
#	label$setMnemonicWidget(BamFolder)
#	hbox$packStart(BamFolder,FALSE,FALSE,0)

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add label in first column
	label <- gtkLabelNewWithMnemonic("_Provide a GTF")
	hbox$packStart(label,FALSE,FALSE,0)
	# Add entry in the second column; named "filename"
	filename <- gtkEntryNew()
	filename$setWidthChars(60)
	label$setMnemonicWidget(filename)
	hbox$packStart(filename,FALSE,FALSE,0)
	buttonOpen <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen, "clicked", openFile)
	hbox$packStart(buttonOpen,FALSE,FALSE,0)

	# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	label <- gtkLabelNewWithMnemonic("\n 'SummarizeOverlaps' setup")
	vbox$packStart(label,FALSE,FALSE,0)
	vbox$packStart(hbox, FALSE, FALSE, 0)

# Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	label <- gtkLabelNewWithMnemonic("Count Mode")
	vbox$packStart(label,FALSE,FALSE,0)
	vbox$packStart(hbox, FALSE, FALSE, 0)

  ## Create a radio button with a GtkEntry widget 
  radio111 <- gtkRadioButtonNewWithLabel(group = NULL,"Union", show = TRUE)
  #The two non-default options should only be used when rank.test = FALSE.
  ## Create a radio button with a label 
  radio222 <- gtkRadioButtonNewWithLabelFromWidget(radio111,"IntersectionStrict")
  ## Create a radio button with a label
  radio333 <- gtkRadioButtonNewWithLabelFromWidget(radio222,"IntersectionNotEmpty")
  
  ## Pack them into a box, then show all the widgets
  hbox$packStart(radio111, TRUE, TRUE, 2)
  hbox$packStart(radio222, TRUE, TRUE, 2)
  hbox$packStart(radio333, TRUE, TRUE, 2)
	 label$setMnemonicWidget(radio111)  
	 label$setMnemonicWidget(radio222) 
	 label$setMnemonicWidget(radio333) 

	# strand specific?
	label <- gtkLabelNewWithMnemonic("_Ignore Strand?")
	hbox$packStart(label,FALSE,FALSE,0)
	StrandEntry <- gtkCheckButton()
	StrandEntry$active <- TRUE
	hbox$packStart(StrandEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(StrandEntry)

	# paired?
	label <- gtkLabelNewWithMnemonic("_Paired End?")
	hbox$packStart(label,FALSE,FALSE,0)
	PairedEndOv <- gtkCheckButton()
	PairedEndOv$active <- TRUE
	hbox$packStart(PairedEndOv,FALSE,FALSE,0)
	label$setMnemonicWidget(PairedEndOv)

	# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	label <- gtkLabelNewWithMnemonic("\n 'FeatureCounts' setup")
	vbox$packStart(label,FALSE,FALSE,0)
	vbox$packStart(hbox, FALSE, FALSE, 0)

# Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	label <- gtkLabelNewWithMnemonic("Count Mode")
	vbox$packStart(label,FALSE,FALSE,0)
	vbox$packStart(hbox, FALSE, FALSE, 0)

  # Strand Number
  ## Create a radio button with a GtkEntry widget 
  radio1 <- gtkRadioButtonNewWithLabel(group = NULL,"unstranded", show = TRUE)
  #The two non-default options should only be used when rank.test = FALSE.
  ## Create a radio button with a label 
  radio2 <- gtkRadioButtonNewWithLabelFromWidget(radio1,"positive strand")
  ## Create a radio button with a label
  radio3 <- gtkRadioButtonNewWithLabelFromWidget(radio2,"reverse strand")

  ## Pack them into a box, then show all the widgets
  hbox$packStart(radio1, TRUE, TRUE, 2)
  hbox$packStart(radio2, TRUE, TRUE, 2)
  hbox$packStart(radio3, TRUE, TRUE, 2)
	 label$setMnemonicWidget(radio1)  
	 label$setMnemonicWidget(radio2) 
	 label$setMnemonicWidget(radio3)  

# Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	label <- gtkLabelNewWithMnemonic("\n Number of threads ")
	vbox$packStart(label,FALSE,FALSE,0)
	vbox$packStart(hbox, FALSE, FALSE, 0)

  # Strand Number
  ## Create a radio button with a GtkEntry widget 
  radio11 <- gtkRadioButtonNewWithLabel(group = NULL,"2 threads", show = TRUE)
  #The two non-default options should only be used when rank.test = FALSE.
  ## Create a radio button with a label 
  radio22 <- gtkRadioButtonNewWithLabelFromWidget(radio11,"4 threads")
  radio33 <- gtkRadioButtonNewWithLabelFromWidget(radio22,"6 threads")
  radio44 <- gtkRadioButtonNewWithLabelFromWidget(radio33,"8 threads")
  radio55 <- gtkRadioButtonNewWithLabelFromWidget(radio44,"16 threads")

  ## Pack them into a box, then show all the widgets
  hbox$packStart(radio11, TRUE, TRUE, 2)
  hbox$packStart(radio22, TRUE, TRUE, 2)
  hbox$packStart(radio33, TRUE, TRUE, 2)
  hbox$packStart(radio44, TRUE, TRUE, 2)
  hbox$packStart(radio55, TRUE, TRUE, 2)
	 label$setMnemonicWidget(radio11)  
	 label$setMnemonicWidget(radio22) 
	 label$setMnemonicWidget(radio33)  
	 label$setMnemonicWidget(radio44) 
	 label$setMnemonicWidget(radio55)  

	# paired?
	label <- gtkLabelNewWithMnemonic("_Paired End?")
	hbox$packStart(label,FALSE,FALSE,0)
	PairedEnd <- gtkCheckButton()
	PairedEnd$active <- TRUE
	hbox$packStart(PairedEnd,FALSE,FALSE,0)
	label$setMnemonicWidget(PairedEnd)

	# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

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

	label <- gtkLabelNewWithMnemonic("_Start Counting Reads")
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
	the.buttons$packStart(noi,fill=F)

 # button
	ReadCount <- gtkButtonNewWithMnemonic("_SummarizeOverlaps", show = TRUE)
	gSignalConnect(ReadCount, "clicked", counting_fun)
	the.buttons$packStart(ReadCount,fill=F)

 # button
	ReadCount2 <- gtkButtonNewWithMnemonic("_FeatureCounts", show = TRUE)
	gSignalConnect(ReadCount2, "clicked", counting_fun2)
	the.buttons$packStart(ReadCount2,fill=F)	

}

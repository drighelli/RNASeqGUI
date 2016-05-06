calculateGUI8 <-
function(project_name) {

 Project <- project_name
 message=paste("You are using the project: ",Project,sep="")
 print(message)
 
 SaveResultsInTSV <- function(res, Project, exportName) {
   if(Sys.info()[[1]]=="Windows"){
     sys.sep="\\"
   }else{
     sys.sep="/"
   }
   # print(colnames(res))
   # columns <- as.character(c("rnames", colnames(res)))
   # print(columns)
   write.table(res, 
               file=file.path("RNASeqGUI_Projects",Project,"Results", paste0(exportName,".tsv")), 
               sep="\t",
               quote=FALSE, 
               row.names=TRUE,
               col.names = NA
               )
 }
 
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

	openFolder <- function(button,user.data) {
		dialog <- gtkFileChooserDialog("Open File",window,c("open","modal","destroy-with-parent"),"gtk-cancel", GtkResponseType["cancel"],"gtk-open",GtkResponseType["accept"])
		dialog$setCurrentFolder("~/")
		if (dialog$run() == GtkResponseType["accept"]) {
			the.sel.Folder <- dialog$getFilename()
			Folder$setText(the.sel.Folder)
			dialog$destroy()
		} else {
			dialog$destroy()
		}
	}

cbinding_fun <- function(button, user.data) {
		res <- NULL
  d <- NULL
		error <- NULL
		warning <- NULL
  Count.Folder <- Folder$getText()

if(Sys.info()[[1]]=="Windows"){
  
  # I need this to get "C:\\Users\\utente\\Downloads\\example_RNASeqGUI\\demo\\" from 
  # "C:\\Users\\utente\\Downloads\\example_RNASeqGUI\\demo\\2L_1.bam"
  
  print("Folder selected: ")
  print(  Count.Folder)

  Count.Folder = strsplit(Count.Folder,"\\\\")
  Count.FolderNew = Count.Folder[[1]][2]
  Count.FolderNew = paste("C:\\",Count.FolderNew,sep="")
  Count.FolderNew
  
  for( i in 4:length(Count.Folder[[1]])-1)
  {
    Count.FolderNew = paste(Count.FolderNew,Count.Folder[[1]][i],sep="\\")
  }
  print("Folder selected: ")
  print(  Count.FolderNew)
		  
	  res = cbinding(Count.FolderNew,Project)
      
		  # Get saving options and save if needed
		  isToSave <- toSave$active
		  exportName <- exportfilename$getText()
		  if ((isToSave)&&(exportName=="")) {
		    warning <- "Invalid user data: a file name must be specified if you want to save results."
		  } else {
		    #write.table(res,file=paste("RNASeqGUI_Projects\\",Project,"\\Results\\",exportName,".tsv",sep="\t"), quote = FALSE, row.names = TRUE )
		    SaveResultsInTSV(res, Project, exportName)
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
		    if ((isToSave)&&(exportName!="")) {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The ", exportName,".tsv file has been saved in RNASeqGUI_Projects\\",Project,"\\Results\\", sep="\t"))
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
    
  Count.Folder = strsplit(Count.Folder,"/")
  Count.FolderNew = Count.Folder[[1]][2]
  Count.FolderNew = paste("/",Count.FolderNew,sep="")
  Count.FolderNew

  for( i in 4:length(Count.Folder[[1]])-1)
  {
    Count.FolderNew = paste(Count.FolderNew,Count.Folder[[1]][i],sep="/")
  }
  print("Folder selected: ")
  print(	Count.FolderNew)

   res = cbinding(Count.FolderNew,Project)
  
  	# Get saving options and save if needed
			isToSave <- toSave$active
			exportName <- exportfilename$getText()
			if ((isToSave)&&(exportName=="")) {
				warning <- "Invalid user data: a file name must be specified if you want to save results."
			} else {
				#write.table(res,file=paste("RNASeqGUI_Projects/",Project,"/Results/",exportName,".tsv",sep="\t"))
			  SaveResultsInTSV(res, Project, exportName)
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
			if ((isToSave)&&(exportName!="")) {
				hbox <- gtkHBoxNew(FALSE,20)
				vbox$packStart(hbox, FALSE, FALSE, 0)
				label <- gtkLabelNew(paste("The ", exportName,".tsv file has been saved in RNASeqGUI_Projects/",Project,"/Results/", sep=""))
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

conversion_fun <- function(button, user.data) {
		res <- NULL
  d <- NULL
		error <- NULL
		warning <- NULL
  Count.FileNew <- Folder$getText()
  hsa <-  radio1$active
  mmu <-  radio2$active
  dme <-  radio3$active
  dre <-  radio4$active
  Convers <- Convers$active
  Convers2 <- Convers2$active
  Convers3 <- Convers3$active

if(Sys.info()[[1]]=="Windows"){
  
  print("File selected: ")
  print(Count.FileNew)
		  
	  res = conversion_function(Count.FileNew,Project,hsa,mmu,dme,dre,Convers,Convers2,Convers3)
      
		  # Get saving options and save if needed
		  isToSave <- toSave$active
		  exportName <- exportfilename$getText()
		  if ((isToSave)&&(exportName=="")) {
		    warning <- "Invalid user data: a file name must be specified if you want to save results."
		  } else {
		    SaveResultsInTSV(res, Project, exportName)
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
		    if ((isToSave)&&(exportName!="")) {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The ", exportName,".tsv file has been saved in RNASeqGUI_Projects\\",Project,"\\Results\\", sep=""))
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
    
  print("File selected: ")
  print(	Count.FileNew)

	  res = conversion_function(Count.FileNew,Project,hsa,mmu,dme,dre,Convers,Convers2,Convers3)
  
  	# Get saving options and save if needed
			isToSave <- toSave$active
			exportName <- exportfilename$getText()
			if ((isToSave)&&(exportName=="")) {
				warning <- "Invalid user data: a file name must be specified if you want to save results."
			} else {
				#write.table(res,file=paste("RNASeqGUI_Projects/",Project,"/Results/",exportName,".tsv",sep="\t"), quote=FALSE, row.names=FALSE)
			  SaveResultsInTSV(res, Project, exportName)
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
			if ((isToSave)&&(exportName!="")) {
				hbox <- gtkHBoxNew(FALSE,20)
				vbox$packStart(hbox, FALSE, FALSE, 0)
				label <- gtkLabelNew(paste("The ", exportName,".tsv file has been saved in RNASeqGUI_Projects/",Project,"/Results/", sep=""))
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


modifycountfile_fun <- function(button, user.data) {
		res <- NULL
  d <- NULL
		error <- NULL
		warning <- NULL
  Count.File <- Folder$getText()
  columns <- colEntry$getText()
  columns <- unlist(strsplit(columns, ","))
  print("Count file selected: ")
  print(Count.File)

  if(Sys.info()[[1]]=="Windows"){
  	  
	  res = modifycountfile(Count.File,Project,columns)
      
		  # Get saving options and save if needed
		  isToSave <- toSave$active
		  exportName <- exportfilename$getText()
		  if ((isToSave)&&(exportName=="")) {
		    warning <- "Invalid user data: a file name must be specified if you want to save results."
		  } else {
		    #write.table(res,file=paste("RNASeqGUI_Projects\\",Project,"\\Results\\",exportName,".tsv",sep="\t"))
		    SaveResultsInTSV(res, Project, exportName)
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
		    if ((isToSave)&&(exportName!="")) {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The ", exportName,".tsv file has been saved in RNASeqGUI_Projects\\",Project,"\\Results\\", sep=""))
		      hbox$packStart(label,FALSE,FALSE,0)
		    }
		    
		    # If results are NOT saved, print information about it
		    if (exportName=="") {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("A file has been produced and saved in RNASeqGUI_Projects\\",Project,"\\Results\\", sep=""))
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

   res = modifycountfile(Count.File,Project,columns)
  
  	# Get saving options and save if needed
			isToSave <- toSave$active
			exportName <- exportfilename$getText()
			if ((isToSave)&&(exportName=="")) {
				warning <- "Invalid user data: a file name must be specified if you want to save results."
			} else {
				#write.table(res,file=paste("RNASeqGUI_Projects/",Project,"/Results/",exportName,".tsv",sep="\t"))
			  SaveResultsInTSV(res, Project, exportName)
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
			if ((isToSave)&&(exportName!="")) {
				hbox <- gtkHBoxNew(FALSE,20)
				vbox$packStart(hbox, FALSE, FALSE, 0)
				label <- gtkLabelNew(paste("The .tsv file has been saved in RNASeqGUI_Projects/",Project,"/Results/", sep=""))
				hbox$packStart(label,FALSE,FALSE,0)
			}

			# If results are NOT saved, print information about it
			if (exportName=="") {
				hbox <- gtkHBoxNew(FALSE,20)
				vbox$packStart(hbox, FALSE, FALSE, 0)
				label <- gtkLabelNew(paste("A file has been produced and saved in RNASeqGUI_Projects/",Project,"/Results/", sep=""))
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


roundcountfile_fun <- function(button, user.data) {
		res <- NULL
  d <- NULL
		error <- NULL
		warning <- NULL
  Count.File <- Folder$getText()
  print("Count file selected: ")
  print(Count.File)

  if(Sys.info()[[1]]=="Windows"){
  	  
	  res = roundcountfile(Count.File,Project)
      
		  # Get saving options and save if needed
		  isToSave <- toSave$active
		  exportName <- exportfilename$getText()
		  if ((isToSave)&&(exportName=="")) {
		    warning <- "Invalid user data: a file name must be specified if you want to save results."
		  } else {
		    #write.table(res,file=paste("RNASeqGUI_Projects\\",Project,"\\Results\\",exportName,".tsv",sep="\t"))
		    SaveResultsInTSV(res, Project, exportName)
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
		    if ((isToSave)&&(exportName!="")) {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("The ", exportName,".tsv file has been saved in RNASeqGUI_Projects\\",Project,"\\Results\\", sep=""))
		      hbox$packStart(label,FALSE,FALSE,0)
		    }
		    
		    # If results are NOT saved, print information about it
		    if (exportName=="") {
		      hbox <- gtkHBoxNew(FALSE,20)
		      vbox$packStart(hbox, FALSE, FALSE, 0)
		      label <- gtkLabelNew(paste("A file has been produced and saved in RNASeqGUI_Projects\\",Project,"\\Results\\", sep=""))
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

   res = roundcountfile(Count.File,Project)
  
  	# Get saving options and save if needed
			isToSave <- toSave$active
			exportName <- exportfilename$getText()
			if ((isToSave)&&(exportName=="")) {
				warning <- "Invalid user data: a file name must be specified if you want to save results."
			} else {
				#write.table(res,file=paste("RNASeqGUI_Projects/",Project,"/Results/",exportName,".tsv",sep="\t"), quote = FALSE, row.names = TRUE )
			  SaveResultsInTSV(res, Project, exportName)
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
			if ((isToSave)&&(exportName!="")) {
				hbox <- gtkHBoxNew(FALSE,20)
				vbox$packStart(hbox, FALSE, FALSE, 0)
				label <- gtkLabelNew(paste("The ", exportName,".tsv file has been saved in RNASeqGUI_Projects/",Project,"/Results/", sep=""))
				hbox$packStart(label,FALSE,FALSE,0)
			}

			# If results are NOT saved, print information about it
			if (exportName=="") {
				hbox <- gtkHBoxNew(FALSE,20)
				vbox$packStart(hbox, FALSE, FALSE, 0)
				label <- gtkLabelNew(paste("A file has been produced and saved in RNASeqGUI_Projects/",Project,"/Results/", sep=""))
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
   label1 <- gtkLabelNewWithMnemonic("If you want to bind count files together then select a count folder by clicking on the corresponding 'Open' button.")
   label2 <- gtkLabelNewWithMnemonic("To select the entire folder, select just one file inside the folder you want to use.")
   label3 <- gtkLabelNewWithMnemonic("The entire folder will be loaded!")
   label4 <- gtkLabelNewWithMnemonic("Please, be sure that the folder ONLY contains the files you want to bind and that there are at least TWO files to bind.")
   label5 <- gtkLabelNewWithMnemonic("Finally, click on 'Bind Count Files' button.")
   label6 <- gtkLabelNewWithMnemonic("If you want to convert ensembl gene ids or GENCODEs to gene symbols then select a count file by clicking on the corresponding 'Open' button.")
   label7 <- gtkLabelNewWithMnemonic("Finally, click on 'Convert' button.")
   label8 <- gtkLabelNewWithMnemonic("The third button is the Modify Count one that creates a new count file containing the columns of interest only.") 
   label9 <- gtkLabelNewWithMnemonic("Select a count file by clicking on the corresponding Open button. Select the number of columns to keep.")
   label10 <- gtkLabelNewWithMnemonic("Finally, click on Modify Count button.")
   label11 <- gtkLabelNewWithMnemonic("For further information, see http://bioinfo.na.iac.cnr.it/RNASeqGUI/Manual.html .")

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
 title=paste("   \n    Utility Interface is ready to work on ",Project, " project.", sep="")
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
	label <- gtkLabelNewWithMnemonic("_Select a count folder or a count file")
	hbox$packStart(label,FALSE,FALSE,0)
	Folder <- gtkEntryNew()
	Folder$setWidthChars(60)
	label$setMnemonicWidget(Folder)
	hbox$packStart(Folder,FALSE,FALSE,0)
	buttonOpen <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen, "clicked", openFolder)
	hbox$packStart(buttonOpen,FALSE,FALSE,0)

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
	label <- gtkLabelNewWithMnemonic("_Conversion from ENSEMBL ids to gene names")
	hbox$packStart(label,FALSE,FALSE,0)
	Convers <- gtkCheckButton()
	Convers$active <- TRUE
	hbox$packStart(Convers,FALSE,FALSE,0)
	label$setMnemonicWidget(Convers) 

 # Conversion2
	label <- gtkLabelNewWithMnemonic("_Conversion from GENCODEs to gene names")
	hbox$packStart(label,FALSE,FALSE,0)
	Convers2 <- gtkCheckButton()
	Convers2$active <- FALSE
	hbox$packStart(Convers2,FALSE,FALSE,0)
	label$setMnemonicWidget(Convers2)

 # Conversion3
	label <- gtkLabelNewWithMnemonic("_Conversion from gene names to ENSEMBL ids")
	hbox$packStart(label,FALSE,FALSE,0)
	Convers3 <- gtkCheckButton()
	Convers3$active <- FALSE
	hbox$packStart(Convers3,FALSE,FALSE,0)
	label$setMnemonicWidget(Convers3)

 # Add an horizontal container 
	hbox <- gtkHBoxNew(FALSE,8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_Modify the count file")
	vbox$packStart(label,FALSE,FALSE,0)

	# col
	label <- gtkLabelNewWithMnemonic("_Select the number of the columns to keep")
	hbox$packStart(label,FALSE,FALSE,0)
	colEntry <- gtkEntryNew()
 colEntry$setWidthChars(5)
	colEntry$setText("1,2")
	hbox$packStart(colEntry,FALSE,FALSE,0)
	label$setMnemonicWidget(colEntry)  

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
	label <- gtkLabel(".tsv")
	hbox$packStart(label,FALSE,FALSE,0)

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


	# Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_Choose an utility function below")
	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)


 # button
	BindCount <- gtkButtonNewWithMnemonic("_Bind Count Files", show = TRUE)
	gSignalConnect(BindCount, "clicked", cbinding_fun)
	the.buttons$packStart(BindCount,fill=F)

 # button
	Convert <- gtkButtonNewWithMnemonic("_Convert", show = TRUE)
	gSignalConnect(Convert, "clicked", conversion_fun)
	the.buttons$packStart(Convert,fill=F)

 # button
	modify <- gtkButtonNewWithMnemonic("_Keep Columns", show = TRUE)
	gSignalConnect(modify, "clicked", modifycountfile_fun)
	the.buttons$packStart(modify,fill=F)

 # button
	round <- gtkButtonNewWithMnemonic("_Round", show = TRUE)
	gSignalConnect(round, "clicked", roundcountfile_fun)
	the.buttons$packStart(round,fill=F)



}

# required: RGtk2
RNASeqGUI <- function() {

      if (length(find.package(package='RGtk2',quiet=T))>0) {
        require(RGtk2)
      } else {
        stop("Package RGtk2 not installed")
      }

require(GenomicRanges)
require(GenomicFeatures)
require(Rsamtools)
require(DEXSeq)
require(pasilla)
require(gplots)
require(RColorBrewer)
require(EDASeq)
require(leeBamViews)
require(preprocessCore)
require(scatterplot3d)
require(BiocParallel)
require(digest)
require(Rsubread)
require(ReportingTools)

require(e1071)
require(ineq) 
require(knitr)
require(filehash)
require(plotrix)
require(lattice)

infoGUI <- function(){

 # Create window
	window <- gtkWindow()
	# Add title
	window["title"] <- "RNASeqGUI"

	# Add a frame
	frame <- gtkFrameNew(" INFO    ")
	window$add(frame)

	# Create vertical container for file name entry
	vbox <- gtkVBoxNew(FALSE, 8)
	vbox$setBorderWidth(24)
	frame$add(vbox)

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
	contactButton <- gtkButtonNewWithMnemonic("Contacts and INFO", show = TRUE)
	gSignalConnect(contactButton, "clicked", contact)
	the.buttons$packStart(contactButton,fill=F)

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add buttons
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

 # button
	licenceButton <- gtkButtonNewWithMnemonic("Licences, Terms and Conditions", show = TRUE)
	gSignalConnect(licenceButton, "clicked", licence)
	the.buttons$packStart(licenceButton,fill=F)

}

selectprojectFun <- function(button,user.data) {

   selectedproject_name <- NULL
			selectedproject_name=filename2$getText()
   pathToProject=paste("RNASeqGUI_Projects/",selectedproject_name,sep="")
   exist=file.exists(pathToProject)
   if(exist==FALSE){
       print("The project DOES NOT exists!")
       # Open a dialog box to print results
       message=paste(selectedproject_name," project DOES NOT exists!",sep="")
			    dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
      	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		    	# Create vertical container for file name entry
	    		vbox <- gtkVBoxNew(FALSE, 8)
		    	vbox$setBorderWidth(24)
	    		dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
       message2=paste(message," Please, select an existing project.",sep="")
      	label1 <- gtkLabelNewWithMnemonic(message2) 

      	vbox$packStart(label1,FALSE,FALSE,0)
		     response <- dialog$run()
		    	# Return to previous window
			    if (response == GtkResponseType["ok"]) { dialog$destroy() }		
   }else{
     if (selectedproject_name!="") {
			  ########## Start dialog...
			  # Open a dialog box to print results
     message=paste(selectedproject_name," project selected!",sep="")
			  dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
    	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)

		  	# Create vertical container for file name entry
			  vbox <- gtkVBoxNew(FALSE, 8)
			  vbox$setBorderWidth(24)
			  dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
     message=paste("You have selected the ", selectedproject_name," project.",sep="")
    	label1 <- gtkLabelNewWithMnemonic(message) 

     #load the existing project report
     report=paste("RNASeqGUI_Projects/",selectedproject_name,"/Logs/report.Rmd",sep="") 

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message <- paste("--------------------------------------------------------------------------",sep="\n")
     write(message, file = report,ncolumns = if(is.character(message)) 1 else 5,append = TRUE, sep = "\n")

     message2 <- paste("### Project RE-started the ",Sys.time(),sep="") 
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")

     message4 <- paste("  ",sep="\n")
     write(message4, file = report,ncolumns = if(is.character(message4)) 1 else 5,append = TRUE, sep = "\n")

     #load sessionInfo file
     sessionInfo=paste("RNASeqGUI_Projects/",selectedproject_name,"/Logs/sessionInfo.txt",sep="")  

     message5 <- paste("  ",sep="\n")
     write(message5, file = sessionInfo,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message3 <- capture.output(sessionInfo())
     write(message3, file = sessionInfo ,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "\n")

  	  vbox$packStart(label1,FALSE,FALSE,0)
			  response <- dialog$run()
		  	# Return to previous window
			  if (response == GtkResponseType["ok"]) {dialog$destroy()}
			} else {
			  ########## Start dialog...
			  # Open a dialog box to print results
     print("ERROR: You did NOT choose a name!")
			  dialog <- gtkDialogNewWithButtons(" ERROR!",window, "modal","gtk-ok", GtkResponseType["ok"])
    	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)

		  	# Create vertical container for file name entry
			  vbox <- gtkVBoxNew(FALSE, 8)
			  vbox$setBorderWidth(24)
			  dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
     label1 <- gtkLabelNewWithMnemonic("You did NOT choose a name!") 
		  	vbox$packStart(label1,FALSE,FALSE,0)
			  response <- dialog$run()
		  	# Return to previous window
			  if (response == GtkResponseType["ok"]) {dialog$destroy()}	
			 }
   }
		
	}


createRNASeqGUI_ProjectsDir <- function(button,user.data) {
 RNASeqGUI_ProjectsDir <- NULL
 RNASeqGUI_ProjectsDir <- paste(getwd(),"/RNASeqGUI_Projects",sep="")
 exist=file.exists(RNASeqGUI_ProjectsDir)
 if ( exist==FALSE ){
   dir.create(RNASeqGUI_ProjectsDir, showWarnings = TRUE, recursive = FALSE)
   print("RNASeqGUI_Projects folder created for the first time!")
  createprojectFun(button,user.data)
 }else{
  createprojectFun(button,user.data)
 }
}


createprojectFun <- function(button,user.data) {

   project_name <- NULL
			project_name=filename$getText()
   pathToProject=paste("RNASeqGUI_Projects/",project_name,sep="")
   exist=file.exists(pathToProject)
   if(exist==FALSE){ # The project does NOT exist. If project_name=="" exist is TRUE
     #create directories
     dir.create(pathToProject, showWarnings = TRUE, recursive = FALSE)
     pathToProject=paste("RNASeqGUI_Projects/",project_name,"/Results",sep="")
     dir.create(pathToProject, showWarnings = TRUE, recursive = FALSE)
     pathToProject=paste("RNASeqGUI_Projects/",project_name,"/Plots",sep="")
     dir.create(pathToProject, showWarnings = TRUE, recursive = FALSE)
     pathToProject=paste("RNASeqGUI_Projects/",project_name,"/Logs",sep="")
     dir.create(pathToProject, showWarnings = TRUE, recursive = FALSE) 
     pathToProject=paste("RNASeqGUI_Projects/",project_name,"/Logs/cache",sep="")
     dir.create(pathToProject, showWarnings = TRUE, recursive = FALSE) 

     #create log file
     report=paste("RNASeqGUI_Projects/",project_name,"/Logs/report.Rmd",sep="")  
     file.create(report)  
     message <- paste("# The ",project_name," project report",sep="") 
     write(message, file = report,ncolumns = if(is.character(message)) 1 else 5,append = TRUE, sep = "\n")

     message <- paste("### Project created the ",Sys.time(),sep="") 
     write(message, file = report,ncolumns = if(is.character(message)) 1 else 5,append = TRUE, sep = "\n")

     message5 <- paste("  ",sep="\n")
     write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     #create sessionInfo file
     sessionInfo=paste("RNASeqGUI_Projects/",project_name,"/Logs/sessionInfo.txt",sep="")  
     file.create(sessionInfo)  

     message3 <- capture.output(sessionInfo())
     write(message3, file = sessionInfo ,ncolumns = if(is.character(message3)) 1 else 5,append = TRUE, sep = "\n")
   
     if (!is.null(project_name)) {
			    ########## Start dialog...
			    # Open a dialog box to print results
       message=paste(project_name," project created!",sep="")
			    dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
      	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		    	# Create vertical container for file name entry
	    		vbox <- gtkVBoxNew(FALSE, 8)
		    	vbox$setBorderWidth(24)
	    		dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
       message=paste("You have created your '", project_name,"' project in the RNASeqGUI__Projects folder. \n\n The RNASeqGUI__Projects folder is in the directory where you started R.",sep="")
      	label1 <- gtkLabelNewWithMnemonic(message) 

      	vbox$packStart(label1,FALSE,FALSE,0)
		     response <- dialog$run()
		    	# Return to previous window
			    if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
     }
   }else{
     if (project_name =="") {
        print("ERROR: You did NOT choose a name!")
       # Open a dialog box to print results
			    dialog <- gtkDialogNewWithButtons(" ERROR!",window, "modal","gtk-ok", GtkResponseType["ok"])
      	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		    	# Create vertical container for file name entry
	    		vbox <- gtkVBoxNew(FALSE, 8)
		    	vbox$setBorderWidth(24)
	    		dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
       message="You did NOT choose a name!"
      	label1 <- gtkLabelNewWithMnemonic(message) 

      	vbox$packStart(label1,FALSE,FALSE,0)
		     response <- dialog$run()
		    	# Return to previous window
			    if (response == GtkResponseType["ok"]) { dialog$destroy() }	
       }else{
       print("The project already exists!")
       # Open a dialog box to print results
       message=paste(project_name," project already exists!",sep="")
			    dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
      	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		    	# Create vertical container for file name entry
	    		vbox <- gtkVBoxNew(FALSE, 8)
		    	vbox$setBorderWidth(24)
	    		dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
       message2=paste(message," Please, choose another name for a new project.",sep="")
      	label1 <- gtkLabelNewWithMnemonic(message2) 

      	vbox$packStart(label1,FALSE,FALSE,0)
		     response <- dialog$run()
		    	# Return to previous window
			    if (response == GtkResponseType["ok"]) { dialog$destroy() }	
      }		
	}	
}

#####   DATA EXPLORATION   ######
GUI1 <- function(button, user.data) {
	 res <- NULL
    #if a new project was created then
    if (filename$getText()!="") {
      new_project_name <- NULL
			   new_project_name=filename$getText()
      #print("You are using the new project: ")
      #print(new_project_name)
      res = calculateGUI1(new_project_name)                 		
    }else{
      # if no project was created and no project was selected then
      if(filename2$getText()==""){
         print("You did NOT created any project NOR selected an existing one!")
			      ########## Start dialog...
			      # Open a dialog box to print results
         message=paste("ERROR MESSAGE !",sep="")
			      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
        	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		      	# Create vertical container for file name entry
	    	  	vbox <- gtkVBoxNew(FALSE, 8)
		      	vbox$setBorderWidth(24)
	    	  	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
         message=paste("You did NOT created any project NOR selected an existing one!",sep="")
        	label1 <- gtkLabelNewWithMnemonic(message) 

        	vbox$packStart(label1,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
      }else{
        project_name <- NULL
		  	   project_name=filename2$getText()
        #print("You are using the existing project: ")
        #print(project_name)
        res = calculateGUI1(project_name) 
      }   
    }
  res
  rm(list = ls())
	} #End of function

#####   DATA ANALYSIS   ######
GUI2 <- function(button, user.data) {
res <- NULL
    #if a new project was created then
    if (filename$getText()!="") {
      new_project_name <- NULL
			   new_project_name=filename$getText()
      #print("You are using the new project: ")
      #print(new_project_name)
      res = calculateGUI2(new_project_name)                 		
    }else{
      # if no project was created and no project was selected then
      if(filename2$getText()==""){
         print("You did NOT created any project NOR selected an existing one!")
			      ########## Start dialog...
			      # Open a dialog box to print results
         message=paste("ERROR MESSAGE !",sep="")
			      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
        	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		      	# Create vertical container for file name entry
	    	  	vbox <- gtkVBoxNew(FALSE, 8)
		      	vbox$setBorderWidth(24)
	    	  	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
         message=paste("You did NOT created any project NOR selected an existing one!",sep="")
        	label1 <- gtkLabelNewWithMnemonic(message) 

        	vbox$packStart(label1,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
      }else{
        project_name <- NULL
		  	   project_name=filename2$getText()
        #print("You are using the existing project: ")
        #print(project_name)
        res = calculateGUI2(project_name) 
      }   
    }
  res
  rm(list = ls())
	} #End of function

#####   Exploration of the Results   ######
GUI3 <- function(button, user.data) {
res <- NULL
    #if a new project was created then
    if (filename$getText()!="") {
      new_project_name <- NULL
			   new_project_name=filename$getText()
      #print("You are using the new project: ")
      #print(new_project_name)
      res = calculateGUI3(new_project_name)                 		
    }else{
      # if no project was created and no project was selected then
      if(filename2$getText()==""){
         print("You did not created any project nor selected an existing one!")
			      ########## Start dialog...
			      # Open a dialog box to print results
         message=paste("ERROR MESSAGE !",sep="")
			      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
        	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		      	# Create vertical container for file name entry
	    	  	vbox <- gtkVBoxNew(FALSE, 8)
		      	vbox$setBorderWidth(24)
	    	  	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
         message=paste("You did not created any project nor selected an existing one!",sep="")
        	label1 <- gtkLabelNewWithMnemonic(message) 

        	vbox$packStart(label1,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
      }else{
        project_name <- NULL
		  	   project_name=filename2$getText()
        #print("You are using the existing project: ")
        #print(project_name)
        res = calculateGUI3(project_name) 
      }   
    }
  res
  rm(list = ls())
	} #End of function

####### COMPARE RESULTS ########
GUI4 <- function(button, user.data) {
res <- NULL
    #if a new project was created then
    if (filename$getText()!="") {
      new_project_name <- NULL
			   new_project_name=filename$getText()
      #print("You are using the new project: ")
      #print(new_project_name)
      res = calculateGUI4(new_project_name)                 		
    }else{
      # if no project was created and no project was selected then
      if(filename2$getText()==""){
         print("You did not created any project nor selected an existing one!")
			      ########## Start dialog...
			      # Open a dialog box to print results
         message=paste("ERROR MESSAGE !",sep="")
			      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
        	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		      	# Create vertical container for file name entry
	    	  	vbox <- gtkVBoxNew(FALSE, 8)
		      	vbox$setBorderWidth(24)
	    	  	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
         message=paste("You did not created any project nor selected an existing one!",sep="")
        	label1 <- gtkLabelNewWithMnemonic(message) 

        	vbox$packStart(label1,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
      }else{
        project_name <- NULL
		  	   project_name=filename2$getText()
        #print("You are using the existing project: ")
        #print(project_name)
        res = calculateGUI4(project_name) 
      }   
    }
  res
  rm(list = ls())
	} #End of function

#####   NORMALIZATION   ######
GUI5 <- function(button, user.data) {
res <- NULL
    #if a new project was created then
    if (filename$getText()!="") {
      new_project_name <- NULL
			   new_project_name=filename$getText()
      #print("You are using the new project: ")
      #print(new_project_name)
      res = calculateGUI5(new_project_name)                 		
    }else{
      # if no project was created and no project was selected then
      if(filename2$getText()==""){
         print("You did not created any project nor selected an existing one!")
			      ########## Start dialog...
			      # Open a dialog box to print results
         message=paste("ERROR MESSAGE !",sep="")
			      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
        	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		      	# Create vertical container for file name entry
	    	  	vbox <- gtkVBoxNew(FALSE, 8)
		      	vbox$setBorderWidth(24)
	    	  	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
         message=paste("You did not created any project nor selected an existing one!",sep="")
        	label1 <- gtkLabelNewWithMnemonic(message) 

        	vbox$packStart(label1,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
      }else{
        project_name <- NULL
		  	   project_name=filename2$getText()
        #print("You are using the existing project: ")
        #print(project_name)
        res = calculateGUI5(project_name) 
      }   
    }
  res
  rm(list = ls())
	} #End of function

#####   BAM FILES   ######
GUI6 <- function(button, user.data) {
res <- NULL
    #if a new project was created then
    if (filename$getText()!="") {
      new_project_name <- NULL
			   new_project_name=filename$getText()
      #print("You are using the new project: ")
      #print(new_project_name)
      res = calculateGUI6(new_project_name)                 		
    }else{
      # if no project was created and no project was selected then
      if(filename2$getText()==""){
         print("You did not created any project nor selected an existing one!")
			      ########## Start dialog...
			      # Open a dialog box to print results
         message=paste("ERROR MESSAGE !",sep="")
			      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
        	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		      	# Create vertical container for file name entry
	    	  	vbox <- gtkVBoxNew(FALSE, 8)
		      	vbox$setBorderWidth(24)
	    	  	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
         message=paste("You did not created any project nor selected an existing one!",sep="")
        	label1 <- gtkLabelNewWithMnemonic(message) 

        	vbox$packStart(label1,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
      }else{
        project_name <- NULL
		  	   project_name=filename2$getText()
        #print("You are using the existing project: ")
        #print(project_name)
        res = calculateGUI6(project_name) 
      }   
    }
  res
  rm(list = ls())
	} #End of function

#####   COUNTING   ######
GUI7 <- function(button, user.data) {
res <- NULL
    #if a new project was created then
    if (filename$getText()!="") {
      new_project_name <- NULL
			   new_project_name=filename$getText()
      #print("You are using the new project: ")
      #print(new_project_name)
      res = calculateGUI7(new_project_name)                 		
    }else{
      # if no project was created and no project was selected then
      if(filename2$getText()==""){
         print("You did not created any project nor selected an existing one!")
			      ########## Start dialog...
			      # Open a dialog box to print results
         message=paste("ERROR MESSAGE !",sep="")
			      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
        	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		      	# Create vertical container for file name entry
	    	  	vbox <- gtkVBoxNew(FALSE, 8)
		      	vbox$setBorderWidth(24)
	    	  	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
         message=paste("You did not created any project nor selected an existing one!",sep="")
        	label1 <- gtkLabelNewWithMnemonic(message) 

        	vbox$packStart(label1,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
      }else{
        project_name <- NULL
		  	   project_name=filename2$getText()
        #print("You are using the existing project: ")
        #print(project_name)
        res = calculateGUI7(project_name) 
      }   
    }
  res
  rm(list = ls())
	} #End of function


#####   DATA EDITING   ######
GUI8 <- function(button, user.data) {
	 res <- NULL
    #if a new project was created then
    if (filename$getText()!="") {
      new_project_name <- NULL
			   new_project_name=filename$getText()
      #print("You are using the new project: ")
      #print(new_project_name)
      res = calculateGUI8(new_project_name)                 		
    }else{
      # if no project was created and no project was selected then
      if(filename2$getText()==""){
         print("You did NOT created any project NOR selected an existing one!")
			      ########## Start dialog...
			      # Open a dialog box to print results
         message=paste("ERROR MESSAGE !",sep="")
			      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
        	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		      	# Create vertical container for file name entry
	    	  	vbox <- gtkVBoxNew(FALSE, 8)
		      	vbox$setBorderWidth(24)
	    	  	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
         message=paste("You did NOT created any project NOR selected an existing one!",sep="")
        	label1 <- gtkLabelNewWithMnemonic(message) 

        	vbox$packStart(label1,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
      }else{
        project_name <- NULL
		  	   project_name=filename2$getText()
        #print("You are using the existing project: ")
        #print(project_name)
        res = calculateGUI8(project_name) 
      }   
    }
  res
  rm(list = ls())
	} #End of function


#####   Gene-set/Pathway Analysis   ######
GUI9 <- function(button, user.data) {
res <- NULL
    #if a new project was created then
    if (filename$getText()!="") {
      new_project_name <- NULL
			   new_project_name=filename$getText()
      #print("You are using the new project: ")
      #print(new_project_name)
      res = calculateGUI9(new_project_name)                 		
    }else{
      # if no project was created and no project was selected then
      if(filename2$getText()==""){
         print("You did not created any project nor selected an existing one!")
			      ########## Start dialog...
			      # Open a dialog box to print results
         message=paste("ERROR MESSAGE !",sep="")
			      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
        	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		      	# Create vertical container for file name entry
	    	  	vbox <- gtkVBoxNew(FALSE, 8)
		      	vbox$setBorderWidth(24)
	    	  	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
         message=paste("You did not created any project nor selected an existing one!",sep="")
        	label1 <- gtkLabelNewWithMnemonic(message) 

        	vbox$packStart(label1,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
      }else{
        project_name <- NULL
		  	   project_name=filename2$getText()
        #print("You are using the existing project: ")
        #print(project_name)
        res = calculateGUI9(project_name) 
      }   
    }
  res
  rm(list = ls())
	} #End of function

#####   REPORT   ######
GUI10 <- function(button, user.data) {
#res <- NULL
    #if a new project was created then
    if (filename$getText()!="") {
      new_project_name <- NULL
			   new_project_name=filename$getText()
      print("You are using the new project: ")
      print(new_project_name)
      #res = calculateGUI10(new_project_name) 
      #print("You are using the project: ")
      #print(project_name)
      a=paste(getwd(),"/RNASeqGUI_Projects/",new_project_name,"/Logs/report.Rmd",sep="")
      main_location=getwd()
      setwd(paste(getwd(),"/RNASeqGUI_Projects/",new_project_name,"/Logs",sep=""))
      knit(a)
      b=paste(getwd(),"/report.md",sep="")
      knitr::knit2html(b)
      c=paste(getwd(),"/report.html",sep="")
      browseURL(c)               		
      setwd(main_location)
      rm(list = ls())                		
    }else{
      # if no project was created and no project was selected then
      if(filename2$getText()==""){
         print("You did NOT created any project NOR selected an existing one!")
			      ########## Start dialog...
			      # Open a dialog box to print results
         message=paste("ERROR MESSAGE !",sep="")
			      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
        	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		      	# Create vertical container for file name entry
	    	  	vbox <- gtkVBoxNew(FALSE, 8)
		      	vbox$setBorderWidth(24)
	    	  	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
         message=paste("You did NOT created any project NOR selected an existing one!",sep="")
        	label1 <- gtkLabelNewWithMnemonic(message) 

        	vbox$packStart(label1,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
      }else{ #if an existing project was selected then
        project_name <- NULL
		  	   project_name=filename2$getText()
        #print("You are using the existing project: ")
        #print(project_name)
        #res = calculateGUI10(project_name) 
      print("You are using the existing project: ")
      print(project_name)
      a=paste(getwd(),"/RNASeqGUI_Projects/",project_name,"/Logs/report.Rmd",sep="")
      main_location=getwd()
      setwd(paste(getwd(),"/RNASeqGUI_Projects/",project_name,"/Logs",sep=""))
      knit(a)
      b=paste(getwd(),"/report.md",sep="")
      knitr::knit2html(b)
      c=paste(getwd(),"/report.html",sep="")
      browseURL(c)               		
      setwd(main_location)
      rm(list = ls())
      }   
    }
  #res
  rm(list = ls())
	} #End of function


#####   Graphite Analysis   ######
GUI11 <- function(button, user.data) {
res <- NULL
    #if a new project was created then
    if (filename$getText()!="") {
      new_project_name <- NULL
			   new_project_name=filename$getText()
      #print("You are using the new project: ")
      #print(new_project_name)
      res = calculateGUI11(new_project_name)                 		
    }else{
      # if no project was created and no project was selected then
      if(filename2$getText()==""){
         print("You did not created any project nor selected an existing one!")
			      ########## Start dialog...
			      # Open a dialog box to print results
         message=paste("ERROR MESSAGE !",sep="")
			      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
        	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		      	# Create vertical container for file name entry
	    	  	vbox <- gtkVBoxNew(FALSE, 8)
		      	vbox$setBorderWidth(24)
	    	  	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
         message=paste("You did not created any project nor selected an existing one!",sep="")
        	label1 <- gtkLabelNewWithMnemonic(message) 

        	vbox$packStart(label1,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
      }else{
        project_name <- NULL
		  	   project_name=filename2$getText()
        #print("You are using the existing project: ")
        #print(project_name)
        res = calculateGUI11(project_name) 
      }   
    }
  res
  rm(list = ls())
	} #End of function


#####   Filtering   ######
GUI12 <- function(button, user.data) {
res <- NULL
    #if a new project was created then
    if (filename$getText()!="") {
      new_project_name <- NULL
			   new_project_name=filename$getText()
      #print("You are using the new project: ")
      #print(new_project_name)
      res = calculateGUI12(new_project_name)                 		
    }else{
      # if no project was created and no project was selected then
      if(filename2$getText()==""){
         print("You did not created any project nor selected an existing one!")
			      ########## Start dialog...
			      # Open a dialog box to print results
         message=paste("ERROR MESSAGE !",sep="")
			      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
        	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
 
		      	# Create vertical container for file name entry
	    	  	vbox <- gtkVBoxNew(FALSE, 8)
		      	vbox$setBorderWidth(24)
	    	  	dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
         message=paste("You did not created any project nor selected an existing one!",sep="")
        	label1 <- gtkLabelNewWithMnemonic(message) 

        	vbox$packStart(label1,FALSE,FALSE,0)
		       response <- dialog$run()
		      	# Return to previous window
			      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
      }else{
        project_name <- NULL
		  	   project_name=filename2$getText()
        #print("You are using the existing project: ")
        #print(project_name)
        res = calculateGUI12(project_name) 
      }   
    }
  res
  rm(list = ls())
	} #End of function

#####   David Analysis   ######
DavidGUI <- function(button, user.data) {
  res <- NULL
  #if a new project was created then
  if (filename$getText()!="") {
    new_project_name <- NULL
    new_project_name=filename$getText()
    #print("You are using the new project: ")
    #print(new_project_name)
    res = DavidFunctionalAnnotationChart(new_project_name, window)                 		
  }else{
    # if no project was created and no project was selected then
    if(filename2$getText()==""){
      print("You did not created any project nor selected an existing one!")
      ########## Start dialog...
      # Open a dialog box to print results
      message=paste("ERROR MESSAGE !",sep="")
      dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
      dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
      
      # Create vertical container for file name entry
      vbox <- gtkVBoxNew(FALSE, 8)
      vbox$setBorderWidth(24)
      dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
      message=paste("You did not created any project nor selected an existing one!",sep="")
      label1 <- gtkLabelNewWithMnemonic(message) 
      
      vbox$packStart(label1,FALSE,FALSE,0)
      response <- dialog$run()
      # Return to previous window
      if (response == GtkResponseType["ok"]) { dialog$destroy() }			 
    }else{
      project_name <- NULL
      project_name=filename2$getText()
      #print("You are using the existing project: ")
      #print(project_name)
      res = DavidFunctionalAnnotationChart(project_name, window) 
    }   
  }
  res
  rm(list = ls())
} #End of function

licence <- function(button, user.data) {
			dialog <- gtkDialogNewWithButtons("Licence",window, "modal","gtk-quit", GtkResponseType["cancel"])
		
			# Create vertical container for file name entry
			vbox <- gtkVBoxNew(FALSE, 8)
			vbox$setBorderWidth(24)
			dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)

  	label1 <- gtkLabelNewWithMnemonic("RNASeqGUI is released under the GPL license.") 
   label2 <- gtkLabelNewWithMnemonic("RNASeqGUI is provided AS IS and WITH ALL FAULTS, WITHOUT WARRANTY of any kind. ")
   label3 <- gtkLabelNewWithMnemonic("The Authors make NO WARRANTY that RNASeqGUI is free of defects or") 
   label4 <- gtkLabelNewWithMnemonic("is suitable for any particular purpose.") 
   label5 <- gtkLabelNewWithMnemonic("In NO EVENT shall the Authors be responsible for loss or damages arising") 
   label6 <- gtkLabelNewWithMnemonic("from the installation or use of the RNASeqGUI.")

  	vbox$packStart(label1,FALSE,FALSE,0)
   vbox$packStart(label2,FALSE,FALSE,0)
  	vbox$packStart(label3,FALSE,FALSE,0)
   vbox$packStart(label4,FALSE,FALSE,0)
  	vbox$packStart(label5,FALSE,FALSE,0)
   vbox$packStart(label6,FALSE,FALSE,0)

			response <- dialog$run()
			if (response == GtkResponseType["cancel"]) {
				dialog$destroy()
			}
   rm(list = ls())
	} #End of function

contact <- function(button, user.data) {
			dialog <- gtkDialogNewWithButtons("Contacts and INFO",window, "modal","gtk-quit", GtkResponseType["cancel"])
		
			# Create vertical container for file name entry
			vbox <- gtkVBoxNew(FALSE, 8)
			vbox$setBorderWidth(24)
			dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)

  	label3 <- gtkLabelNewWithMnemonic("Please, contact rnaseqgui@na.iac.cnr.it")
   label4 <- gtkLabelNewWithMnemonic("for any questions, feedback or to report bugs about RNASeqGUI.")
  	label1 <- gtkLabelNewWithMnemonic("RNASeqGUI has been founded by InterOmics FlagShip Project (http://www.interomics.eu).")
   label2 <- gtkLabelNewWithMnemonic("RNASeqGUI has been developed by Francesco Russo at CNR-IAC Naples (http://www.iac.cnr.it) .")

  	vbox$packStart(label3,FALSE,FALSE,0)
  	vbox$packStart(label4,FALSE,FALSE,0)
  	vbox$packStart(label1,FALSE,FALSE,0)
  	vbox$packStart(label2,FALSE,FALSE,0)

			response <- dialog$run()
			# Return to previous window
			if (response == GtkResponseType["cancel"]) {
				dialog$destroy()
			}
   rm(list = ls())
	} #End of function

####################################################################################################

#Execution started !

 # Create window
	window <- gtkWindow()
	# Add title
	window["title"] <- "RNASeqGUI"

	# Add a frame
	frame <- gtkFrameNew("    Please, EITHER create a new project OR select an existing one. Then, choose one of the Interfaces below. ")
	window$add(frame)

	# Create vertical container for file name entry
	vbox <- gtkVBoxNew(FALSE, 8)
	vbox$setBorderWidth(24)
	frame$add(vbox)

# Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 4)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add label in first column
	label <- gtkLabelNewWithMnemonic("_Choose a Project Name")
	hbox$packStart(label,FALSE,FALSE,0)
	# Add entry in the second column; named "filename"
	filename <- gtkEntryNew()
	filename$setWidthChars(32)
	label$setMnemonicWidget(filename)
	hbox$packStart(filename,FALSE,FALSE,0)
	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 hbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

	createproject <- gtkButtonNewWithMnemonic("Create a New Project", show = TRUE)
	gSignalConnect(createproject, "clicked", createRNASeqGUI_ProjectsDir)
	the.buttons$packStart(createproject,fill=F)

 # Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 4)
	vbox$packStart(hbox, FALSE, FALSE, 0)
 # Add label in first column
	label <- gtkLabelNewWithMnemonic("_Otherwise, choose an existing project")
	hbox$packStart(label,FALSE,FALSE,0)
	# Add entry in the second column; named "filename"
	filename2 <- gtkEntryNew()
	filename2$setWidthChars(20)
	label$setMnemonicWidget(filename2)
	hbox$packStart(filename2,FALSE,FALSE,0)

 # Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 hbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

	selectproject <- gtkButtonNewWithMnemonic("Select this project!", show = TRUE)
	gSignalConnect(selectproject, "clicked", selectprojectFun)
	the.buttons$packStart(selectproject,fill=F)

#########################

 # Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_BAM EXPLORATION SECTION")
	vbox$packStart(label,FALSE,FALSE,0)

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 4)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

 # button
	GUI6button <- gtkButtonNewWithMnemonic("_Bam Exploration Interface", show = TRUE)
	gSignalConnect(GUI6button, "clicked", GUI6)
	the.buttons$packStart(GUI6button,fill=F)

 # Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_COUNT SECTION")
	vbox$packStart(label,FALSE,FALSE,0)

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
	GUI7button <- gtkButtonNewWithMnemonic("_Read Count Interface", show = TRUE)
	gSignalConnect(GUI7button, "clicked", GUI7)
	the.buttons$packStart(GUI7button,fill=F)

 # Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_PRE-ANALYSIS SECTION")
	vbox$packStart(label,FALSE,FALSE,0)


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
	GUI1button <- gtkButtonNewWithMnemonic("_Data Exploration Interface", show = TRUE)
	gSignalConnect(GUI1button, "clicked", GUI1)
	the.buttons$packStart(GUI1button,fill=F)

 # button
	GUI5button <- gtkButtonNewWithMnemonic("_Normalization Interface", show = TRUE)
	gSignalConnect(GUI5button, "clicked", GUI5)
	the.buttons$packStart(GUI5button,fill=F)

 # button
	GUI12button <- gtkButtonNewWithMnemonic("_Filtering Interface", show = TRUE)
	gSignalConnect(GUI12button, "clicked", GUI12)
	the.buttons$packStart(GUI12button,fill=F)

 # Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_DATA ANALYSIS SECTION")
	vbox$packStart(label,FALSE,FALSE,0)


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
	GUI2button <- gtkButtonNewWithMnemonic("_Data Analysis Interface", show = TRUE)
	gSignalConnect(GUI2button, "clicked", GUI2)
	the.buttons$packStart(GUI2button,fill=F)

 # Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_POST ANALYSIS SECTION")
	vbox$packStart(label,FALSE,FALSE,0)

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
	GUI3button <- gtkButtonNewWithMnemonic("_Result Inspection Interface", show = TRUE)
	gSignalConnect(GUI3button, "clicked", GUI3)
	the.buttons$packStart(GUI3button,fill=F)

 # button
	GUI4button <- gtkButtonNewWithMnemonic("_Result Comparison Interface", show = TRUE)
	gSignalConnect(GUI4button, "clicked", GUI4)
	the.buttons$packStart(GUI4button,fill=F)

 # Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_GO/PATHWAY SECTION")
	vbox$packStart(label,FALSE,FALSE,0)

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
	GUI11button <- gtkButtonNewWithMnemonic("_Graphite Interface", show = TRUE)
	gSignalConnect(GUI11button, "clicked", GUI11)
	the.buttons$packStart(GUI11button,fill=F)

# button
DavidGUIbutton <- gtkButtonNewWithMnemonic("_David Interface", show = TRUE)
gSignalConnect(DavidGUIbutton, "clicked", DavidGUI)
the.buttons$packStart(DavidGUIbutton,fill=F)

 # button
	GUI9button <- gtkButtonNewWithMnemonic("_Gage Interface", show = TRUE)
	gSignalConnect(GUI9button, "clicked", GUI9)
	the.buttons$packStart(GUI9button,fill=F)

 # Add separator
	vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0)

	label <- gtkLabelNewWithMnemonic("_REPORT AND UTILITY SECTION")
	vbox$packStart(label,FALSE,FALSE,0)

 # Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

 # Add buttons
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

 # button
 GUI10button <- gtkButtonNewWithMnemonic("_Report", show = TRUE)
	gSignalConnect(GUI10button, "clicked", GUI10)
	the.buttons$packStart(GUI10button,fill=FALSE)

 # button
	GUI8button <- gtkButtonNewWithMnemonic("_Utility Interface", show = TRUE)
	gSignalConnect(GUI8button, "clicked", GUI8)
	the.buttons$packStart(GUI8button,fill=F)

 infoGUI()


}

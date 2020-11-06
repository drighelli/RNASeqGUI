calculateGUI6 <-
function(project_name) {

 Project <- project_name
 message=paste("You are using the project: ",Project,sep="")
 print(message)

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


performStatistics_ReadCount <- function(button, user.data) {  
  Bam.Folder <- BamFolder$getText()
  
  if(Sys.info()[[1]]=="Windows"){
    Bam.Folder = strsplit(Bam.Folder,"\\\\")
    Bam.FolderNew = Bam.Folder[[1]][2]
    Bam.FolderNew = paste("\\",Bam.FolderNew,sep="")
    Bam.FolderNew
    
    for( i in 4:length(Bam.Folder[[1]])-1)
    {
      Bam.FolderNew = paste(Bam.FolderNew,Bam.Folder[[1]][i],sep="\\")
    }
    print("Bam Folder selected: ")
    print(  Bam.FolderNew)
    
    res <- NULL
    #the.file = filename$getText()
    res = readCount(Bam.FolderNew,Project)
    
  }else{
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

		res <- NULL
  #the.file = filename$getText()
  res = readCount(Bam.FolderNew,Project)
  }
  rm(list = ls())
	} #End of function


performStatistics_PlotQuality <- function(button, user.data) {
  Bam.Folder <- BamFolder$getText()
  if(Sys.info()[[1]]=="Windows"){
    Bam.Folder = strsplit(Bam.Folder,"\\\\")
    Bam.FolderNew = Bam.Folder[[1]][2]
    Bam.FolderNew = paste("\\",Bam.FolderNew,sep="")
    Bam.FolderNew
    
    for( i in 4:length(Bam.Folder[[1]])-1)
    {
      Bam.FolderNew = paste(Bam.FolderNew,Bam.Folder[[1]][i],sep="\\")
    }
    print("Bam Folder selected: ")
    print(  Bam.FolderNew)
    
    res <- NULL
    #the.file = filename$getText()
    res = plotQual(Bam.FolderNew,Project)   
    
  }else{
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

	 	res <- NULL
   #the.file = filename$getText()
   res = plotQual(Bam.FolderNew,Project)   
  }
   rm(list = ls())                 
	} #End of function


performStatistics_perBaseQuality <- function(button, user.data) {
  Bam.Folder <- BamFolder$getText()
  if(Sys.info()[[1]]=="Windows"){
    Bam.Folder = strsplit(Bam.Folder,"\\\\")
    Bam.FolderNew = Bam.Folder[[1]][2]
    Bam.FolderNew = paste("\\",Bam.FolderNew,sep="")
    Bam.FolderNew
    
    for( i in 4:length(Bam.Folder[[1]])-1)
    {
      Bam.FolderNew = paste(Bam.FolderNew,Bam.Folder[[1]][i],sep="\\")
    }
    print("Bam Folder selected: ")
    print(  Bam.FolderNew)
    
    res <- NULL
    #the.file = filename$getText()
    res = perBaseQuality(Bam.FolderNew,Project)
    
  }else{
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

 		res <- NULL
   #the.file = filename$getText()
   res = perBaseQuality(Bam.FolderNew,Project)
  }
   rm(list = ls())                   
	} #End of function


performStatistics_ChNumber <- function(button, user.data) {
  Bam.Folder <- BamFolder$getText()
  if(Sys.info()[[1]]=="Windows"){
    Bam.Folder = strsplit(Bam.Folder,"\\\\")
    Bam.FolderNew = Bam.Folder[[1]][2]
    Bam.FolderNew = paste("\\",Bam.FolderNew,sep="")
    Bam.FolderNew
    
    for( i in 4:length(Bam.Folder[[1]])-1)
    {
      Bam.FolderNew = paste(Bam.FolderNew,Bam.Folder[[1]][i],sep="\\")
    }
    print("Bam Folder selected: ")
    print(  Bam.FolderNew)
    
    res <- NULL	
    #the.file = filename$getText()
    res = ChNumber(Bam.FolderNew,Project)
    
  }else{
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

	 	res <- NULL	
   #the.file = filename$getText()
   res = ChNumber(Bam.FolderNew,Project)
  }
   rm(list = ls())                    
	} #End of function


performStatistics_plotNtFrequency <- function(button, user.data) {
  Bam.Folder <- BamFolder$getText()
  if(Sys.info()[[1]]=="Windows"){
    Bam.Folder = strsplit(Bam.Folder,"\\\\")
    Bam.FolderNew = Bam.Folder[[1]][2]
    Bam.FolderNew = paste("\\",Bam.FolderNew,sep="")
    Bam.FolderNew
    
    for( i in 4:length(Bam.Folder[[1]])-1)
    {
      Bam.FolderNew = paste(Bam.FolderNew,Bam.Folder[[1]][i],sep="\\")
    }
    print("Bam Folder selected: ")
    print(  Bam.FolderNew)
    
    res <- NULL
    #the.file = filename$getText()
    res = plotNtFreq(Bam.FolderNew,Project) 
    
  }else{
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

	 	res <- NULL
   #the.file = filename$getText()
   res = plotNtFreq(Bam.FolderNew,Project) 
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
   label1 <- gtkLabelNewWithMnemonic("Select a bam folder by clicking on the corresponding Open button. ")
   label2 <- gtkLabelNewWithMnemonic("To select the entire bam folder, select just one bam file inside  ")
   label3 <- gtkLabelNewWithMnemonic("the bam folder you want to use. The entire folder will be loaded! ")
   label4 <- gtkLabelNewWithMnemonic("Then, click on the button you want to use. ")
   label5 <- gtkLabelNewWithMnemonic("If you are using the 'demo' bam folder of the 'example', remember to delete '2L_1.bam' ")
   label6 <- gtkLabelNewWithMnemonic("file from the 'demo' folder before clicking on the 'Mean Quality of Reads' and 'Per Base Quality of Reads' ")
   label7 <- gtkLabelNewWithMnemonic("For further information, see http://bioinfo.na.iac.cnr.it/RNASeqGUI/manual.pdf ")

         vbox$packStart(label1,FALSE,FALSE,0)
         vbox$packStart(label2,FALSE,FALSE,0)
        	vbox$packStart(label3,FALSE,FALSE,0)
         vbox$packStart(label4,FALSE,FALSE,0)
        	vbox$packStart(label5,FALSE,FALSE,0)
         vbox$packStart(label6,FALSE,FALSE,0)
        	vbox$packStart(label7,FALSE,FALSE,0)
		       response <- dialog$run()
		       response <- dialog$run()
		      	# Return to previous window
			      if (GtkResponseType["ok"]) { dialog$destroy() }
}

#####################################################
	# Create window
	window <- gtkWindow()
	# Add title
	window["title"] <- "RNASeqGUI"

	# Add a frame
 title=paste("   \n    Bam Exploration Interface is ready to work on ",Project, " project.", sep="")
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
	# Add entry in the second column; named "filename"
	BamFolder <- gtkEntryNew()
	BamFolder$setWidthChars(60)
	label$setMnemonicWidget(BamFolder)
	hbox$packStart(BamFolder,FALSE,FALSE,0)
	buttonOpen <- gtkButtonNewFromStock("gtk-open")
	gSignalConnect(buttonOpen, "clicked", openBam)
	hbox$packStart(buttonOpen,FALSE,FALSE,0)

########################

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
	ReadCount <- gtkButtonNewWithMnemonic("Read Counts", show = TRUE)
	gSignalConnect(ReadCount, "clicked", performStatistics_ReadCount)
	the.buttons$packStart(ReadCount,fill=FALSE)

 # button
	PlotQuality <- gtkButtonNewWithMnemonic("Mean Quality of Reads", show = TRUE)
	gSignalConnect(PlotQuality, "clicked", performStatistics_PlotQuality)
	the.buttons$packStart(PlotQuality,fill=FALSE)

 # button
	perBaseQuality <- gtkButtonNewWithMnemonic("Per Base Quality of Reads", show = TRUE)
	gSignalConnect(perBaseQuality, "clicked", performStatistics_perBaseQuality)
	the.buttons$packStart(perBaseQuality,fill=FALSE)

	vbox$packStart(label,FALSE,FALSE,0)

	# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)

 # button
	ChNumber <- gtkButtonNewWithMnemonic("Reads Per Chromosome", show = TRUE)
	gSignalConnect(ChNumber, "clicked", performStatistics_ChNumber)
	the.buttons$packStart(ChNumber,fill=FALSE)

 # button
	plotNtFrequency <- gtkButtonNewWithMnemonic("Nucleotide Frequencies", show = TRUE)
	gSignalConnect(plotNtFrequency, "clicked", performStatistics_plotNtFrequency)
	the.buttons$packStart(plotNtFrequency,fill=FALSE)

}

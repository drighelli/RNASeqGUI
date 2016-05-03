calculateGUI11 <- function(project_name) {
  
  Project <- project_name
  message=paste("You are using the project: ", Project, sep="")
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
  
  ########
  SPIAAnalysis <- function(button, user.data) {
    
    res <- NULL
    file.name <- NULL
    file.name <- filename$getText()
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
#     compare.input <- compare$getText()
#     stouffer.input <- stouffer$active
    geneSet1 <- set1$active
    geneSet2 <- set2$active
    geneSet3 <- set3$active
    datasetsel <- datasetselEntry$getText()
    specie <- specieEntry$getText()
    verbose <- verboseCheck$active
    prepareSPIA <- prepareSPIACheck$active
  
  if(verbose){
    print('verbose active!')
  }else{
    print('verbose NOT active!')
  }
  
  if(prepareSPIA){
    print('PrepareSPIA active!')
  }else{
    print('prepareSPIA NOT active!')
  }

    print("Gene Sets selected: ")
    if (geneSet1==TRUE){
      print("reactome")
      dbsel <- 'reactome'
    }else if(geneSet2==TRUE){
      print("kegg")
      dbsel <- 'kegg'
    } else {
      print('biocarta')
      dbsel <- 'biocarta'
    }
    
    res <- SPIA_function(dbsel, datasetsel, file.name = file.name, species=specie, Project=Project, verbose=verbose, 
                         print.names.flag=verbose, prepare.SPIA.flag=prepareSPIA)
    PrintSPIAReport(dbsel, datasetsel, file.name = file.name, species=specie, Project=Project, verbose=verbose, 
                    print.names.flag=verbose, prepare.SPIA.flag=prepareSPIA)

    if(Sys.info()[[1]]=="Windows"){
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
        error <- 'No Pathways Enriched for this gene set!'
        dialog <- gtkMessageDialogNew(window, c("modal", "destroy-with-parent"), "info", "ok", error)
        dialog$run()
        dialog$destroy()
      }
      
    }else{
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
        error <- 'No Pathways Enriched for this gene set!'
        dialog <- gtkMessageDialogNew(window, c("modal", "destroy-with-parent"), "info", "ok",error)
        dialog$run()
        dialog$destroy()
      }
  }
    rm(list = ls())
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
    label1 <- gtkLabelNewWithMnemonic("Select expression data by clicking on the corresponding 'Open' button.")
    label2 <- gtkLabelNewWithMnemonic("Select the Gene Sets by choosing either 'Reactome', 'Kegg' or 'Biocarta'.")
    label3 <- gtkLabelNewWithMnemonic("Check the 'Prepare SPIA' checkbox if you don't have a SPIA database file prepared.") 
    label4 <- gtkLabelNewWithMnemonic("Check the 'Verbose' checkbox if you want additional output in the R shell.")
    label5 <- gtkLabelNewWithMnemonic("The 'Species Data Set' and the relative 'Kegg code' is uneditable because the tool is actually performed only for human species.")
    label6 <- gtkLabelNewWithMnemonic("For further information, see http://www.bioconductor.org/packages/release/bioc/html/graphite.html .")
     
    vbox$packStart(label1,FALSE,FALSE,0)
    vbox$packStart(label2,FALSE,FALSE,0)
    vbox$packStart(label3,FALSE,FALSE,0)
    vbox$packStart(label4,FALSE,FALSE,0)
    vbox$packStart(label5,FALSE,FALSE,0)
    vbox$packStart(label6,FALSE,FALSE,0)
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
  title=paste("    \n Gene-set/Pathway SPIA via Graphite Interface is ready to work on ",Project, " project.", sep="")
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
  
  # Add horizontal container for every widget line
  hbox <- gtkHBoxNew(FALSE, 8)
  label <- gtkLabelNewWithMnemonic(" Select the Pathway Set ")
  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)
  
  ## Create a radio button with a GtkEntry widget 
  set1 <- gtkRadioButtonNewWithLabel(group = NULL,"Reactome", show = TRUE)
  #The two non-default options should only be used when rank.test = FALSE.
  ## Create a radio button with a label 
  set2 <- gtkRadioButtonNewWithLabelFromWidget(set1,"Kegg")
  ## Create a radio button with a label 
  set3 <- gtkRadioButtonNewWithLabelFromWidget(set2,"Biocarta")
  
  ## Pack them into a box, then show all the widgets
  hbox$packStart(set1, TRUE, TRUE, 2)
  hbox$packStart(set2, TRUE, TRUE, 2)
  hbox$packStart(set3, TRUE, TRUE, 2)
  label$setMnemonicWidget(set1)  
  label$setMnemonicWidget(set2) 
  label$setMnemonicWidget(set3) 
  
  # Add horizontal container for every widget line
  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)

  label <- gtkLabelNewWithMnemonic("\t\t\t\t\tPrepare SPIA?")
  hbox$packStart(label,FALSE,FALSE,0)
  prepareSPIACheck <- gtkCheckButton()
  hbox$packStart(prepareSPIACheck,FALSE,FALSE,0)

  label <- gtkLabelNewWithMnemonic("\t\t\tVerbose?")
  hbox$packStart(label,FALSE,FALSE,0)
  verboseCheck <- gtkCheckButton()
  hbox$packStart(verboseCheck,FALSE,FALSE,0)


  hbox <- gtkHBoxNew(FALSE, 8)
  label <- gtkLabelNewWithMnemonic("\n Gene Id conversion")
  vbox$packStart(label,FALSE,FALSE,0)
  vbox$packStart(hbox, FALSE, FALSE, 0)
  
  # datasetsel
  label <- gtkLabelNewWithMnemonic("_Specie Data Set?")
  hbox$packStart(label,FALSE,FALSE,0)
  datasetselEntry <- gtkEntryNew()
  datasetselEntry$setWidthChars(60)
  datasetselEntry$setText('hsapiens_gene_ensembl')#"dmelanogaster_gene_ensembl")
  gtkEntrySetEditable(datasetselEntry, FALSE)##
  #colormap<-gtkWidgetGetColormap(datasetselEntry)
  hbox$packStart(datasetselEntry,FALSE,FALSE,0)
  label$setMnemonicWidget(datasetselEntry)

  # specie
  label <- gtkLabelNewWithMnemonic("_Specie (kegg.code) ?")
  hbox$packStart(label,FALSE,FALSE,0)
  specieEntry <- gtkEntryNew()
  specieEntry$setWidthChars(5)
  specieEntry$setText("hsa")
  gtkEntrySetEditable(specieEntry, FALSE)##
  hbox$packStart(specieEntry,FALSE,FALSE,0)
  label$setMnemonicWidget(specieEntry)
  
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
  
  label <- gtkLabelNewWithMnemonic("_Start Gene-set/Pathway Analysis")
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
  spiabut <- gtkButtonNewWithMnemonic("_SPIA", show = TRUE)
  gSignalConnect(spiabut, "clicked", SPIAAnalysis)
  the.buttons$packStart(spiabut,fill=F)

}

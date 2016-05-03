
# if(Sys.info()[[1]]=='Darwin'){
#   source('Desktop/Dropbox/Lavori/IAC/coding/davidquery/DavidFunctions.R')  
# }else{
#   source('Scrivania/Dropbox/Lavori/IAC/coding/davidquery/DavidFunctions.R')
# }

# source('WaitingWindow.R')
# source('DavidFunctions.R')
require(RGtk2)

MyDialog <- function(parent.w, type.d, message, title){
  dialog <- gtkMessageDialogNew(parent = parent.w, flags = NULL , type=type.d, buttons = 'ok', message)
  dialog['title'] <- title
  if((dialog$run() == GtkResponseType['ok']) || (dialog$run() == GtkResponseType['close'])) {
    dialog$destroy()
  }
  return(dialog)
}

ShowDataFrameInTable <- function(data.frame, title="Tabular view of DAVID results", window.p=NULL, w.width=900, w.height=350) {
#     load('results.data.frame.RData')
#   data.frame <- results.data.frame
  results.gtk.data.frame <- rGtkDataFrame(data.frame)
  view<-gtkTreeView(results.gtk.data.frame)
  mapply( view$insertColumnWithAttributes, 
          position=-1,
          title=colnames(results.gtk.data.frame),
          cell= list(gtkCellRendererText()), 
          text=seq_len(ncol(results.gtk.data.frame)) -1
          )
  
  window <- gtkWindow(show = FALSE)
  gtkWindowSetDefaultSize(window, width = w.width, height = w.height)
  
  #   gtkWindowSetTransientFor(window, window.p)
  if(Sys.info()[[1]]=='Darwin'){
    gtkWindowSetModal(window, TRUE) ## comment to have multiple windows
  }
  
  window$setTitle(title) 
  scrolled_window <- gtkScrolledWindow() 
  window$add(scrolled_window) 
  scrolled_window$add(view)
  gtkWidgetShow(window)
}

DavidFunctionalAnnotationChart <- function(projectname, parent.window){
  Project <- projectname
  
  openFile <- function(button, user.data) {
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
  
  infoFunction <- function(button, user.data) {
    ########## Start dialog...
    # Open a dialog box to print results
    message=paste("Vignette",sep="")
    dialog <- gtkDialogNewWithButtons(message,window, "modal","gtk-ok", GtkResponseType["ok"])
    #dialog$getContentArea()$packStart(vbox, FALSE, FALSE, 0)
    
    # Create vertical container for file name entry
    vbox <- gtkVBoxNew(FALSE, 8)
    vbox$setBorderWidth(24)
    dialog$getContentArea()$packStart(vbox)

    label1 <- gtkLabelNewWithMnemonic(paste0("Select differential expression data file by clicking on the corresponding 'Open' button.\n",
                                             "It can also be a single column file within a list of differential expressed genes.\n",
                                             "In the column field, insert the number of the column containing the gene list."))
    
    label2 <- gtkLabelNewWithMnemonic(paste0("In the Gene Identifier field, select the identifier of your genes.\n",
                                             "WARNING: If your genes are genesymbols, you have to select the specie of the genes,\n",
                                             "because they will be converted to ensembl gene id.\n",
                                             "We are sorry, but this is due to DAVID constraints."))
    
    label3 <- gtkLabelNewWithMnemonic(paste0("The list type indicate if you are using a gene list or a background list\n",
                                             "Further information here: http://david.abcc.ncifcrf.gov/helps/list_manager.html#upload"))
    label4 <- gtkLabelNewWithMnemonic(paste0("You can choose two types of analysis: Pathways or Gene Ontology.\n",
                                             "In the next step a popup window will be open.\n",
                                             "- Pathway Analysis: you have to choose one of the pathway database to use.\n",
                                             "- Gene Ontology: there are two categories you can choose: FAT and ALL.\n",
                                             "                 Think about them as two independent analysis to perform.\n",
                                             "-- In each category all the choosen terms type will be analysed together."))
    label5 <- gtkLabelNewWithMnemonic(paste0("In the list name field you have to enter a name for your gene list.\n",
                                             "WARNING: DAVID connects this name to your gene list for a certain amount of time!\n",
                                             "         So, if you change the gene list, you have to change the list name\n",
                                             "         until DAVID forgot the initially associated gene list."))
    label6 <- gtkLabelNewWithMnemonic(paste0("The email field is optional because we have registered a RNASeqGUI user on the DAVID site.\n",
                                             "Any registered user have some limitations, so if you encounter problems you can register a\n",
                                             "new user on the DAVID site and insert your own email in this field.\n",
                                             "Registration link: http://david.abcc.ncifcrf.gov/webservice/register.htm"))
    
    vbox$packStart(label1)
    vbox$packStart(gtkHSeparatorNew())
    vbox$packStart(label2)
    vbox$packStart(gtkHSeparatorNew())
    vbox$packStart(label3)
    vbox$packStart(gtkHSeparatorNew())
    vbox$packStart(label4)
    vbox$packStart(gtkHSeparatorNew())
    vbox$packStart(label5)
    vbox$packStart(gtkHSeparatorNew())
    vbox$packStart(label6)
    
    response <- dialog$run()
    # Return to previous window
    if (GtkResponseType["ok"]) {
      dialog$destroy()
    }
  }
  
  CheckInputData <- function(button, user.data) {
    ## check input data function ################################################
    file.name <- filename$GetText()
    
    if(file.name == "") { ##or file not exists
      print('ERROR: empty filename!')
      dialog <- MyDialog(parent.w=window, type.d='error', message='You have to enter a valid input filename!\nEmpty filename.', title='ERROR')
      return(1)
    } else if(!file.exists(file.name)) {
      print('ERROR: filename doesn\'t exist!')
      dialog <- MyDialog(parent.w=window, type.d='error', message='You have to enter a valid input filename!\nFile does not exist, please check it!', title='ERROR')
      return(1)
    }
    
    gene.list.position <- column$GetText()
    if(gene.list.position != ""){
      if(nzchar(gsub("[0-9]", "", gene.list.position))){
        print('ERROR: only numbers!')
        dialog <- MyDialog(window, 'error', message='You can enter ONLY NUMBERS in the column field!', title='ERROR')        
        return(1)
      }
    } else {
      print('ERROR: empty column field!')
      dialog <- MyDialog(window, 'error', message='You have to enter a number in the column field!',title= 'ERROR')
      return(1)
    }
    gene.list.position <- as.numeric(gene.list.position)
    
    specie.identifier = ""
    gene.identifier <- identifier$GetActiveText()
    if(length(gene.identifier) == 0){
      print("ERROR: please select a gene identifier!")
      dialog <- MyDialog(window, "error", message="You have to select an identifier type for your list!", title="ERROR")
      return(1)
    } else if (gene.identifier == "GENE_SYMBOL") {
      specie.identifier <- specie$GetActiveText()
      if(length(specie.identifier) == 0){
        print("ERROR: please select a specie!")
        dialog <- MyDialog(window, "error", message="You have to select a specie, because you choose GENE_SYMBOL identifier!", title="ERROR")
        return(1)
      }
    }
    
    for(item in list.type.gp) {
      if(item$active) {
        if(item$label == "Gene List") {
          list.type="Gene"
          break
        }else if(item$label == "Background") {
          list.type="Background"
          break
        }
      }
    }
    
    for(item in analysis.type.gp) {
      if(item$active) {
        if(item$label == "GO Analysis") {
          analysis.type="GO"
          break
        }else if(item$label == "Pathway Analysis") {
          analysis.type="Pathway"
          break
        }
      }
    }
    
    list.name <- listname$GetText()
    if(list.name == ""){
      print('ERROR: empty listname!')
      dialog <- MyDialog(window, 'error', message='You have to enter a valid name for your list!', title='ERROR')
      return(1)
    }
    
    filter.column <- adjustment$GetActiveText()
    if(filter.column=="No"){
      filter.flag <- FALSE
      filter.threshold <- Inf
    } else {
      filter.flag <- TRUE
      filter.threshold <- as.numeric(threshold$GetActiveText())
    }
    
    filter.list <- list(flag=filter.flag, col.name=filter.column, thr=filter.threshold)
    
    #     out.file.name <- outfilename$GetText()
    #     if(out.file.name == "") {
    #       print('ERROR: empty output filename!')
    #       dialog <- MyDialog(window, 'error', message='You have to enter a valid output filename!', title='ERROR')
    #       return(1)
    #     }
    
    out.file.name <- ""
    DavidQuery(file.name, gene.list.position, gene.identifier, list.type, analysis.type, list.name, out.file.name, specie.identifier, filter.list)
      ## end checkinputdata ##############################################
  }
    
    
  DavidQuery <- function(file.name, gene.list.position, gene.identifier, list.type, analysis.type, 
                         list.name, out.file.name, specie="", filter.list) {
    ## general david function ##############################

    SelectPathwayFunction<-function(parent.w, user.data) {
      ## pathway main function ##############################################
      ## da inserire nella sezione gui di pathway
      checkPathwayFunction <-function(entry, user.data) {
        for(item in pathway.gp) {
          if(item$active) {
            pathway.db.selected <- item$label
            break
          }
        }
        
        #       waiting.dialog <- MyWaitingDialog(parent.w=mini.window, message='Please wait for DAVID response!', title='Waiting for DAVID response')
        ##gtkWidgetSetSensitive(mini.window,FALSE)##
        results.data.frame <- DavidAnalysis(mini.window, file.name, gene.list.position, gene.identifier, 
                                            list.type, list.name, out.file.name, pathway.db.selected, 
                                            analysis.type, Project, specie, filter.list)
        #       waiting.dialog$destroy()      
        #save(results.data.frame, file='results.data.frame.RData')
        
        if(exists('results.data.frame') && dim(results.data.frame)[1]!=0){
          title.pw <- paste0('Tabular view of DAVID ', pathway.db.selected, ' results')
          ShowDataFrameInTable(data.frame=results.data.frame, window.p=mini.window, title=title.pw)
        } else {
          print(paste0('The pathway enrichment analysis on ', pathway.db.selected, 'produced NO results!'))
          dialog <- MyDialog(window, 'info', message=paste0('The pathway enrichment analysis on ', pathway.db.selected, 'produced NO results!'), title='INFORMATION')
        }
        
        ## end of pathway analysis elaboration ################################
      }
      
      ## pathway selection gui components ####################################
      mini.window <- gtkWindow()
      gtkWindowSetTransientFor(mini.window, parent.w)
      gtkWindowSetModal(mini.window, TRUE)
      mini.window['title'] <- 'Pathway Database'
      mini.window$SetResizable(FALSE)
      frame <- gtkFrameNew('Select Pathway Database')
      mini.window$add(frame)
      vvvbox <- gtkVBoxNew(FALSE, 8)
      vvvbox$setBorderWidth(24)
      
      label <- gtkLabelNewWithMnemonic("_Select Analysis type:")
      labels <- c('KEGG_PATHWAY', 'BIOCARTA', 'REACTOME_PATHWAY', 'PANTHER_PATHWAY', 'BBID', 'EC_NUMBER')
      pathway.gp <- list()
      # list for group
      pathway.gp[[labels[1]]] <- gtkRadioButton(label=labels[1])
      for(lab in labels[-1])
        pathway.gp[[lab]] <- gtkRadioButton(pathway.gp, label=lab)
      for(item in pathway.gp )
        vvvbox$packStart(item, FALSE, FALSE, 0)
      
      frame$add(vvvbox)
      
      # Add button
      the.buttons <- gtkHButtonBoxNew()
      the.buttons$setBorderWidth(5)
      vvvbox$add(the.buttons)
      the.buttons$setLayout("spread")
      the.buttons$setSpacing(40)
        
      # button
      cancelbut <- gtkButtonNewWithMnemonic("_Cancel", show = TRUE)
      gSignalConnect(cancelbut, "clicked", mini.window$destroy)
      the.buttons$packStart(cancelbut, fill=F)
    
      # button
      pathwaybut <- gtkButtonNewWithMnemonic("_Analysis", show = TRUE)
      gSignalConnect(pathwaybut, "clicked", checkPathwayFunction)
      the.buttons$packStart(pathwaybut, fill=F)
        
      ## end pathway function and components ##################### 
    }
    
    SelectGOFunction<-function(parent.w, user.data) {
      ## main go function ########################################
      ## da inserire nella sezione gui di pathway
      checkGOFunction <-function(entry, user.data) {
        ## check go input data ###################################
        
        alls <- c()
        for(item in check.box.group.all[-1]) {
          if (item['active']==TRUE) {
            if( length(grep('MOLECULAR FUNCTION', item['label']))!=0 ) {
              alls <- c(alls, 'GOTERM_MF_ALL')
            } else if( length(grep('BIOLOGICAL PROCESS', item['label']))!=0 ) {
              alls <- c(alls, 'GOTERM_BP_ALL') 
            } else if( length(grep('CELLULAR COMPONENT', item['label']))!=0 ) {
              alls <- c(alls, 'GOTERM_CC_ALL') 
            }
            #print(item['label'])
          }
        }
        
        fats <- c()
        for(item in check.box.group.fat[-1]) {
          if (item['active']==TRUE){
            if( length(grep('MOLECULAR FUNCTION', item['label']))!=0 ) {
              fats <- c(fats, 'GOTERM_MF_FAT')
            } else if( length(grep('BIOLOGICAL PROCESS', item['label']))!=0 ) {
              fats <- c(fats, 'GOTERM_BP_FAT') 
            } else if( length(grep('CELLULAR COMPONENT', item['label']))!=0 ) {
              fats <- c(fats, 'GOTERM_CC_FAT') 
            }
            #print(item['label'])
          }
        }
        
        if(length(alls)!=0) {
          alls.results.data.frame <- DavidAnalysis(mini.window, file.name, gene.list.position, gene.identifier, 
                                            list.type, list.name, out.file.name, db.selected=alls, analysis.type, Project, specie,filter.list)
        }
        
        if(length(fats)!=0) {
          fats.results.data.frame <- DavidAnalysis(mini.window, file.name, gene.list.position, gene.identifier, 
                                                   list.type, list.name, out.file.name, db.selected=fats, analysis.type, Project, specie, filter.list)
        }
      
        
        if(exists('alls.results.data.frame') && dim(alls.results.data.frame)[1]!=0) {
          title.alls <- paste0('Tabular view of DAVID GO_ALL results')
          ShowDataFrameInTable(data.frame=alls.results.data.frame, window.p=mini.window, title=title.alls)
        } else {
          print('The GO_ALL enrichment analysis produced NO results!')
          dialog <- MyDialog(window, 'info', message='The GO_ALL enrichment analysis produced NO results!', title='INFORMATION')
        }
        
        if(exists('fats.results.data.frame') && dim(fats.results.data.frame)[1]!=0) {
          title.fats <- paste0('Tabular view of DAVID GO_FAT results')
          ShowDataFrameInTable(data.frame=fats.results.data.frame, window.p=mini.window, title=title.fats)
        } else {
          print('The GO_FAT enrichment analysis produced NO results!')
          dialog <- MyDialog(window, 'info', message='The GO_FAT enrichment analysis produced NO results!', title='INFORMATION')
        }
        
        
        ## end of go input data #################################
      }
      
      ## go gui components ######################################
      mini.window <- gtkWindow()
      gtkWindowSetTransientFor(mini.window, parent.w)
      gtkWindowSetModal(mini.window, TRUE)
      mini.window['title'] <- 'Gene Ontology Class'
      mini.window$SetResizable(FALSE)
      frame <- gtkFrameNew('Select GO Class(es)')
      mini.window$add(frame)
      govvvbox <- gtkVBoxNew(FALSE, 8)
      govvvbox$setBorderWidth(24)
      
      label <- gtkLabelNewWithMnemonic("_Select GO type:")
      
      labels.all <- c('All ALLs Together','MOLECULAR FUNCTION\nGOTERM_MF_ALL', 'BIOLOGICAL PROCESS\nGOTERM_BP_ALL', 'CELLULAR COMPONENT\nGOTERM_CC_ALL')
      labels.fat <- c('All FATs Together','MOLECULAR FUNCTION\nGOTERM_MF_FAT', 'BIOLOGICAL PROCESS\nGOTERM_BP_FAT', 'CELLULAR COMPONENT\nGOTERM_CC_FAT')
      
      check.box.group.all <- list()
      for(item in labels.all) {
        check.box <- gtkCheckButton(item)
        check.box['active'] <- FALSE
        check.box.group.all <- c(check.box.group.all, check.box)
        govvvbox$packStart(check.box, FALSE, FALSE, 0)
      }
    
      gSignalConnect(check.box.group.all[[1]], 'toggled', function(button) {
        state <- ifelse(button$active, "active", "inactive")
        if(state=='active') {
          for(item in check.box.group.all[-1]) {
            item['active'] <- TRUE
            item['sensitive'] <- FALSE
          }
          
        }else{
          for(item in check.box.group.all[-1]) {
            item['active'] <- FALSE
            item['sensitive'] <- TRUE
          }
        }
      })
    
      govvvbox$packStart(gtkHSeparator() , FALSE, FALSE, 0)
    
      check.box.group.fat <- list()
      for(item in labels.fat) {
        check.box <- gtkCheckButton(item)
        check.box['active'] <- FALSE
        check.box.group.fat <- c(check.box.group.fat, check.box)
        govvvbox$packStart(check.box, FALSE, FALSE, 0)
      }
    
      gSignalConnect(check.box.group.fat[[1]], 'toggled', function(button) {
        state <- ifelse(button$active, "active", "inactive")
        if(state=='active') {
          for(item in check.box.group.fat[-1]) {
            item['active'] <- TRUE
            item['sensitive'] <- FALSE
          }
          
        }else{
          for(item in check.box.group.fat[-1]) {
            item['active'] <- FALSE
            item['sensitive'] <- TRUE
          }
        }
        
      })
    
      frame$add(govvvbox)
      
      # Add button
      the.buttons <- gtkHButtonBoxNew()
      the.buttons$setBorderWidth(5)
      govvvbox$add(the.buttons)
      the.buttons$setLayout("spread")
      the.buttons$setSpacing(40)
      
      # button
      cancelbut <- gtkButtonNewWithMnemonic("_Cancel", show = TRUE)
      gSignalConnect(cancelbut, "clicked", mini.window$destroy)
      the.buttons$packStart(cancelbut, fill=F)
    
      # button
      gobut <- gtkButtonNewWithMnemonic("_Analysis", show = TRUE)
      gSignalConnect(gobut, "clicked", checkGOFunction)
      
      the.buttons$packStart(gobut, fill=F)
      ## end go function ##########################################
    }
    
  
    if(analysis.type=='Pathway'){
      SelectPathwayFunction(parent.w=window, user.data)
    } else {
      SelectGOFunction(parent.w=window, user.data)
    }
    ## end of general david function ##############################
  }
  
  ## main david gui ###############################################
  window <- gtkWindow()
  gtkWindowSetTransientFor(window, parent.window)
  gtkWindowSetDestroyWithParent(window, setting=TRUE)
  # Add title
  window["title"] <- "David Interface"
  window$SetResizable(FALSE)
  # Add a frame
  title=paste("   \n    David Interface is ready to work on ",Project, " project.", sep="")
  frame <- gtkFrameNew(title)
  window$add(frame)
  
  # Create vertical container for file name entry
  vbox <- gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)
  
  # Add horizontal container for every widget line
  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)
  
  # Add label in first column
  label <- gtkLabelNewWithMnemonic("_Select differential expression data file:")
  hbox$packStart(label,FALSE,FALSE,0)
  # Add entry in the second column; named "filename"
  filename <- gtkEntryNew()
  #   filename$setWidthChars(50)
  filename$setWidthChars(32)
  label$setMnemonicWidget(filename)
  hbox$packStart(filename,FALSE,FALSE,0)
  buttonOpen <- gtkButtonNewFromStock("gtk-open")
  gSignalConnect(buttonOpen, "clicked", openFile)
  hbox$packStart(buttonOpen,FALSE,FALSE,0)


  # Add label in first column
  label <- gtkLabelNewWithMnemonic("_Column:")
  hbox$packStart(label,FALSE,FALSE,0)
  # Add entry in the second column; named "filename"
  column <- gtkEntryNew() ##check the entry to numbers
  column$setWidthChars(6)
  column$setMaxLength(3) 
  
  gSignalConnect(column, "changed", 
                 function(entry) {
                   text <- entry$getText()
                   if(nzchar(gsub("[0-9]", "", text))) {
                     entry$setIconFromStock("primary", "gtk-no")
                     
                   } else {
                     entry$setIconFromStock("primary", "gtk-yes")
                     entry$setIconTooltipText("primary", NULL)
                   }
                 })
  column$setIconFromStock("primary", "gtk-yes")
  column$setIconTooltipText("primary", "Only numbers are allowed" )
  label$setMnemonicWidget(column)
  hbox$packStart(column,FALSE,FALSE,0)
  
  # Add horizontal container for every widget line
  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)
  
  ## combo box gene identifier 
  label <- gtkLabelNewWithMnemonic("_Select identifier:")
  hbox$packStart(label,FALSE,FALSE,0)
  identifier <- gtkComboBoxNewText()
  identifiers <- c("AFFYMETRIX_3PRIME_IVT_ID", "AFFYMETRIX_EXON_GENE_ID", "AFFYMETRIX_SNP_ID", "AGILENT_CHIP_ID", "AGILENT_ID", "AGILENT_OLIGO_ID", "ENSEMBL_GENE_ID", 
                 "ENSEMBL_TRANSCRIPT_ID", "ENTREZ_GENE_ID", "FLYBASE_GENE_ID", "FLYBASE_TRANSCRIPT_ID" , "GENCODE", "GENE_SYMBOL", 
                 "GENBANK_ACCESSION", "GENOMIC_GI_ACCESSION", "GENPEPT_ACCESSION", 
                 "ILLUMINA_ID", "IPI_ID", "MGI_ID", "PFAM_ID", "PIR_ID", "PROTEIN_GI_ACCESSION", "REFSEQ_GENOMIC", "REFSEQ_MRNA", "REFSEQ_PROTEIN", "REFSEQ_RNA", 
                 "RGD_ID", "SGD_ID", "TAIR_ID", "UCSC_GENE_ID", "UNIGENE", "UNIPROT_ACCESSION", "UNIPROT_ID", "UNIREF100_ID", "WORMBASE_GENE_ID", "WORMPEP_ID", 
                 "ZFIN_ID")
  sapply(identifiers , identifier$appendText)
  label$setMnemonicWidget(identifier)
  identifier$setActive(6) ## 6 is ENSEMBL_GENE_ID
  hbox$packStart(identifier,FALSE,FALSE,0)
  
  gSignalConnect(identifier, "changed", 
                function(identif, user.data) {
                  if (identif$getActive()<0) {
                    message("no value selected")
                  } else {
                    if (identif$getActiveText()=="GENE_SYMBOL") {
                      gtkWidgetShow(label.specie)
                      gtkWidgetShow(specie)
                    } else {
                      gtkWidgetHide(label.specie)
                      gtkWidgetHide(specie)
                    }
                  }
                })

  label.specie <- gtkLabelNewWithMnemonic("_Select Specie:")
  hbox$packStart(label.specie,FALSE,FALSE,0)
  specie <- gtkComboBoxNewText()
  
  species <- c("DROSOPHILA", "HUMAN", "MOUSE", "ZEBRAFISH")
  
  sapply(species, specie$appendText)
  label.specie$setMnemonicWidget(specie)
  hbox$packStart(specie, FALSE, FALSE, 0)

  gtkWidgetHide(label.specie)
  gtkWidgetHide(specie)
  
  ##########################################################################
  ## Add horizontal container for every widget line
  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)
  ## combo box adjustment identifier
  label <- gtkLabelNewWithMnemonic("_Filter Results on:")
  hbox$packStart(label,FALSE,FALSE,0)
  adjustment <- gtkComboBoxNewText()
  adjustments <- c("No", "Bonferroni", "Benjamini", "FDR")
  sapply(adjustments , adjustment$appendText)
  label$setMnemonicWidget(adjustment)
  adjustment$setActive(0)
  hbox$packStart(adjustment,FALSE,FALSE,0)
  
  gSignalConnect(adjustment, "changed", 
                 function(adju, user.data) {
                   if (adju$getActive()<0) {
                     message("no value selected")
                   } else {
                     if (adju$getActiveText()!="No") {
                       gtkWidgetShow(label.threshold)
                       gtkWidgetShow(threshold)
                     } else {
                       gtkWidgetHide(label.threshold)
                       gtkWidgetHide(threshold)
                     }
                   }
                 })
  

  ## combo box threshold identifier
  label.threshold <- gtkLabelNewWithMnemonic("_ With threshold:")
  hbox$packStart(label.threshold,FALSE,FALSE,0)
  threshold <- gtkComboBoxNewText()
  thresholds <- c("None", "0.01", "0.05")
  sapply(thresholds , threshold$appendText)
  label$setMnemonicWidget(threshold)
  threshold$setActive(0)
  hbox$packStart(threshold,FALSE,FALSE,0)
  
  gtkWidgetHide(label.threshold)
  gtkWidgetHide(threshold)
  
  ##########################################################################
  
  # Add horizontal container for every widget line
  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)
  
  ## building vbox to contain radiobutton group
  vvbox <- gtkVBox(FALSE, 5) ;
  label <- gtkLabelNewWithMnemonic("_Select list type:")
  labels <- c('Gene List', 'Background')
  list.type.gp <- list()
  # list for group
  list.type.gp[[labels[1]]] <- gtkRadioButton(label=labels[1])
  vvbox$packStart(label, FALSE, FALSE, 0)
  for(lab in labels[-1])
    list.type.gp[[lab]] <- gtkRadioButton(list.type.gp, label=lab)
  # label$setMnemonicWidget(list.type.gp)
  sapply(list.type.gp, gtkBoxPackStart, object=vvbox)
  hbox$packStart(vvbox, TRUE, TRUE, 100)

  ## building vbox to contain radiobutton group
  vvbox <- gtkVBox(FALSE, 5) ;
  label <- gtkLabelNewWithMnemonic("_Select Analysis type:")
  labels <- c('Pathway Analysis', 'GO Analysis')
  analysis.type.gp <- list()
  vvbox$packStart(label, FALSE, FALSE, 0)
  # list for group
  analysis.type.gp[[labels[1]]] <- gtkRadioButton(label=labels[1])
  for(lab in labels[-1])
    analysis.type.gp[[lab]] <- gtkRadioButton(analysis.type.gp, label=lab)
  for(item in analysis.type.gp )
    vvbox$packStart(item, FALSE, FALSE, 0)
  # label$setMnemonicWidget(analysis.type.gp)
  hbox$packStart(vvbox, TRUE, TRUE, 100)
  
  ## input list name
  hbox <- gtkHBoxNew(FALSE, 8)
  vbox$packStart(hbox, FALSE, FALSE, 0)
  # Add label in first column
  label <- gtkLabelNewWithMnemonic("_List name:")
  hbox$packStart(label,FALSE,FALSE,0)
  # Add entry in the second column; named "filename"
  listname <- gtkEntryNew()
  listname$setWidthChars(25)
  label$setMnemonicWidget(listname)
  hbox$packStart(listname,FALSE,FALSE,0)
  

  label <- gtkLabelNewWithMnemonic("_Registered account mail (opt.):")
  hbox$packStart(label,FALSE,FALSE,0)
  # Add entry in the second column; named "filename"
  mail <- gtkEntryNew()
  mail$setWidthChars(25)
  label$setMnemonicWidget(mail)

##^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,6}$
#   gSignalConnect(mail, "changed", 
#                           function(entry) {
#                             text <- entry$getText()
#                             if(nzchar(gsub("^[A-Z0-9._%+-]+@[A-Z0-9.-]+.[A-Z]{2,6}$", "", text))) {
#                               entry$setIconFromStock("primary", "gtk-no")
#                               mail$setIconTooltipText("primary", "Please enter a valid E-Mail address")
#                             } else {
#                               entry$setIconFromStock("primary", "gtk-yes")
#                               mail$setIconTooltipText("primary", "Please, if you encounter problems, enter an E-Mail used to register a DAVID account")
#                             }
#                           })
  mail$setIconTooltipText("primary", "Please, if you encounter problems, enter a registered E-Mail on DAVID website")
  mail$setIconFromStock("primary", "gtk-about")
  label$setMnemonicWidget(mail)
  hbox$packStart(mail,FALSE,FALSE,0)
  
  # Add button
  the.buttons <- gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)
  
  # button
  info <- gtkButtonNewWithMnemonic("How to use this Interface", show = TRUE)
  gSignalConnect(info, "clicked", infoFunction)
  the.buttons$packStart(info,fill=F)
  
  # button
  davidbut <- gtkButtonNewWithMnemonic("_DAVID", show = TRUE)
#     gSignalConnect(davidbut, "clicked", DavidQuery)
  gSignalConnect(davidbut, "clicked", CheckInputData)
  the.buttons$packStart(davidbut, fill=F)

  ## end of david main function and gui components ##############################
}

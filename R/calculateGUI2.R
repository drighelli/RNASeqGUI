calculateGUI2 <-
function(project_name) {

 Project <- project_name
 message=paste("You are using the project: ",Project,sep="")
 print(message)

 GUI_EdgeR  <- function(button, user.data) {
		res <- NULL
  res = calculate_EdgeR(project_name)                 		
  rm(list = ls())
	} #End of function

 GUI_DESeq <- function(button, user.data) {
		res <- NULL
  res = calculate_DESeq(project_name)                 		
  rm(list = ls())
	} #End of function

 GUI_DESeq2 <- function(button, user.data) {
		res <- NULL
  res = calculate_DESeq2(project_name)                 		
  rm(list = ls())
	} #End of function

 GUI_EdgeRComplexDesign  <- function(button, user.data) {
		res <- NULL
  res = calculate_EdgeRComplexDesign(project_name)                 		
  rm(list = ls())
	} #End of function

 GUI_DESeqComplexDesign <- function(button, user.data) {
		res <- NULL
  res = calculate_DESeqComplexDesign(project_name)                 		
  rm(list = ls())
	} #End of function

 GUI_DESeq2ComplexDesign <- function(button, user.data) {
		res <- NULL
  res = calculate_DESeq2ComplexDesign(project_name)                 		
  rm(list = ls())
	} #End of function

 GUI_NOISeq <- function(button, user.data) {
		res <- NULL
  res = calculate_NoiSeq(project_name)                 		
  rm(list = ls())
	} #End of function

 GUI_BaySeq <- function(button, user.data) {
		res <- NULL
  res = calculate_BaySeq(project_name)                 		
  rm(list = ls())
	} #End of function

#########################

# Create window
	window <- gtkWindow()
	# Add title
	window["title"] <- "RNASeqGUI"

	# Add a frame
 title=paste(" \n Please, choose one of the methods below to identify DE genes. \n You are working on ",Project, " project.  ", sep="")
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

# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

 # button
	GUI_EdgeRbutton <- gtkButtonNewWithMnemonic("_EdgeR Exact Test", show = TRUE)
	gSignalConnect(GUI_EdgeRbutton, "clicked", GUI_EdgeR)
	the.buttons$packStart(GUI_EdgeRbutton,fill=FALSE)

 # button
	GUI_EdgeRComplexDesignbutton <- gtkButtonNewWithMnemonic("_EdgeR GLM for Multi-Factors", show = TRUE)
	gSignalConnect(GUI_EdgeRComplexDesignbutton, "clicked", GUI_EdgeRComplexDesign)
	the.buttons$packStart(GUI_EdgeRComplexDesignbutton,fill=FALSE)

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
	GUI_DESeqbutton <- gtkButtonNewWithMnemonic("_DESeq", show = TRUE)
	gSignalConnect(GUI_DESeqbutton, "clicked", GUI_DESeq)
	the.buttons$packStart(GUI_DESeqbutton,fill=FALSE)

 # button
	GUI_DESeqComplexDesignbutton <- gtkButtonNewWithMnemonic("_DESeq for Complex Design", show = TRUE)
	gSignalConnect(GUI_DESeqComplexDesignbutton, "clicked", GUI_DESeqComplexDesign)
	the.buttons$packStart(GUI_DESeqComplexDesignbutton,fill=FALSE)

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
	GUI_DESeq2button <- gtkButtonNewWithMnemonic("_DESeq2", show = TRUE)
	gSignalConnect(GUI_DESeq2button, "clicked", GUI_DESeq2)
	the.buttons$packStart(GUI_DESeq2button,fill=FALSE)

 # button
	GUI_DESeq2ComplexDesignbutton <- gtkButtonNewWithMnemonic("_DESeq2 for Complex Design", show = TRUE)
	gSignalConnect(GUI_DESeq2ComplexDesignbutton, "clicked", GUI_DESeq2ComplexDesign)
	the.buttons$packStart(GUI_DESeq2ComplexDesignbutton,fill=FALSE)

# Add horizontal container for every widget line
	hbox <- gtkHBoxNew(FALSE, 8)
	vbox$packStart(hbox, FALSE, FALSE, 0)

# Add button
	the.buttons <- gtkHButtonBoxNew()
	the.buttons$setBorderWidth(5)
 vbox$add(the.buttons)
 the.buttons$setLayout("spread")
 the.buttons$setSpacing(40)

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
	GUI_NOISeqbutton <- gtkButtonNewWithMnemonic("_NoiSeq", show = TRUE)
	gSignalConnect(GUI_NOISeqbutton, "clicked", GUI_NOISeq)
	the.buttons$packStart(GUI_NOISeqbutton,fill=FALSE)

 # button
 GUI_BaySeqbutton <- gtkButtonNewWithMnemonic("_BaySeq", show = TRUE)
	gSignalConnect(GUI_BaySeqbutton, "clicked", GUI_BaySeq)
	the.buttons$packStart(GUI_BaySeqbutton,fill=FALSE)
}

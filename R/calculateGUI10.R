calculateGUI10 <- function(project_name) {

 Project <- project_name
 message=paste("You are using the project: ",Project,sep="")
 print(message)

#####  SHOW REPORT IN HTML   ######
reportFun <- function(button, user.data) {
      res <- NULL
      print("You are using the new project: ")
      print(project_name)
      a=paste(getwd(),"/RNASeqGUI_Projects/",project_name,"/Logs/report.Rmd",sep="")
      main_location=getwd()
      setwd(paste(getwd(),"/RNASeqGUI_Projects/",project_name,"/Logs",sep=""))
      knit(a)
      b=paste(getwd(),"/report.md",sep="")
      knitr::knit2html(b)
      c=paste(getwd(),"/report.html",sep="")
      browseURL(c)               		
      res
      setwd(main_location)
      rm(list = ls())
	} #End of function

#####  SHOW REPORT IN PDF   ######
reportFunPDF <- function(button, user.data) {
      res <- NULL   
      print("You are using the new project: ")
      print(project_name)
      a=paste(getwd(),"/RNASeqGUI_Projects/",project_name,"/Logs/report.Rmd",sep="")
      main_location=getwd()
      setwd(paste(getwd(),"/RNASeqGUI_Projects/",project_name,"/Logs",sep=""))
      knit(a)
      b=paste(getwd(),"/report.md",sep="")
      pandoc('report.md', format='latex') # LaTeX/PDF
      c=paste(getwd(),"/report.pdf",sep="")
      browseURL(c)               		
      res
      setwd(main_location)
      rm(list = ls())
	} #End of function

#####  SHOW REPORT IN DOCX   ######
reportFunDOCX <- function(button, user.data) {
      res <- NULL
      print("You are using the new project: ")
      print(project_name)
      a=paste(getwd(),"/RNASeqGUI_Projects/",project_name,"/Logs/report.Rmd",sep="")
      main_location=getwd()
      setwd(paste(getwd(),"/RNASeqGUI_Projects/",project_name,"/Logs",sep=""))
      knit(a)
      b=paste(getwd(),"/report.md",sep="")
      pandoc('report.md', format='docx')  # MS Word
      c=paste(getwd(),"/report.docx",sep="")
      browseURL(c)               		
      res
      setwd(main_location)
      rm(list = ls())
	} #End of function

#####  SHOW REPORT IN ODT   ######
reportFunODT <- function(button, user.data) {
      res <- NULL    
      print("You are using the new project: ")
      print(project_name)
      a=paste(getwd(),"/RNASeqGUI_Projects/",project_name,"/Logs/report.Rmd",sep="")
      main_location=getwd()
      setwd(paste(getwd(),"/RNASeqGUI_Projects/",project_name,"/Logs",sep=""))
      knit(a)
      b=paste(getwd(),"/report.md",sep="")
      pandoc('report.md', format='odt')  #Open office
      c=paste(getwd(),"/report.odt",sep="")
      browseURL(c)               		     
      res
      setwd(main_location)
      rm(list = ls())
	} #End of function


#########################

 # Create window
	window <- gtkWindow()
	# Add title
	window["title"] <- "RNASeqGUI"

	# Add a frame
 title=paste(" \n Print your log file via a WEB Browser. \n You are working on '",Project, "' project.  ", sep="")
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
	Reportbutton <- gtkButtonNewWithMnemonic("_report", show = TRUE)
	gSignalConnect(Reportbutton, "clicked", reportFun)
	the.buttons$packStart(Reportbutton,fill=FALSE)

 ## button
	#ReportbuttonPDF <- gtkButtonNewWithMnemonic("_pdf", show = TRUE)
	#gSignalConnect(ReportbuttonPDF, "clicked", reportFunPDF)
	#the.buttons$packStart(ReportbuttonPDF,fill=FALSE)

 ## Add horizontal container for every widget line
	#hbox <- gtkHBoxNew(FALSE, 8)
	#vbox$packStart(hbox, FALSE, FALSE, 0)

 ## Add button
	#the.buttons <- gtkHButtonBoxNew()
	#the.buttons$setBorderWidth(5)
 #vbox$add(the.buttons)
 #the.buttons$setLayout("spread")
 #the.buttons$setSpacing(40)

 ## button
	#ReportbuttonDOCX <- gtkButtonNewWithMnemonic("_docx", show = TRUE)
	#gSignalConnect(ReportbuttonDOCX, "clicked", reportFunDOCX)
	#the.buttons$packStart(ReportbuttonDOCX,fill=FALSE)

 ## button
	#ReportbuttonODT <- gtkButtonNewWithMnemonic("_odt", show = TRUE)
	#gSignalConnect(ReportbuttonODT, "clicked", reportFunODT)
	#the.buttons$packStart(ReportbuttonODT,fill=FALSE)

}



WaitingWindow <- function(message, parent.w=NULL, title='Please Wait', waiting.image=system.file("imgs", "researcher.png", package="RNASeqGUI")) {
  require(RGtk2)
  ## invoke this function to istantiate a new Waiting Window
  ## @return: a gtkWindow
  ## to destroy it invoke returnedWindowName$destroy()
  ## waiting window GUI ###############################################
  window <- gtkWindow(show=FALSE)
  # Add title
  window["title"] <- title
  window$SetResizable(FALSE)
  gtkWindowSetTransientFor(window, parent.w)
  gtkWindowSetModal(window, TRUE) ## comment to have multiple windows
  #     window$SetSensitive(FALSE)
  # Create vertical container for file name entry
  vbox <- gtkVBoxNew(FALSE, 20)
  vbox$setBorderWidth(24)
  
  window$add(vbox)
  
  label <- gtkLabelNewWithMnemonic(message)
  vbox$packStart(label)
  
  pixbuf.anim <- gdkPixbufAnimationNewFromFile(waiting.image)
  image=gtkImageNew()
  gtkImageSetFromAnimation(image, pixbuf.anim$retval)

  gtkWidgetShow(image)
  vbox$packStart(image)

  gtkWidgetShow(window)
  return(window)
}

# WaitingWindow(message='prova')

# RNASeqGUI

RNASeqGUI is an R Graphical User Interface for Bulk RNA-seq data analysis in a reproducible research way.

# Requisites Installation

It needs GTK2 libraries to run. 
Below, it is described how to install GTK2 libraries on Linux, Mac OS and Windows.

## For Linux users

We tested RNASeqGUI on Ubuntu 12.04 (precise) 64-bit, Kernel Linux 3.2.0-37-generic, GNOME 3.4.2.

1 - Open a terminal and type: 

  sudo apt-get update

  sudo apt-get install libgtk2.0-dev

2 - Type:

  sudo apt-get install libcurl4-gnutls-dev

3 - Type:

  sudo apt-get install libxml2-dev


## For Mac OS users

1 - Install Xcode developer tools (at least version 5.0.1) from Apple Store (it is free).
2 - Install XQuartz-2.7.5.dmg from http://xquartz.macosforge.org/landing/
3 - Install GTK_2.24.17_X11.pkg from http://r.research.att.com .


## For Windows users

1 - download gtk+-bundle_2.22.1-20101229_win64.zip from http://ftp.gnome.org/pub/gnome/binaries/win64/gtk+/2.22/ .
2 - This is a bundle containing the GTK+ stack and its dependencies for Windows. To use it, create some empty folder like C : \opt\gtk .
3 - Unzip this bundle.
4 - Now, you have to add the bin folder to your PATH variable. Make sure you have no
other versions of GTK+ in PATH variable. To do this, execute the following instructions:
Open Control Panel, click on System and Security, click on System, click
on Advanced System Settings, click on Environment Variables. In the
Environment Variables window you will notice two columns User variables
for a user name and System variables. Change the PATH variable in the System
variables to be C : \opt\gtk\bin .

This is all you need to install the GTK2 libraries.


# RNASeqGUI installation

1 - Install R version 3.2.2 (2015-08-14) – “Fire Safety” from http://cran.r-project.org/  according to your operating system.

2 - Download RNASeqGUI package from http://bioinfo.na.iac.cnr.it/RNASeqGUI/Download.
ForWindows operating system, download the zip binary file. For MacOS and
Linux download the tar.gz file.

• For Windows users: select “Install packages(s) from local zip files”,
under the “Packages” pull-down menu.

• For MacOS users: under “Package and Data” pull-down menu, select “Package Installer”.
In the “Package Installer”, pull down the top-left menu, select “Local Source Package” and navigate to where you have downloaded the
source package.
Then go to http://cran.r-project.org/web/packages/RGtk2/index.html and choose the binary version for OS X Snow Leopard binaries: r-release:
RGtk2 2.20.29.tgz. Then, in the “Package Installer”, pull down the top-left menu and select “Local Binary Package”.

• For Linux users: open a shell and go to the directory containing the
package tree and type the command

  sudo R CMD INSTALL -l /path/to/library RNASeqGUI

3 - Finally, if the libraries required by RNASeqGUI are not automatically downloaded and installed.
Inside the RNASeqGUI package there is a file called InstallPackages.R. This file can be loaded from an R shell to install all packages needed automatically.
Please open a R shell and type:
  
  source("InstallPackages.R")


# Quick start

If you have successfully gone through the installation you are ready to use RNASeqGUI, as follows.
1 - Open R.
2 - Type

  library(RNASeqGUI)

in the R environment. Wait for the package to be loaded.
3 - Finally, type

  RNASeqGUI()





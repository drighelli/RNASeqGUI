filteringfunction <- function(x, the.file, conditions, type1, type2, type3, cv.cutoff, norm, cpm, Project){
    
    if(Sys.info()[[1]]=="Windows"){ ##only separator changes
      separator='\\'  
    }else{         # Linux
      separator='/'
    }
 
  res=NULL

  require(NOISeq)

    if(Sys.info()[[1]]=="Windows"){ 
       the.file2 = strsplit(the.file,'\\\\') 
    }else{         # Linux
       the.file2 = strsplit(the.file,'/') 
    }

  the.file2 = the.file2[[1]][length(the.file2[[1]])]  #estract the namefile

  filtering.db <- InitDb(db.name=paste(the.file2,'filtering_db',sep=""), db.path=file.path("RNASeqGUI_Projects",Project,"Logs","cache"))
  
  SaveInCache(filtering.db, the.file, "the_file_key")
  SaveInCache(filtering.db, Project, "project_key")
  SaveInCache(filtering.db, x, "filtering_dataframe_key") 
  SaveInCache(filtering.db, conditions, "conditions_key")
  SaveInCache(filtering.db, type1, "type1_key")
  SaveInCache(filtering.db, type2, "type2_key")
  SaveInCache(filtering.db, type3, "type3_key")
  SaveInCache(filtering.db, norm, "norm_key")

  cpm = as.numeric(cpm)
  SaveInCache(filtering.db, cpm, "cpm_key")
  
  cv.cutoff = as.numeric(cv.cutoff )
  SaveInCache(filtering.db, cv.cutoff, "cvcutoff_key")

  print("You loaded this count file: ")
  print(dim(as.matrix(x)))
  print(head(as.matrix(x),10))
  message3 <- paste("This file contains ",dim(x)[1]," features.",sep="")
  print(message3)

  filtered_x=NULL
     
     depth = NULL
     for( i in 1:ncol(x) ){depth[i] = sum(x[,i])}
     if(type1=="TRUE"){    # CPM procedure
         print("Filtering process has been started with CPM procedure to filter low counts data")
         filtered_x = filtered.data(x, factor = conditions, norm = norm, depth = depth, method = 1, cpm = cpm, cv.cutoff=cv.cutoff)
         write.table(filtered_x, file =  paste(getwd(), "RNASeqGUI_Projects", Project, "Results", paste0("CPM_", the.file2, sep=""), sep=separator) , quote=FALSE, sep="\t", row.names=TRUE)
         print("First lines of the results")
         print(dim(as.matrix(filtered_x)))
         print(head(filtered_x,10))
     }
     if(type2=="TRUE"){  # Wilcoxon test
         print("   ")
         print("   << WARNING >>: YOU MUST HAVE AT LEAST 5 REPLICATES PER CONDITION IN ORDER TO USE THE WILCOXON TEST CORRECTLY!!!   ")
         print("   ")
         print("Filtering process has been started anyway with Wilcoxon test procedure to filter low counts data")  
         filtered_x = filtered.data(x, factor = conditions, norm = norm, depth = depth, method = 2)
         write.table(filtered_x, file =  paste(getwd(), "RNASeqGUI_Projects", Project, "Results", paste0("Wilcoxon_", the.file2, sep=""), sep=separator), quote=FALSE, sep="\t", row.names=TRUE)
         print("First lines of the results")
         print(dim(as.matrix(filtered_x)))
         print(head(filtered_x,10))
     }
     if(type3=="TRUE"){    # Proportion test
         print("Filtering process has been started with Proportion test procedure to filter low counts data")
         filtered_x = filtered.data(x, factor = conditions, norm = norm, depth = depth, method = 3, cpm = cpm)
         write.table(filtered_x, file =  paste(getwd(), "RNASeqGUI_Projects", Project, "Results", paste0("Proportion_", the.file2, sep=""), sep=separator), quote=FALSE, sep="\t", row.names=TRUE)
         print("First lines of the results")
         print(dim(as.matrix(filtered_x)))
         print(head(filtered_x,10))
     }

    print("Filtering process finished! Results has been saved in the Results folder.")  
    SaveInCache(filtering.db, filtered_x, "filteredx_key")
    res = filtered_x
  message3 <- paste("This filtered file contains ",dim(res)[1]," features.",sep="")
  print(message3)


#PrintFiltering <- function (x,the.file,conditions,type1,type2,type3,cv.cutoff,norm,cpm,Project) {

cat('processing filtering report...\n')
  if(Sys.info()[[1]]=="Windows"){ ##only separator changes
    sys.separator='\\'  
  }else{         # Linux
    sys.separator='/'
  }
  report=paste("RNASeqGUI_Projects", Project, "Logs", "report.Rmd",sep=sys.separator)

  message5 <- paste("  ",sep="\n")
  write(message5, file = report, ncolumns = if(is.character(message5)) 1 else 5, append = TRUE, sep = "\n")
  
  message2 <- paste(" * In the *Filtering Interface*, you clicked the **Filter** button at `", Sys.time(),"` and the result has been saved in the `", Project,"/Results` folder.", sep="")
  write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message2 <- paste("You chose the following file: `",the.file,"` ",sep="\n")
  write(message2, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

     message1 <- paste("conditions:= c(",sep="")
     write(message1, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message2 <- paste("'",conditions[1:(length(conditions)-1)],"',",sep="")
     write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message3 <- paste("'",conditions[length(conditions)],"'",sep="")
     write(message3, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

     message4 <- paste("), ")
     write(message4, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "")

  message2 <- paste("` type1: `",type1,"` type2: `",type2,"`type3: `",type3,"` cv.cutoff: `",cv.cutoff,"` norm: `",norm,"` cpm: `",cpm,"` , Project: `",Project,"`",sep="\n")
  write(message2, file = report,ncolumns = if(is.character(message2)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
 
  message5 <- paste(" This R code has been run:",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("  ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" ```{r} ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

    message5 <- paste("the.file2='",the.file2,"'",sep="")
    write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("filtering.db <- InitDb(db.name=paste(the.file2,'filtering_db',sep=''), db.path='cache')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste(" x <- LoadCachedObject(filtering.db, 'filtering_dataframe_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("head(x) ", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("dim(x)[1]", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("the.file <- LoadCachedObject(filtering.db, 'the_file_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("Project <- LoadCachedObject(filtering.db, 'project_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("conditions <- LoadCachedObject(filtering.db, 'conditions_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("type1 <- LoadCachedObject(filtering.db, 'type1_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("type2 <- LoadCachedObject(filtering.db, 'type2_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("type3 <- LoadCachedObject(filtering.db, 'type3_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("cpm <- LoadCachedObject(filtering.db, 'cpm_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  message5 <- paste("cv.cutoff <- LoadCachedObject(filtering.db, 'cvcutoff_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("setwd('",getwd(),"')", sep=sys.separator)
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message2 <- paste("#filteringfunction(x=x,the.file=the.file,conditions=conditions,type1=type1,type2=type2,type3=type3,cv.cutoff=cv.cutoff,norm=norm,cpm=cpm,Project=Project)",sep='')
  write(message2, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("filtered_x <- LoadCachedObject(filtering.db, 'filteredx_key')", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste("head(filtered_x) ", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")

  message5 <- paste("dim(filtered_x)[1]", sep="")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  
  message8 <- paste(" ``` ",sep="\n")
  write(message8, file = report,ncolumns = if(is.character(message8)) 1 else 5,append = TRUE, sep = "\n")
  
  message5 <- paste(" _______________________________________________________________________ ",sep="\n")
  write(message5, file = report,ncolumns = if(is.character(message5)) 1 else 5,append = TRUE, sep = "\n")
  cat('done\n')

res 

}



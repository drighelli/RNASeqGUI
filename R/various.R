ExtractFileNameFromPath <- function(file.name){
  just.filename <- substring(file.name, max(gregexpr("/",file.name)[[1]])+1)
  return(just.filename)
}
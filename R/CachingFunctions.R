require(filehash)

InitDb <- function(db.name, db.path='') {
  ## this function creates a new database or select an existing one
  ## @name: the name of the database file
  ## @path: if not null, creates 
  ## returns: the database object
  if(db.path=='') {
    db.path.name <- db.name
  } else {
    db.path.name <- file.path(db.path, db.name)
    if(!file.exists(db.path)) {
      dir.create(db.path, recursive=T)
    }
  }
  if(!file.exists(db.path.name)) {
    dbCreate(db.path.name)
  }
  db <- dbInit(db.path.name)
  return(db)
}
# 
# CreateDb <- function(db.name, db.path='') {
#   ## this function creates a new database
#   ## @name: the name of the database file
#   ## @path: if not null, creates 
#   ## returns: the database object
#   if(db.path=='') {
#     db.path.name <- db.name
#   } else {
#     db.path.name <- file.path(db.path, db.name)
#     if(!file.exists(db.path)) {
#       dir.create(db.path, recursive=T)
#     }
#   }
#   dbCreate(db.path.name)
#   db <- dbInit(db.path.name)
#   return(db)
# }


SaveInCache <- function(db, object, key) {
  ## this function save an object in the database
  ## @db: the database object within to save the object
  ## @object: the object to save
  ## @key: the key value to associate to the object
  ## returns: none
  dbInsert(db, key, object)
}

LoadCachedObject <- function(db, key) {
  ## this function retrieve an object from a database
  ## @db: the database object within the object is saved
  ## @key: the key value associated to the object
  ## returns: the object retrieved from the database
  object <- dbFetch(db, key)
  return(object)
}


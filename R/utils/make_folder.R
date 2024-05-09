# Make a folder
# Written by David Schoeman
# Last modified: October 2023    

make_folder <- function(folder) {
  if(!isTRUE(file.info(folder)$isdir)) dir.create(folder, recursive=TRUE)
  return(folder)
}
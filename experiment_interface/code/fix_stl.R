## ---------------------------
##
## Script name: fix_stl.R
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2023-02-28
##
## ---------------------------
##
## Notes: Converts ASCII STL files into binary STL files that can be 
##        read with rgl::readSTL
##   
##
## ---------------------------

#Python to read/write binary stl files
library(reticulate)
source_python('code/convert_stl.py')

#Use function with R function
fix_stl <- function(file){
  convert_stl(file, file)
}

#List of all stl files in experiment_interface
files <- list.files(pattern = '.stl$', recursive = T)

#Go through files and convert to binary
for(i in 1:length(files)){
  fix_stl(files[i])
}

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

library(reticulate)
source_python('code/convert_stl.py')

fix_stl <- function(file){
  tf <- tempfile(fileext = '.stl')
  convert_stl(file, tf)
  return(tf)
  unlink(tf)
}
file = 'stl_files/id-03-Type1-Rep01.stl'
fix_stl(file)

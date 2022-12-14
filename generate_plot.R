## ---------------------------
##
## Script name: generate_plot.R
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2022-09-22
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

generate_plot = function(data, type = c(2,3), ...){
  
  args = list(...)
  
  source('Bar2D.R')
  source('Bar3D.R')
  
  if(type == 2)(p = Bar2D(data))
  if(type == 3)(p = Bar3D(data, output_style = args[1]))
  return(p)
}

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
source("Bar2D.R")
# source('Bar3D.R')

<<<<<<< HEAD:generate_plot.R
generate_plot = function(data, type = c(2,3), ...){
  
  args = list(...)
  
  source('Bar2D.R')
  source('Bar3D.R')
  
  if(type == 2)(p = Bar2D(data))
  if(type == 3)(p = Bar3D(data, output_style = args[1]))
=======
generate_plot <- function(data, type = c(2, 3), ...) {
  if (type == 2) (p <- Bar2D(data))
  if (type == 3) (p <- "NEED TO MAKE A 3D PLOT HERE")
>>>>>>> e735b8486a8cc065dc2a015a6d2a3cae58065afd:code/generate_plot.R
  return(p)
}

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

generate_plot <- function(data, type = c(2, 3), ...) {
  if (type == 2) (p <- Bar2D(data))
  if (type == 3) (p <- "NEED TO MAKE A 3D PLOT HERE")
  return(p)
}

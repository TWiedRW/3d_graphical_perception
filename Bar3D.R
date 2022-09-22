## ---------------------------
##
## Script name: Bar3D.R
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

Bar3D = function(data){
  require(rayshader)
  source('Bar2D.R')
  
  
}

samp = read.csv('data/type1dataset39_ratio0.681_target46.42.csv')

p = Bar2D(samp)
plot_gg(p, width = 3, height = 3, multicore = T, scale = 200,
        shadow_intensity = 0)




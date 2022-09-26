## ---------------------------
##
## Script name: generate_plots.R
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2022-09-22
##
## ---------------------------
##
## Notes: Generate all plots at once
##   
##
## ---------------------------

generate_plots = function(type = c(2,3), .clean_slate = F){
  require(ggplot2)
  source('generate_plot.R')
  
  #TESTING PURPOSES ONLY. THIS DELETES THE DATA FOLDER FOR A CLEAN START
  if(.clean_slate == T)(unlink('plots', recursive = T))
  
  #Create data folder if folder does not exist in working directory
  if(!file.exists('plots'))(dir.create('plots'))
  
  #Getting files from data folder
  files = dir('data')
  n = length(files)
  
  #Progress bar
  pb = txtProgressBar(min = 0,
                      max = n,
                      style = 3,
                      width = 50,
                      char = '=')
  
  for(i in 1:n){
    
    dat = read.csv(paste0('data/', files[i]))
    p = generate_plot(data = dat, type = type)

    ggsave(paste0('plots/', files[i], '.jpeg'),
           width = 6,
           height = 6)
    #Update progress bar    
    setTxtProgressBar(pb, i)
  }
  close(pb)
}

generate_plots(type = 2, .clean_slate = T)

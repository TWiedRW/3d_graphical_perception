## ---------------------------
##
## Script name: generate_data.R
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2022-09-21
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

#Testing values
x = 1:10
cm_values = 10*10^((x-1)/12)

target_heights = c(20, 30, 40)
ratios = c(0.5, 0.7, 0.9)



generate_data = function(target_heights, ratios, type = c('Type1', 'Type3'), random_process = c('unif', 'beta'), .clean_slate = F){
 
  #TESTING PURPOSES ONLY. THIS DELETES THE DATA FOLDER FOR A CLEAN START
  if(.clean_slate == T)(unlink('data', recursive = T))
  
  #These error messages don't work and I can't figure it out :(
  
  #if(!is.numeric(target_heights))(stop('Requires numeric target heights'))
  #if(!is.numeric(ratios))(stop('Requires numeric ratios'))
  #if(!(type %in% c('Type1','Type3'))(stop('Need to specify type = "Type1" or type = "Type3"'))

  #Create data folder if folder does not exist in working directory
  if(!file.exists('data'))(dir.create('data'))
  
  #Combinations of ratios and target heights
  combinations = expand.grid(target_heights, ratios)
  n_reps = nrow(combinations)
  
  #Progress bar
  pb = txtProgressBar(min = 0,
                      max = n_reps,
                      style = 3,
                      width = 50,
                      char = '=')
  
  #Generating data
  for(i in 1:n_reps){
    target = combinations[i,1]
    ratio = combinations[i,2]
    newbar = target * ratio
    
    #Random bars
    if(random_process == 'beta')(rand_bars = 100 * rbeta(n=8, shape1 = 2, shape2 = 2))
    if(random_process == 'unif')(rand_bars = runif(n = 8, min = 0, max = 100))
    
    #Arrangement of Type 1
    if(type == 'Type1'){
      
      #Comparison in left or right set of bars
      set_comparison = sample(1:2, size = 1)
      
      #Order of bars within given set
      bar_arrangement = sample(c(target, newbar, rand_bars[1:3]))
      
      #Arranging bars based on set
      arrangement = numeric(10)
      if(set_comparison == 1)(arrangement = c(bar_arrangement, rand_bars[4:8]))
      if(set_comparison == 2)(arrangement = c(rand_bars[4:8], bar_arrangement))
      
      #Tracking bars of interest
      pos_target = match(target, arrangement)
      pos_newbar = match(newbar, arrangement)
      
    }
    
    #Arrangement of Type 3
    if(type == 'Type3'){
      
      #Target in left or right set
      set_order = sample(1:2, size = 1)
      
      #Order of bars within given set
      bar_arrangement1 = sample(c(target, rand_bars[1:4]))
      bar_arrangement2 = sample(c(newbar, rand_bars[5:8]))
      
      #Arranging bars based on set
      arrangement = numeric(10)
      if(set_order == 1)(arrangement = c(bar_arrangement1, bar_arrangement2))
      if(set_order == 2)(arrangement = c(bar_arrangement2, bar_arrangement1))
      
      #Tracking bars of interest
      pos_target = match(target, arrangement)
      pos_newbar = match(newbar, arrangement)
      
    }
    
    #Label positions
    identifier = rep(NA, 10)
    identifier[pos_target] <- 'Target'
    identifier[pos_newbar] <- 'Newbar'
    
    #Dataset
    dat = data.frame(Order = 1:10, 
                     Group = c(1:5, 1:5),
                     Height = arrangement, 
                     Identifier = identifier)
    
    #Saving data into files ("dataset#_type#_ratio#_target#.csv")
    filename = paste0(tolower(type),
                      'dataset', as.character(i), 
                      '_ratio', as.character(ratio),
                      '_target', as.character(target))
    write.csv(x = dat,
              file = paste0('data/', filename, '.csv'),
              row.names = F)
   
     #Update progress bar    
    setTxtProgressBar(pb, i)
  }
  close(pb)
}


#Running the functions
generate_data(target_heights = target_heights,
              ratios = ratios,
              type = 'Type1',
              random_process = 'beta',
              .clean_slate = T)
generate_data(target_heights = target_heights,
              ratios = ratios,
              type = 'Type3',
              random_process = 'beta',
              .clean_slate = F)

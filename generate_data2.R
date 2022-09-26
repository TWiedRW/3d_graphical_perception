## ---------------------------
##
## Script name: generate_data2.R
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2022-09-21
##
## ---------------------------
##
## Notes: Ratio as decimal
##   
##
## ---------------------------

#Testing values
x = 1:10
cm_values = round(10*10^((x-1)/12),2)


generate_data = function(value1, value2, type = c('Type1', 'Type3'), random_process = c('unif', 'beta'), .clean_slate = F){
  
  #TESTING PURPOSES ONLY. THIS DELETES THE DATA FOLDER FOR A CLEAN START
  if(.clean_slate == T)(unlink('data', recursive = T))
  
  #These error messages don't work and I can't figure it out :(
  
  #stopifnot(is.numeric(target_heights))
  
  #if(!is.numeric(target_heights))(stop('Requires numeric target heights'))
  #if(!is.numeric(ratios))(stop('Requires numeric ratios'))
  #if(!(type %in% c('Type1','Type3'))(stop('Need to specify type = "Type1" or type = "Type3"'))
  
  #Create data folder if folder does not exist in working directory
  if(!file.exists('data')) {
    dir.create('data')
  }
  
  #Number of repetitions
  n_reps = length(value1)
  
  #Progress bar
  pb = txtProgressBar(min = 0,
                      max = n_reps,
                      style = 3,
                      width = 50,
                      char = '=')
  
  #Generating data
  for(i in 1:n_reps){
    height1 = value1[i]
    height2 = value2[i]
    
    values = c(height1, height2)
    
    #Random bars
    if(random_process == 'beta')(rand_bars = 100 * rbeta(n = 8, shape1 = 2, shape2 = 2))
    if(random_process == 'unif')(rand_bars = runif(n = 8, min = 0, max = 100))
    
    #Arrangement of Type 1
    if(type == 'Type1'){
      
      #Order of bars
      arrangement = c(rand_bars[1], sample(values, 2), rand_bars[2:8])
      
      #Tracking bars of interest
      pos_height1 = match(height1, arrangement)
      pos_height2 = match(height2, arrangement)
      
    }
    
    #Arrangement of Type 3
    if(type == 'Type3'){
      
      #Value in left set
      set_order = sample(1:2, size = 2)
      
      #Order of bars within given set
      arrangement = c(rand_bars[1], values[set_order[1]], rand_bars[2:4],
                      rand_bars[5], values[set_order[2]], rand_bars[6:8])
      
      #Tracking bars of interest
      pos_height1 = match(height1, arrangement)
      pos_height2 = match(height2, arrangement)
      
    }
    
    #Label positions
    identifier = rep(NA, 10)
    identifier[pos_height1] <- 'Value1'
    identifier[pos_height2] <- 'Value2'
    
    #Dataset
    dat = data.frame(Order = 1:10, 
                     Group = rep(c('A', 'B'), each = 5),
                     GroupOrder = c(1:5, 1:5),
                     Height = arrangement, 
                     Identifier = identifier)
    
    #Saving data into files ("type#_dataset#_value1_value2.csv")
    filename = paste0(tolower(type),
                      '_dataset', as.character(i), 
                      '_', as.character(round(height1, 2)),
                      '_compared_to_', as.character(round(height2, 2)))
    write.csv(x = dat,
              file = paste0('data/', filename, '.csv'),
              row.names = F)
    
    #Update progress bar    
    setTxtProgressBar(pb, i)
  }
  close(pb)
}


#Data collection on all CM values
set.seed(99)

cm_all = expand.grid(cm_values, cm_values)
cm_all = cm_all[cm_all[,1] != cm_all[,2],]

generate_data(value1 = cm_all[,1],
              value2 = cm_all[,2],
              type = 'Type1',
              random_process = 'beta',
              .clean_slate = T)
generate_data(value1 = cm_all[,1],
              value2 = cm_all[,2],
              type = 'Type3',
              random_process = 'beta',
              .clean_slate = F)

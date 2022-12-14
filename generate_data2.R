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


generate_data = function(value1, value2, type = c('Type1', 'Type3'), random_process = c('unif', 'beta'), save = T, filename = NA){
  
  #Number of repetitions
  n_reps = length(value1)
  
  #Generating data
  for(i in 1:n_reps){
    height1 = value1[i]
    height2 = value2[i]
    
    values = c(height1, height2)
    
    #Random bars
    if(random_process == 'beta')(rand_bars = 100 * rbeta(n = 8, shape1 = 3, shape2 = 5))
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
    
    #Saving data into files
    if(save == T){
    write.csv(x = dat,
              file = paste0(filename, '.csv'),
              row.names = F)
    }
    else(return(dat))
  }
}

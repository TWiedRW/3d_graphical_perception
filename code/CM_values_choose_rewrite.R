## ---------------------------
##
## Script name: CM_values_choose_rewrite.R
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2022-10-04
##
## ---------------------------
##
## Notes: Figuring out combinations from CM study, we known 10 and 56
##   
##
## ---------------------------

N <- 20 # Number of groups to split everything into


#Testing values
x = 1:10
cm_values = 10*10^((x-1)/12)

#Ratios present in paper
ratios = sort(c(0.825, 0.825, 0.681, 0.681, 0.562, 
                0.464, 0.464, 0.383, 0.261, 0.178))

#Possible values to compare and corresponding ratios
cbn = combn(cm_values, 2)

find_ratios = function(x){
  round(x[1]/x[2], 3)
}

cbn_ratio = apply(cbn, MARGIN = 2, find_ratios)

#Only value combinations with specified ratio
cbn_limited = cbn[,cbn_ratio %in% ratios]
cbn_ratio = cbn_ratio[cbn_ratio %in% ratios]

#Only one result for 17.8%, storage and removal from data, guessing on another
known_value = cbn_limited[,7]
cbn_limited = cbn_limited[,c(1:6, 8:39)]

ratios = ratios[2:10]


# # Choosing 9 possible value combinations (column is index of possible combinations)
# possible_combinations = combn(1:ncol(cbn_limited), 9)
# 
# 
# # Partition the matrix of possible combinations into N separate files
# idx <- 1:ncol(possible_combinations)
# len <- ceiling(ncol(possible_combinations)/N)
# listidx <- by(idx, INDICES = ceiling(idx/len), identity)
# 
# dir.create("Possible_Combinations")
# # Write each matrix sub-part out into a separate file
# for (i in 1:N) {
#   write.csv(possible_combinations[,listidx[[i]]], file = paste0("Possible_Combinations/part_", i, "_of_", N, ".csv"))
# }
# 





#Function to check conditions
check_conditions = function(x){
  
  x = as.numeric(x)
  combinations = cbn_limited[,x]
  
  count_of_values = table(c(combinations, cm_values))
  ratio_of_values = sort(find_ratios(combinations))
  
  #Check conditions of the values
  value_conditions <- isTRUE(all.equal(as.numeric(count_of_values), c(2,3,3,3,3,3,3,3,3,2)))
  
  #Check conditions of the ratios
  ratio_conditions <- floor(mean(ratio_of_values == ratios))
  
  #Check both conditions true
  res <- ratio_conditions & value_conditions
  
  #Return TRUE or FALSE
  return(res)
}


#Create folder to store data
if (!file.exists('value_combinations')) (dir.create('value_combinations'))

for (i in 1:N) {
  possible_combinations <- read.csv(paste0("Possible_Combinations/part_", i, "_of_", N, ".csv"))
  
  orig_res = mclapply(possible_combinations, 2, check_conditions, mc.cores = 8)
  
  res = which(orig_res)
  
  #Saving index
  save.combination.index = possible_combinations[,res]
  write.table(save.combination.index, paste0('value_combinations/saved_index_', i, '_of_', N, '.txt'),
              row.names = FALSE)
  
  #Saving combinations
  for (j in 1:length(res)) {
    combinations = round(cbind(known_value, cbn_limited[,save.combination.index[,j]],2))
    write.table(combinations, file = paste0('value_combinations/CM_values_',i,'_', j, '.txt'),
                row.names = FALSE)
  }
}

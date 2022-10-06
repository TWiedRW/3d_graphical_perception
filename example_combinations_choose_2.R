## ---------------------------
##
## Script name: CM_values_choose_9.R
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



#Choosing 2 possible value combinations (column is index of possible combinations)
possible_combinations = combn(1:ncol(cbn_limited), 2)


#Function to check conditions
check_conditions = function(x){
  
  x = as.numeric(x)
  combinations = cbn_limited[,x]
  
  count_of_values = as.numeric(table(c(combinations, cm_values)))
  ratio_of_values = sort(apply(combinations, 2, find_ratios))
  
  #Check conditions of the values
  value_conditions <- isTRUE(all.equal(count_of_values, c(1,1,1,2,1,1,2,2,2,1)))
  
  #Check conditions of the ratios
  ratio_conditions <- floor(mean(ratio_of_values == c(0.562, 0.825)))
  
  #Check both conditions true
  res <- ratio_conditions & value_conditions
  
  #Return TRUE or FALSE
  return(res)
}



#Collecting column index where combination meets criteria
res = which(apply(possible_combinations, 2, check_conditions))

cbn_limited[,possible_combinations[,res]]

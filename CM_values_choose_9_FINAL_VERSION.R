## ---------------------------
##
## Script name: CM_values_choose_9_ratios_only.R
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

#Only one result for 17.8%, storage and removal from data
known_value = cbn_limited[,7]
cbn_limited = cbn_limited[,-7]
cbn_ratio = cbn_ratio[-7]

ratios = ratios[2:10]



#Choosing 9 possible value combinations (column is index of possible combinations)
possible_combinations = combn(1:ncol(cbn_limited), 9)





###############################################################################
#
# Testing ratios
#
# - check_conditions(): function to test ratios from pre-calculated ratio list
# - check_conditions_fn(): function to loop through all combinations for ratio
# - results: T/F vector with matching column indices
# - ratio_combinations: limited possible combination matrix with only correct 
#                       ratios


#Function to check ratios
check_conditions = function(x){
  
  x = as.numeric(x)
  combinations = cbn_limited[,x]
  
  #Get ratios from pre-calculated ratio list
  ratio_of_values = sort(cbn_ratio[x])
  
  #Check conditions of the ratios
  ratio_conditions <- all(ratio_of_values == ratios)
  
  #Check both conditions true
  res <- ratio_conditions #& value_conditions
  
  #Return TRUE or FALSE
  return(res)
}

#Function to loop through all combinations and update progress
check_conditions_fn = function(){
  pb = txtProgressBar(min = 0,
                      max = ncol(possible_combinations),
                      style = 3,
                      width = 50,
                      char = '=')
  results = logical(ncol(possible_combinations))
  for(i in 1:ncol(possible_combinations)){
    res = check_conditions(possible_combinations[,i])
    results[i] <- as.logical(res)
    setTxtProgressBar(pb, i)
  }
  return(results)
  close(pb)
  
}
results = check_conditions_fn()

#Combinations with correct ratios
ratio_combinations = possible_combinations[,results]





###############################################################################
#
# Testing values
#
# - check_conditions_values(): function to check that each value is used twice
# - check_conditions_values_fn(): function to loop through ratio combinations
#                                 for values 
# - results_values: T/F vector with matching column indices
# - final_results: matrix with indices for correct values and ratios



#Function to check conditions
check_conditions_values = function(x){
  
  x = as.numeric(x)
  combinations = cbn_limited[,x]
  
  count_of_values = as.numeric(table(combinations))
  
  #Check conditions of the values
  value_conditions <- length(count_of_values) == 10 && all(count_of_values == c(1,2,2,2,2,2,2,2,2,1))
  
  #Check both conditions true
  res <- value_conditions
  
  #Return TRUE or FALSE
  return(res)
}





#Collecting correct ratio values
check_conditions_values_fn = function(){
  pb = txtProgressBar(min = 0,
                      max = ncol(ratio_combinations),
                      style = 3,
                      width = 50,
                      char = '=')
  results = logical(ncol(ratio_combinations))
  for(i in 1:ncol(ratio_combinations)){
    res = check_conditions_values(ratio_combinations[,i])
    results[i] <- as.logical(res)
    setTxtProgressBar(pb, i)
  }
  return(results)
  close(pb)
  
}
results_values = check_conditions_values_fn()
final_results = ratio_combinations[,results_values]





###############################################################################
#
# Confirming results and saving data
#
# - list_combinations: list with each index as the matrix of comparison 
#                      combinations
# - confirm_ratios: visual inspection that all ratios match (compare to `ratios`)
# - confirm_counts: visual inspection that each value is used twice
# - final_df: data frame with `Value1` as lower value to compare, `Value2` as 
#             upper value to compare, and `Set` as the combination identifier
# Results saved to `CM_possible_values.csv`



#Storing value comparisons in list, including the known combination
list_combinations = list()
for(i in 1:ncol(final_results)){
  indices = final_results[,i]
  
  list_combinations[[i]] <- cbind(known_value, cbn_limited[,indices])
}


#Confirming ratios and counts
confirm_ratios = lapply(list_combinations, function(x)(sort(apply(x, 2, find_ratios))))
unique(confirm_ratios)

confirm_counts = lapply(list_combinations, table)
unique(confirm_counts)



#Saving results as a .csv in a long format
final_df = data.frame(row.names = F)
for(i in 1:length(list_combinations)){
  temp = as.data.frame(cbind(t(list_combinations[[i]]),i),
                       row.names = F)
  final_df = rbind(temp, final_df)
}

names(final_df) <- c('Value1', 'Value2', 'Set')

write.csv(final_df, file = 'CM_possible_values.csv')

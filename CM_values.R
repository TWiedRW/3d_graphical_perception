## ---------------------------
##
## Script name: CM_values.R
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2022-09-22
##
## ---------------------------
##
## Notes: Figuring out combinations from CM study
##   
##
## ---------------------------

#Testing values
x = 1:10
cm_values = 10*10^((x-1)/12)

#Plotting natural vs. log scale
par(mfrow = c(1,2))
plot(x, cm_values, type = 'o', 
     main = 'Natural Scale\nBar Heights')
plot(x, log(cm_values), type = 'o',
     main = 'Log Scale\nBar Heights')

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
#known_value = cbn_limited[,7]
#cbn_limited = cbn_limited[,-7]
#cbn_limited = round(cbn_limited, 1)

#Choosing 10 possible value combinations (column is index of possible combinations)
start_time = Sys.time()
possible_combinations = combn(1:ncol(cbn_limited), 10)
end_time = Sys.time()
timediff1 = end_time - start_time



#Function to check conditions
check_conditions = function(x){
  
  x = as.numeric(x)
  combinations = cbn_limited[,x]
  
  count_of_values = table(combinations)
  ratio_of_values = sort(find_ratios(combinations))
  
  value_conditions = FALSE
  
  if(length(count_of_values) == 10){
    if(mean(count_of_values == rep(2, 10)) == 1){
      value_conditions = TRUE
    }
  }
  
  ratio_conditions = FALSE
  if(mean(ratio_of_values == ratios) == 1){
    ratio_conditions = TRUE
  }
  
  
  res = FALSE
  if(value_conditions & ratio_conditions){
    res = TRUE
  }
  
  return(res)
}




#Loop through all columns of combinations for conditions and store
#results into col_index_true

col_index_true = c()
j = 1

start_time2 = Sys.time()
for(i in 1:ncol(possible_combinations)){
  res = check_conditions(possible_combinations[,i])
  if(res){
    col_index_true[j] = i
    j = j + 1
  }
}
end_time2 = Sys.time()
timediff2 = end_time2 - start_time2

col_index_true
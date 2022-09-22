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
## Notes: Figuring out combinations from 
##   
##
## ---------------------------

#Testing values
x = 1:10
cm_values = 10*10^((x-1)/12)

plot(x, log(cm_values))

ratios = c(0.825, 0.825, 0.681, 0.681, 0.562, 
           0.464, 0.464, 0.383, 0.261, 0.178)


test = expand.grid(cm_values, cm_values)

library(dplyr)

test %>% 
  filter(Var1 != Var2) %>% 
  mutate(Lower = ifelse(Var1 < Var2, Var1, Var2),
         Upper = ifelse(Var2 > Var1, Var2, Var1)) %>% 
  select(-Var1, -Var2) %>% 
  unique() %>% 
  mutate(Ratio = round(100 * Lower/Upper, 1)) %>% 
  arrange(Ratio) %>% 
  filter(Ratio %in% c(82.5, 68.1, 56.2,46.4,38.3,26.1,17.8))


## ---------------------------
##
## Script name: set85.R
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2023-01-26
##
## ---------------------------
##
## Notes: Gets data from Set 85
##   
##
## ---------------------------

library(tidyverse)

#Random assignment of kits
set.seed(4125)

#Setting up kit ratios and order
kit_ratios <- combn(1:7, 5)
kit_ratios <- kit_ratios[,sample(1:ncol(kit_ratios), ncol(kit_ratios))]

#Defining available graph units
#ratio3p: 3D printed graphs
#ratio3d: 3D digital graphs
#ratio3d: 2D digital graphs
ratio3p <- data.frame(cbind(ratio = rep(1:7, each = 16), type = rep(1:2, times = 7*8),
                 id = 1:112)) %>% 
  group_by(ratio) %>% 
  nest() %>% 
  mutate(ran_order = map(data, sample_n, size = 16))

ratio3d <- ratio3p %>% 
  mutate(ran_order = map(data, sample_n, size = 16))

ratio2d <- ratio3p %>% 
  mutate(ran_order = map(data, sample_n, size = 16))

#Storage for kits
kits <- vector(mode='list', length=21)

#All units
units <- 1:112

#Assigning 3d print units to kits
for(i in 1:length(kits)){
  
  ratioInKit <- kit_ratios[,i]
  
  ratio3p <- ratio3p %>% 
    mutate(kit_data = NA, rem_id = NA) %>% 
    mutate(kit_data = ifelse(ratio %in% ratioInKit, map(ran_order, function(x)(x[1,])), NA),
           rem_id = map(kit_data, function(x)(x[[1,2]]))) %>% 
    unnest(rem_id, keep_empty = T) %>% 
    mutate(ran_order = map(ran_order, function(x)(x[-1,])))

  kits[[i]][['3dPrint']] <- ratio3p %>% unnest(kit_data) %>% select(ratio, type)
  
}


#Assigning units to kits
for(i in 1:length(kits)){
  
  ratioInKit <- kit_ratios[,i]
  
  ratio3d <- ratio3d %>% 
    mutate(kit_data = NA, rem_id = NA) %>% 
    mutate(kit_data = ifelse(ratio %in% ratioInKit, map(ran_order, function(x)(x[1,])), NA),
           rem_id = map(kit_data, function(x)(x[[1,2]]))) %>% 
    unnest(rem_id, keep_empty = T) %>% 
    mutate(ran_order = map(ran_order, function(x)(x[-1,])))
  
  kits[[i]][['3dDigital']] <- ratio3d %>% unnest(kit_data) %>% select(ratio, type)
  
}


#Assigning units to kits
for(i in 1:length(kits)){
  
  ratioInKit <- kit_ratios[,i]
  
  ratio2d <- ratio2d %>% 
    mutate(kit_data = NA, rem_id = NA) %>% 
    mutate(kit_data = ifelse(ratio %in% ratioInKit, map(ran_order, function(x)(x[1,])), NA),
           rem_id = map(kit_data, function(x)(x[[1,2]]))) %>% 
    unnest(rem_id, keep_empty = T) %>% 
    mutate(ran_order = map(ran_order, function(x)(x[-1,])))
  
  kits[[i]][['2dDigital']] <- ratio2d %>% unnest(kit_data) %>% select(ratio, type)
  
}










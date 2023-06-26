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

#### Setting up graph units ####

#Random assignment of kits
set.seed(4125)

#Setting up kit ratios and order
kit_ratios <- data.frame(combn(1:7, 5))
kit_ratios <- kit_ratios[,sample(1:ncol(kit_ratios), ncol(kit_ratios))]

#Defining available graph units
#ratio3p: 3D printed graphs
#ratio3d: 3D digital graphs
#ratio3d: 2D digital graphs
#ratio3s: 3D static graphs

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

#### Assigning 3d print units to kits ####
for(i in 1:length(kits)){
  
  #Get set of 5 ratios
  ratioInKit <- kit_ratios[,i]
  
  ratio3p <- ratio3p %>% 
    
    #Set columns to NA for each loop
    mutate(kit_data = NA, rem_id = NA) %>% 
    
    #For ratios, get first row from the random order and removal id
    mutate(kit_data = ifelse(ratio %in% ratioInKit, map(ran_order, function(x)(x[1,])), NA),
           rem_id = map(kit_data, function(x)(x[[1,2]]))) %>% 
    
    #Unnest by ID, keeping all values
    unnest(rem_id, keep_empty = T) %>% 
    
    #If removal ID is not NA, remove first row from random order
    mutate(ran_order = ifelse(!is.na(rem_id), map(ran_order, function(x)(x[-1,])), ran_order))

  #Collect ratio and type and save into kit
  kits[[i]][['3dPrint']] <- ratio3p %>% unnest(kit_data) %>% select(ratio, type)
  
}


#### Assigning 3d digital units to kits ####
for(i in 1:length(kits)){
  
  ratioInKit <- kit_ratios[,i]
  
  ratio3d <- ratio3d %>% 
    mutate(kit_data = NA, rem_id = NA) %>% 
    mutate(kit_data = ifelse(ratio %in% ratioInKit, map(ran_order, function(x)(x[1,])), NA),
           rem_id = map(kit_data, function(x)(x[[1,2]]))) %>% 
    unnest(rem_id, keep_empty = T) %>% 
    mutate(ran_order = ifelse(!is.na(rem_id), map(ran_order, function(x)(x[-1,])), ran_order))
  
  kits[[i]][['3dDigital']] <- ratio3d %>% unnest(kit_data) %>% select(ratio, type)
  
}


#### Assigning 2d digital units to kits ####
for(i in 1:length(kits)){
  
  ratioInKit <- kit_ratios[,i]
  
  ratio2d <- ratio2d %>% 
    mutate(kit_data = NA, rem_id = NA) %>% 
    mutate(kit_data = ifelse(ratio %in% ratioInKit, map(ran_order, function(x)(x[1,])), NA),
           rem_id = map(kit_data, function(x)(x[[1,2]]))) %>% 
    unnest(rem_id, keep_empty = T) %>% 
    mutate(ran_order = ifelse(!is.na(rem_id), map(ran_order, function(x)(x[-1,])), ran_order))
  
  kits[[i]][['2dDigital']] <- ratio2d %>% unnest(kit_data) %>% select(ratio, type)
  
}







#### Set 85 Files ####
dir('data/pilot/Set85', recursive = T)
set85Files <- list.files(path = 'data/pilot/Set85', pattern = '*.csv',
           all.files = T, recursive = T)

#Ratio key
set85id = c(1,2,3,4,5,6,9)
ran_id = 1:7
ratio_key = data.frame(cbind(set85id,
                             ran_id = sample(ran_id)))

#Type key
type = c('Type1', 'Type3')
ran_type = sample(1:2)
type_key = data.frame(cbind(type, ran_type))


#All data files
set85 <- lapply(paste0('data/pilot/Set85/', set85Files), read.csv)
set85Files




#----- Including new static charts after all other random generation ----- #

#### Assigning 3d static units to kits ####
ratio3s <- ratio3p %>% 
  mutate(ran_order = map(data, sample_n, size = 16))

for(i in 1:length(kits)){
  
  ratioInKit <- kit_ratios[,i]
  
  ratio2d <- ratio3s %>% 
    mutate(kit_data = NA, rem_id = NA) %>% 
    mutate(kit_data = ifelse(ratio %in% ratioInKit, map(ran_order, function(x)(x[1,])), NA),
           rem_id = map(kit_data, function(x)(x[[1,2]]))) %>% 
    unnest(rem_id, keep_empty = T) %>% 
    mutate(ran_order = ifelse(!is.na(rem_id), map(ran_order, function(x)(x[-1,])), ran_order))
  
  kits[[i]][['3dStatic']] <- ratio2d %>% unnest(kit_data) %>% select(ratio, type)
  
}








#### Table matching random IDs for ratio and type, along with dataset index ####
random_id_key <- data.frame(file = set85Files, fileID = 1:length(set85Files)) %>% 
  mutate(set85id = as.numeric(substr(file, 4, 5)),
         graphtype = substr(file, 7, 11)) %>% 
  left_join(ratio_key, by = c('set85id' = 'set85id')) %>% 
  left_join(type_key, by = c('graphtype' = 'type')) %>% 
  mutate(ran_type = as.numeric(ran_type))



#Provides kits with information and dataset keys (fileID)
kitsWithData <- map(kits, function(x)(map(x, function(y){
  left_join(y, random_id_key,
            by = c('ratio' = 'ran_id', 'type' = 'ran_type'))
}))) 



#Datasets with key of fileID
datasets <- tibble(fileID = 1:14, file = paste0('data/pilot/Set85/', set85Files)) %>% 
  mutate(data = map(file, read_csv))



#Only 3D prints for easy sorting
map(kitsWithData, function(x)(x[['3dPrint']]))[[1]]

#Practice data as fileID 15
practice_data = data.frame(fileID = 15, file = 'data/Set63/Ratio1895/Type1-Rep01.csv') %>% 
  mutate(data = map(file, read_csv),
         file = 'practice_data')

datasets <- bind_rows(datasets, practice_data)

#### Saving data ####
save(kitsWithData, file = 'experiment_interface/data/kits.Rdata')
save(datasets, file = 'experiment_interface/data/set85data.Rdata')












#----- Check with old data to verify that kits are the same except for new plots

# kitsWithData2 <- kitsWithData
# 
# load('experiment_interface/data/kits.Rdata')
# 
# #bind_rows(map(kitsWithData, function(x)(x[[c(1,2,3)]]))) == bind_rows(kitsWithData2)
# 
# 
# k.old = bind_rows(map(kitsWithData, bind_rows, .id = 'plot'), .id = 'kit')
# k.new = bind_rows(map(kitsWithData2, bind_rows, .id = 'plot'), .id = 'kit') %>% 
#   filter(plot != '3dStatic')
# 
# all(k.old == k.new)

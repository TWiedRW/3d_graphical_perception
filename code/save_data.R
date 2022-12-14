## ---------------------------
##
## Script name: save_data.R
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2022-10-27
##
## ---------------------------
##
## Notes:
##   Read CM values, choose one set
##   Save 
##
## ---------------------------

source('generate_data2.R')

#Reading data and selecting random possible set
data = read.csv('CM_possible_values.csv')

set.seed(21)
data1 = data[data$Set == sample(1:max(data$Set), 1),]
x1 = data1[,'Value1']; x2 = data1[, 'Value2']



#Create data folder if non-existent
if(!file.exists('data'))(dir.create('data'))

#Number of sets, ideally randomized to each sample size
n_sets = 10

#Folder for set
for(i in 1:n_sets){
  set = paste0('data/Set ', i)
  if(!file.exists(set))(dir.create(set))
  
  #Folder for graph type
  for(j in c('2D', '3D Digital', '3D Prints')){
    graph = paste0(set, '/', j)
    if(!file.exists(graph))(dir.create(graph))
    
    #Folder for comparison type
    for(k in c(1,3)){
      type = paste0(graph, '/Type ', k)
      if(!file.exists(type))(dir.create(type))
      if(k == 1)(output_type = 'Type1')
      if(k == 3)(output_type = 'Type3')
      generate_data(value1 = x1, value2 = x2, 
                    type = output_type, random_process = 'beta',
                    save_dir = type)
    }
  }
}

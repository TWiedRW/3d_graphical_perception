## ---------------------------
##
## Script name: save_plots.R
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
##   
##
## ---------------------------


source('generate_plot.R')
source('save_data.R')

#Create data folder if non-existent
if(!file.exists('plots'))(dir.create('plots'))

#Folder for set
for(i in 1:n_sets){
  set = paste0('plots/Set ', i)
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
    }
  }
}



#List of all data folders
folders = list.dirs(path = 'data')

#Full file paths (Last component is "Type")
folder_locs = folders[grepl('Type', folders)]






###
for(i in 1:length(folder_locs)){
  
  #Identify graph type folder
  if(grepl('2D', folder_locs[i]))(graph_type = 2)
  if(grepl('3D', folder_locs[i]))(graph_type = 3) #NEED TO ADJUST FOR STL AND PLOTTED GRAPH
  
  #Getting data within type and set
  data_sets = dir(folder_locs[i])
  
  ###Generating plots for each data set
  for(j in 1:length(data_sets)){
    
    #File location
    file_loc = paste0(folder_locs[i], '/', data_sets[j])
    
    #Reading data
    data = read.csv(file_loc)
    
    #Generating plot
    p = generate_plot(data, type = graph_type)
    
    #Change location to match plots file path
    plot_loc = gsub('data/', 'plots/', file_loc)
    plot_loc = gsub('csv', 'jpeg', plot_loc)
    
    #SAVE 2D ONLY!!! NEED TO INCLUDE STL ONCE Bar3D IS COMPLETE!
    ggsave(plot_loc, 
           width = 4.125,
           height = 5)
  }
  
}



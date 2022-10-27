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




folders = list.dirs(path = 'data')
folder_locs = folders[grepl('Type', folders)]

for(i in 1:length(folder_locs)){
  if(grepl('2D', folder_locs[i]))(graph_type = 2)
  if(grepl('3D', folder_locs[i]))(graph_type = 3)
  data_sets = dir(folder_locs[i])
  
  for(j in 1:length(data_sets)){
    file_loc = paste0(folder_locs[i], '/', data_sets[j])
    data = read.csv(file_loc)
    p = generate_plot(data, type = graph_type)
    
    plot_loc = gsub('data/', 'plots/', file_loc)
    plot_loc = gsub('csv', 'jpeg', plot_loc)
    ggsave(plot_loc, 
           width = 4.125,
           height = 5)
  }
  
}



## ---------------------------
##
## Script name: Bar2D.R
##
## Purpose of script: Creates 2D plot
##
## Author: Tyler Wiederich
##
## Date Created: 2022-09-22
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

Bar2D = function(data, mark_height = 5){
 
  require(ggplot2)
  
  #Inserting dot height
  data[!is.na(data[,'Identifier']),'Identifier'] <- as.numeric(mark_height)
  
  #Plot
  ggplot(data, mapping = aes(x = GroupOrder, y = Height)) +
    facet_grid(.~Group, switch = 'x') + 
    geom_col(color = 'black',
             fill = NA,
             width = 1) +
    geom_point(data = data[!is.na(data[,'Identifier']),],
               mapping = aes(x = GroupOrder, y = as.numeric(Identifier)),
               size = 1) +
    scale_x_discrete() +
    ylim(0, 100) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 20),
          strip.text = element_text(size = 20),
          legend.position = 'none')
}
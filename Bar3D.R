## ---------------------------
##
## Script name: Bar3D.R
##
## Purpose of script:
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

Bar3D = function(data){
  require(rayshader)
  
  
  
}










#Sample plot

samp = read.csv('data/type1_dataset54_56.23_compared_to_26.1.csv')
samp[,'Marker'] <- rep(1:5, 2)

samp[is.na(samp[,'Identifier']),'Marker'] <- NA


require(ggplot2)

p = ggplot(samp, mapping = aes(x = GroupOrder, y = 1,
                           fill = Height)) +
  facet_grid(.~Group, switch = 'x') + 
  geom_tile(color = 'black', size = 3/4) +
  geom_point(mapping = aes(x = Marker), na.rm = T) +
  scale_fill_gradient(low = 'grey20', high = 'grey80',
                      limits = c(0, 100)) +
  coord_equal() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = 12),
        plot.background = element_rect(fill = 'white', color = 'white'),
        legend.position = 'none',
        panel.background = element_rect(fill = 'white', color = 'white'))
p

plot_gg(p, width = 6, height = 1, raytrace = F, scale = 600, multicore = F,
        shadow_intensity = 0,
        units = 'in',
        offset_edges = T)  

render_snapshot(clear = T)


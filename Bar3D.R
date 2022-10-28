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
samp = read.csv('/Users/tylerwiederich/Library/CloudStorage/OneDrive-UniversityofNebraska-Lincoln/Research/3d_graphical_perception/data/Set 1/2D/Type 1/type1_dataset1.csv')

#Position of identifying markers
samp[,'Marker'] <- rep(1:5, 2)
samp[is.na(samp[,'Identifier']),'Marker'] <- NA

#Identify left and right bars for shape argument
samp[!is.na(samp[,'Identifier']),'Shape'] <- c('Left', 'Right')

shapes <- c('Left' = 16, 'Right' = 17)


require(ggplot2)
require(rayshader)

p = ggplot(samp, mapping = aes(x = GroupOrder, y = 1,
                           fill = Height
                           )) +
  facet_grid(.~Group, switch = 'x') + 
  geom_tile() +

  scale_fill_gradient(low = 'grey20', high = 'grey80',
                      limits = c(0, 100)) +  

  coord_equal() +
  theme(
    legend.position = 'none',
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.text = element_blank()
        )
p



p2 = ggplot(samp, mapping = aes(x = GroupOrder, y = 1,
                               #fill = Height + 0.5, 
                               color = Height + 0.5
                           )) +
  facet_grid(.~Group, switch = 'x') + 
  geom_tile(fill = NA, color = NA) +
  geom_point(mapping = aes(x = Marker, shape = Shape), 
             na.rm = T,
             size = 6) +
  scale_color_gradient(low = 'grey20', high = 'grey80',
                     limits = c(0, 100)) +  
  coord_equal() +
  theme(
    legend.position = 'none',
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.text = element_blank()
  )
p2



plot_gg(p, width = 4.125, height = 1, raytrace = F, scale = 1500, multicore = F,
        shadow_intensity = 0,
        emboss_text = 0.02,
        preview = F,
        units = 'in',
        offset_edges = 0.000001,
        theta = 0,
        phi = 0)  



plot_gg(p2, width = 4.125, height = 1, raytrace = F, scale = 1500, multicore = F,
        shadow_intensity = 0,
        emboss_text = 0.05,
        preview = F,
        units = 'in',
        offset_edges = 0.000001,
        theta = 0,
        phi = 0)  



rayshader::save_3dprint('test_print.stl',
                        remove_extras = T,
                        unit = 'in',
                        maxwidth = 4.125,
                        clear = T)






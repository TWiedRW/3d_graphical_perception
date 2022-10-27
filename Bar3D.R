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
require(rayshader)

p = ggplot(samp, mapping = aes(x = GroupOrder, y = 1,
                           fill = Height)) +
  facet_grid(.~Group, switch = 'x') + 
  geom_tile(size = 1) +
  geom_point(mapping = aes(x = Marker), na.rm = T) +
  scale_fill_gradient(low = 'grey20', high = 'grey80',
                      limits = c(0, 100)) +
  coord_equal() +
  theme(legend.position = 'none',
        #panel.background = element_blank(),
        panel.grid = element_blank(),
        #plot.background = element_blank(),
        strip.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())
p

plot_gg(p, width = 4.125, height = 1, raytrace = F, scale = 500*1.171, multicore = F,
        shadow_intensity = 0,
        units = 'in',
        offset_edges = T,
        theta = 0,
        phi = 0)  



#rayshader::save_3dprint('test_print.stl',
#                        remove_extras = F,
#                        unit = 'in',
#                        maxwidth = 4.125,
#                        clear = T)








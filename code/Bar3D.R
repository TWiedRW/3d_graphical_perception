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

Bar3D = function(samp, output_style = c('3D', 'stl'), scale = 1096.935){
  
  
  #Position of identifying markers
  samp[,'Marker'] <- rep(1:5, 2)
  samp[is.na(samp[,'Identifier']),'Marker'] <- NA
  
  #Identify left and right bars for shape argument
  samp[!is.na(samp[,'Identifier']),'Shape'] <- c('Left', 'Right')
  
  shapes <- c('Left' = 16, 'Right' = 17)
  
  if(output_style == '3D'){
    emboss <- 0
    } else {
      emboss <- 0.0125
    }
  
  require(ggplot2)
  require(rayshader)
  
  p = ggplot(samp, mapping = aes(x = GroupOrder, y = 1,
                                 fill = Height
  )) +
    facet_grid(.~Group, switch = 'x') + 
    geom_tile(color = 'black') +
    
    scale_fill_gradient(low = 'grey80', high = 'grey80',
                        limits = c(0, 100)) +  
    
    coord_equal() +
    theme(
      legend.position = 'none',
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      strip.background = element_blank(),
      panel.background = element_rect(fill = 'white',
                                      color = 'white'),
      plot.background = element_rect(fill = 'white',
                                     color = 'white'),
      strip.text = element_text(size = 20, face = 'bold'),
      panel.grid = element_blank(),
      axis.text = element_blank()
    )
  p
  
  
  
p2 = ggplot(samp, mapping = aes(x = GroupOrder, y = 1,
                                color = Height + 0.5)) +
  facet_grid(.~Group, switch = 'x') +
  geom_tile(fill = NA, color = NA) +
  geom_point(mapping = aes(x = Marker, shape = Shape),
             na.rm = T,
             size = 6) +
  scale_color_gradient(low = '#000000', high = '#000000',
                       limits = c(0, 100)) +
  coord_equal() +
  theme(
    legend.position = 'none',
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    panel.background = element_rect(fill = 'white',
                                    color = 'white'),
    plot.background = element_rect(fill = 'white',
                                   color = 'white'),
    strip.text = element_text(size = 20, face = 'bold'),
    panel.grid = element_blank(),
    axis.text = element_blank()
  )
p2
  
  
  
  
  
#THIS IS A GOOD COMBINATION OF ARGUMENTS! DO NOT DELETE  
#  plot_gg(p, width = 4.125, height = 1, raytrace = F, scale = 1500, multicore = F,
#          shadow_intensity = 0,
#          #emboss_text = 0.025,
#          emboss_text = 0,
#          preview = F,
#          units = 'in',
#          theta = 25,
#          phi = 40,
#          #soliddepth = 0,
#          soliddepth = -100/1000,
#          solidcolor = 'white', 
#          background = 'white',
#          solidlinecolor = 'white',
#          shadow = FALSE)  
  


  
  plot_gg(p, width = 4.125, height = 1, raytrace = F, scale = scale, multicore = F,
          shadow_intensity = 0,
          emboss_text = emboss,
          #emboss_text = 0,
          preview = F,
          offset_edges = 0.000001,
          units = 'in',
          theta = 20,
          phi = 15,
          soliddepth = -5/100,
          solidcolor = 'grey80', 
          background = 'white',
          solidlinecolor = 'grey80',
          shadow = FALSE)  
  
  
#THIS ADDS THE POINTS! DO NOT DELETE
  plot_gg(p2, width = 4.125, height = 1, raytrace = F, scale = scale, multicore = F,
          shadow_intensity = 0,
          emboss_text = emboss,
          preview = F,
          units = 'in',
          theta = 20,
          phi = 15,
          soliddepth = -5/100,
          solidcolor = 'grey80', 
          background = 'white',
          solidlinecolor = 'grey80',
          shadow = FALSE)  
}
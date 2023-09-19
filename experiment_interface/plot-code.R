require(tidyverse)

#### 2D Bar Chart ####
Bar2D <- function(data, mark_height = 5, color = 'grey50', shape_order=c(1,2)){
  
  if(shape_order==1){
    shape1 = 17
    shape2 = 16
  } else if(shape_order==2){
    shape1 = 16
    shape2 = 17
  } else {
      cat('Invalid shape order for 2D chart: defaulting to original order')
      shape1 = 17
      shape2 = 16
    }
  
  ggplot(data, mapping = aes(x = GroupOrder, y = Height)) +
    facet_grid(.~Group, switch = 'x') + 
    geom_col(color = 'black',
             fill = color,
             #color = color,
             width = 1) +
    geom_point(data = dplyr::filter(data, IDchr != ""),
               mapping = aes(x = GroupOrder, y = mark_height),
               shape = c(shape1, shape2),
               size = 3) +
    scale_x_discrete() +
    ylim(0, 100) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid = element_blank(),
          #aspect.ratio = 4/3.3,
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 20),
          strip.text = element_text(size = 20),
          legend.position = 'none')
}

#### 3D Printed (Choose from kit) ####
print3DPlot <- ggplot(mapping = aes(x = 1, y = 1)) +
  geom_text(aes(label = 'Please randomly select a chart\nfrom your kit'),
            size = 6) +
  theme_void()

Bar3D <- function(file, color = 'black'){
  rgl::readSTL(file, 
               col = color,
               lit = T,
               theta = 90,
               phi = 50,
               specular = 'black',
               ambient = 'black',
               fog = F,
               shininess = 95)
  clear3d(type = 'lights')
  # rgl.pop('lights')
  # 
  #Colors back of plot
  light3d(viewpoint.rel = F,
           x = 7*130, y = -1000, z = 1000,
           diffuse = 'grey80',
           specular = 'grey80',
           ambient = 'grey80')
  #Colors front of plot
  light3d(viewpoint.rel = F,
          x = -6*130, y = 1030, z = 1000,
          diffuse = 'grey80',
          specular = 'grey80',
          ambient = 'grey80')
  #Colors bottom of plot
  light3d(viewpoint.rel = F,
          x = 130/2, y = 15, z = -100,
          diffuse = 'grey80',
          specular = 'grey80',
          ambient = 'grey80')
}


# x = c(7*130, -6*130, 130/2) #light positions from testing
# y = c(-1000, 1030, 15)
# z = c(1000, 1000, -100)

#### 3D Bar Chart ####
# Bar3D <- function(samp, output_style = '3D', scale = 1096.935){
#   
#   if(output_style == '3D'){
#     emboss <- 0
#   } else {
#     emboss <- 0.0125
#   }
#   
#   require(ggplot2)
#   require(rayshader)
#   
#   p = ggplot(samp, mapping = aes(x = GroupOrder, y = 1,
#                                  fill = Height
#   )) +
#     facet_grid(.~Group, switch = 'x') +
#     geom_tile(color = 'black') +
#     
#     scale_fill_gradient(low = 'grey80', high = 'grey80',
#                         limits = c(0, 100)) +
#     
#     coord_equal() +
#     theme(
#       legend.position = 'none',
#       axis.title = element_blank(),
#       axis.ticks = element_blank(),
#       strip.background = element_blank(),
#       panel.background = element_rect(fill = 'white',
#                                       color = 'white'),
#       plot.background = element_rect(fill = 'white',
#                                      color = 'white'),
#       strip.text = element_text(size = 20, face = 'bold'),
#       panel.grid = element_blank(),
#       axis.text = element_blank()
#     )
#   
#   
#   
#   #Height of bars of interest
#   samp[samp[,'Identifier'] == 'random','Height'] <- NA
#   p2 = ggplot(samp, mapping = aes(x = GroupOrder, y = 1,
#                                   color = Height + 2)) +
#     facet_grid(.~Group, switch = 'x') +
#     geom_tile(fill = NA, color = NA) +
#     geom_point(mapping = aes(x = GroupOrder, shape = IDchr),
#                na.rm = T,
#                size = 6) +
#     scale_color_gradient(low = '#000000', high = '#000000',
#                          limits = c(0, 100)) +
#     coord_equal() +
#     theme(
#       legend.position = 'none',
#       axis.title = element_blank(),
#       axis.ticks = element_blank(),
#       strip.background = element_blank(),
#       panel.background = element_rect(fill = 'white',
#                                       color = 'white'),
#       plot.background = element_rect(fill = 'white',
#                                      color = 'white'),
#       strip.text = element_text(size = 20, face = 'bold'),
#       panel.grid = element_blank(),
#       axis.text = element_blank()
#     )
#   
#   
#   plot_gg(p, width = 4.125, height = 1, raytrace = F, scale = scale, multicore = F,
#           shadow_intensity = 0,
#           emboss_text = emboss,
#           #emboss_text = 0,
#           preview = F,
#           offset_edges = 0.000001,
#           units = 'in',
#           theta = 20,
#           phi = 15,
#           soliddepth = -5/100,
#           solidcolor = 'grey80',
#           background = 'white',
#           solidlinecolor = 'grey80',
#           shadow = FALSE)
#   
#   
#   #THIS ADDS THE POINTS! DO NOT DELETE
#   plot_gg(p2, width = 4.125, height = 1, raytrace = F, scale = scale, multicore = F,
#           shadow_intensity = 0,
#           emboss_text = emboss,
#           preview = F,
#           units = 'in',
#           theta = 20,
#           phi = 15,
#           soliddepth = -5/100,
#           solidcolor = 'grey80',
#           background = 'white',
#           solidlinecolor = 'grey80',
#           shadow = FALSE)
# }




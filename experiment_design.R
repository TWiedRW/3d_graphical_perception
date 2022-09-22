## One replication of Type 1

# - generate 9 random bars with first bar being the pair max
# - for Type 1, randomize placement of first 5
# - for Type 3, randomize placement of first bar
#       within first 5, new bar within second 5

require(ggplot2)

#set.seed(21)

percent_smaller <- 0.82
bars <- runif(9, min = 0, max = 100)
target <- bars[1]

newbar <- target * percent_smaller

#Arranging 
ordered_placement <- c(sample(c(bars[1:4], newbar), size = 5), bars[5:9])

#Get target positions
position_target <- match(x = target, table = ordered_placement)
position_newbar <- match(x = newbar, table = ordered_placement)

#Groups 
groups <- rep(c('A', 'B'), each = 5)

#Color Groups
bar_colors <- rep('random', 10)
bar_colors[position_target] <- 'target'
bar_colors[position_newbar] <- 'newbar'

bar_colors

dat <- data.frame(grouping = groups, yval = ordered_placement,
                  bar_colors = bar_colors, ordering = c(1:5, 1:5))

dat

#Plot
p = ggplot(dat, mapping = aes(x = ordering, y = yval, fill = bar_colors)) +
  facet_grid(.~grouping, switch = 'x') + 
  geom_col(position = position_dodge2(padding = 0),
           color = 'black',
           #fill = 'grey80',
           width = 1) +
  scale_x_discrete() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 18),
        legend.position = 'none')

library(rayshader)
plot_gg(p, width = 5, height = 5, raytrace = F, preview = T)




ggdiamonds = ggplot(diamonds) +
  stat_density_2d(aes(x = x, y = depth, fill = stat(nlevel)), 
                  geom = "polygon", n = 200, bins = 50,contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")

par(mfrow = c(1, 2))

plot_gg(ggdiamonds, width = 5, height = 5, raytrace = FALSE, preview = TRUE)
plot_gg(ggdiamonds, width = 5, height = 5, multicore = TRUE, scale = 250, 
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))
Sys.sleep(0.2)
render_snapshot(clear = TRUE)

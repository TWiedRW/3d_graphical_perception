## One replication of Type 1

# - generate 9 random bars with first bar being the pair max
# - for Type 1, randomize placement of first 5
# - for Type 3, randomize placement of first bar
#       within first 5, new bar within second 5

set.seed(21)

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

dat <- data.frame(grouping = groups, yval = ordered_placement)

#Plot
ggplot(dat, mapping = aes(x = grouping, y = yval)) +
  geom_col(position = position_dodge2(padding = 0),
           color = 'black',
           fill = 'grey80',
           width = 3/4) +
  scale_x_discrete() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank())

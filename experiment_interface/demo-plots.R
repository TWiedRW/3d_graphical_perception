require(ggplot2)
p1 <- ggplot(mapping = aes(x = 1:2, y = c(80, 40))) +
  geom_col(width = 1,
           fill = NA,
           color = 'black') +
  geom_point(aes(y = 5), shape = c(16,17),
             size = 2) +
  ylim(0, 100) +
  labs(title = 'Smaller bar is 50% the \nsize of the larger bar') +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 20),
        legend.position = 'none',
        aspect.ratio = 4/3.3)



p2 <- ggplot(mapping = aes(x = 1:2, y = c(60, 80))) +
  geom_col(width = 1,
           fill = NA,
           color = 'black') +
  geom_point(aes(y = 5), shape = c(16,17),
             size = 2) +
  ylim(0, 100) +
  labs(title = 'Smaller bar is 75% the \nsize of the larger bar') +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 20),
        legend.position = 'none',
        aspect.ratio = 4/3.3)

p3 <- ggplot(mapping = aes(x = 1:2, y = c(80, 20))) +
  geom_col(width = 1,
           fill = NA,
           color = 'black') +
  geom_point(aes(y = 5), shape = c(16,17),
             size = 2) +
  ylim(0, 100) +
  labs(title = 'Smaller bar is 25% the \nsize of the larger bar') +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(size = 20),
        legend.position = 'none',
        aspect.ratio = 4/3.3)





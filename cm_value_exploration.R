## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2022-10-18
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

library(tidyverse)

values = read_csv('CM_possible_values.csv')

values_d = values %>% 
  select(Value1:Set) %>% 
  group_by(Set) %>% 
  arrange(Set, Value1, Value2) %>% 
  mutate(Sequence = 1:10,
         Set = as.character(Set)) %>% 
  ungroup() %>% 
  mutate(VL1 = log(Value1),
         VL2 = log(Value2),
         D = Value2 - Value1,
         DL = VL2 - VL1,
         Ratio = round(Value1/Value2,3)) %>% 
  arrange(Set, Ratio)
  



  ggplot(values_d,
         mapping = aes(x = Value1,
                       y = Value2,
                       color = Set)) +
  geom_point(position = 'jitter') +
  theme(legend.position = 'none')

cm_seq = data.frame(Seq = 1:10, CM = sort(unique(c(values_d$Value1, values_d$Value2))))

values_d %>% 
  pivot_longer(cols = c(Value1, Value2)) %>% 
  left_join(cm_seq, by = c('value' = 'CM')) %>% 
  mutate(Within = rep(1:2, times = 5040/2)) %>% 
  filter(Set == 12) %>% 
  ggplot(aes(x = Seq, y = value, 
             group = Sequence
             #, color = as.character(Sequence)
             )) +
    geom_step(direction = 'vh',
              linetype = 'dotted',
              color = 'grey20') +
    geom_point() +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  scale_y_continuous(#breaks = round(c(0,cm_values),2),
                     limits = c(0, 100)) +
  facet_grid(.~Sequence) +
  labs(x = 'Order of CM Values',
       y = '',
       title = 'Height Comparisons of Values') +
  ggthemes::theme_base() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8),
        strip.text = element_text(size = 8),
        title = element_text(size = 12))





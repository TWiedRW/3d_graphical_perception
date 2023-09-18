## ---------------------------
##
## Script name: pilot_design.R
##
## Purpose of script:
##
## Author: Tyler Wiederich
##
## Date Created: 2022-11-03
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# Block design ------------------------------------------------------------

#Design parameters
n_sets = 1 # number of replicates of each set of 7 ratios x 2 types
n_kits = 12
n_ratio = 5 # for each kit, 7 total

#Packages and seed for reproducibility
library(purrr)
library(dplyr)
library(tidyr)
source('code/generate_data2.R')
source('code/generate_plot.R')
source('code/generate_test_data.R')
set.seed(6214)



#Reading data and selecting random possible set

data <- read.csv('CM_possible_values.csv') %>%
  group_by(Set) %>%
  mutate(ratio = round(Value1/Value2, 3), 
         ratio_id = 1:n()) %>%
  filter(!duplicated(ratio)) %>%
  ungroup() %>%
  arrange(Set, ratio) %>%
  select(Set, Value1, Value2, ratio, ratio_id) %>%
  nest(data = -Set) %>%
  arrange(Set) %>%
  slice_sample(n = 1) %>% # This selects a set
  crossing(rep = 1:n_sets, .) %>%
  unnest(data) %>%
  mutate(bars = purrr::map2(Value1, Value2, create_data)) %>%
  crossing(type = c(1, 3)) %>%
  mutate(filename = sprintf("data/pilot/Set%02d/id-%02d/Type%d-Rep%02d", 
                            Set, ratio_id, type, rep)) %>%
  mutate(data = purrr::map2(bars, type, fix_data_type)) %>%
  mutate(plot_2d = purrr::map2(data, filename, write_data))

save(data, file = "data/pilot/Overall_Data_Frame.Rdata")


#Factor combinations
comparisons = c('Type1', 'Type3')
types = c('2d', '3d_digital', '3d_print')
ratios = unique(c(0.178, 0.261, 0.383, 0.464, 0.464, 
                  0.562, 0.681, 0.681, 0.825, 0.825))


#Creating data set
kit = rep(1:n_kits, each = 3*n_ratio)
graph_type = rep(rep(types, each = n_ratio), times = 3)
sample_ratio = c(replicate(n_kits, rep(sample(ratios, n_ratio), times = 3)))
sample_types = sample(comparisons, 3*n_kits*n_ratio, replace = T)

dat = data.frame(kit = kit,
                 graph_type = graph_type,
                 comparison = sample_types,
                 ratio = sample_ratio)

#Reading data and selecting random possible set
data = read.csv('CM_possible_values.csv')
data1 = data[data$Set == sample(1:max(data$Set), 1),]
data1$ratio = round(data1$Value1/data1$Value2,3)
values = data1[!duplicated(data1$ratio),c('Value1', 'Value2', 'ratio')]


#Pilot study
pilot_study = dat %>% 
  left_join(values, by = c('ratio' = 'ratio')) %>% 
  mutate(dat = pmap(list(Value1, Value2, comparison), 
                    generate_data, random_process = 'beta', save = F),
         plot2d = map(dat, generate_plot, type = 2))





# for(i in 1:nrow(pilot_study)){
#   ggsave(filename = paste0('pilot/data', i),
#          plot = pilot_study$plot2d[[i]],
#          width = 4.125, height = 5,
#          device = 'png')
# }

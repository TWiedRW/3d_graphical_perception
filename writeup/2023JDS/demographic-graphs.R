library(ggplot2)
library(dplyr)
library(magrittr)
library(patchwork)
library(RSQLite)

con <- dbConnect(SQLite(), 'experiment_interface/department.db')
users <- dbReadTable(con, 'user')
dbDisconnect(con)

users

users$userAppStartTime <- as.POSIXct(users$userAppStartTime,
           origin = '1970-1-1')

age.levels = c(sort(unique(users$age))[9], sort(unique(users$age))[1:8])
p1 = users[users[,'userAppStartTime'] <= '2023-05-22',] %>% 
  group_by(age) %>% 
  summarise(count = n()) %>% 
  ggplot(mapping = aes(x = factor(age, levels = age.levels),
                       y = count)) + 
  geom_bar(stat = 'identity') +
  geom_text(mapping = aes(y = count+1, label = count),
             size = 2) +
  labs(title = 'Age Groups',
       x = '',
       y = 'Count') +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 45, hjust=1, size = 8))



p2 = users[users[,'userAppStartTime'] <= '2023-05-22',] %>% 
  group_by(gender) %>% 
  summarise(count = n()) %>% 
  ggplot(mapping = aes(x = gender,
                       y = count)) + 
  geom_bar(stat = 'identity') +
  geom_text(mapping = aes(y = count+1, label = count),
             size = 2) +
  labs(title = 'Gender Groups',
       x = '',
       y = 'Count') +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 45, hjust=1, size = 8))


educ.levels = sort(unique(users$education))[c(2,4,3,1)]
p3 = users[users[,'userAppStartTime'] <= '2023-05-22',] %>% 
  group_by(education) %>% 
  summarise(count = n()) %>% 
  ggplot(mapping = aes(x = factor(education, levels = educ.levels),
                       y = count)) + 
  geom_bar(stat = 'identity') +
  geom_text(mapping = aes(y = count+2, label = count),
             size = 2) +
  labs(title = 'Education Levels',
       x = '',
       y = 'Count') +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 45, hjust=1, size=8))

p = p1 + p2 + p3
p


ggsave('writeup/2023JDS/demographic-plots.png', p,
       width = 8, height = 4)

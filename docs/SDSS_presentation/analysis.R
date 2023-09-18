library(tidyverse)
library(RSQLite)
library(lme4)

con = dbConnect(SQLite(), 'experiment_interface/department.db')
dbListTables(con)
results = dbReadTable(con, 'results')
users = dbReadTable(con, 'user')
userMatrix = dbReadTable(con, 'userMatrix')
dbDisconnect(con)

users
results


#Fill in correct values for incorrect 3d graph kits
# 1 result, need to fill in fileID, graphtype, and plot
results = results %>% 
  mutate(fileID = ifelse(graphCorrecter %in% 'id-01/Type1-Rep01', 1, fileID),
         graphtype = ifelse(graphCorrecter %in% 'id-01/Type1-Rep01', 'Type1', graphtype),
         plot = ifelse(graphCorrecter %in% 'id-01/Type1-Rep01', '3dPrint', plot))



load('experiment_interface/data/set85data.Rdata')
load('experiment_interface/data/kits.Rdata')


trueRatios = datasets %>% 
  mutate(ratio.df = map(data, function(x)(x[!is.na(x[,'IDchr']),4])),
         trueRatio = map(ratio.df, function(x)(x[1,'Height'] / x[2,'Height']))) %>% 
  unnest(trueRatio) %>% 
  filter(fileID != 15) %>% 
  #mutate(Height = Height * 100) %>% 
  select(fileID, Height)

res = results %>% 
  left_join(trueRatios, by = 'fileID') %>% 
  mutate(response = log2(abs(byHowMuch - Height*100) + 1/8),
         subject = paste0(nickname, participantUnique, appStartTime),
         ratioLabel = round(100*Height, 1)) %>% 
  arrange(appStartTime) %>% 
  filter(whichIsSmaller == 'Triangle (▲)',
         as.Date.POSIXct(appStartTime) <= '2023-05-22') #Only verified results from before the start of SDSS





library(lme4)
mod = lmer(response ~ (1|subject) + ratio + type + ratio:plot,
     data = res)
anova(mod)



res %>% 
  ggplot(mapping = aes(x = graphtype, y = response)) +
  geom_point(stat = 'summary',
             fun = 'mean') +
  facet_grid(plot ~ ratioLabel) + 
  theme_bw() +
  labs(title = 'Replication of Figure 11')



res %>% 
  ggplot(mapping = aes(x = ratioLabel, y = response)) +
  geom_point(stat = 'summary',
             fun = 'midmean') +
  geom_smooth(se = F, alpha = 1/8) +
  facet_grid(plot ~ graphtype) + 
  theme_bw() +
  labs(title = 'Replication of Figure 13')


res %>% 
  ggplot(mapping = aes(x = response, y = graphtype)) +
  geom_violin() +
  facet_grid(plot ~ .) + 
  theme_bw() +
  labs(title = '"Replication" of Figure 16',
       caption = 'Replace with 95% CIs when available')

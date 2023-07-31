library(ggplot2)
library(dplyr)
library(magrittr)
library(patchwork)
library(RSQLite)

con = dbConnect(SQLite(), 'experiment_interface/department.db')
dbListTables(con)
results = dbReadTable(con, 'results')
users = dbReadTable(con, 'user')
userMatrix = dbReadTable(con, 'userMatrix')
dbDisconnect(con)


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


plot.types = c('2D', 'Rendered 3D', '3D Printed')
names(plot.types) = c('2dDigital', '3dDigital', '3dPrint')



res %>% 
  group_by(Height, graphtype, plot) %>% 
  summarize(midmean = mean(response, trim=0.25, na.rm = T)) %>% 
  ggplot(mapping = aes(x = Height, y = midmean,
                       color = graphtype)) +
  geom_point() +
  geom_smooth(se = F, alpha=1/2) +
  scale_color_discrete(labels = c('Adjacent', 'Separated')) +
  facet_wrap(~plot, labeller = labeller(plot = plot.types)) +
  labs(title = '',
       x = 'True Proportional Difference (%)',
       y = 'Midmeans of Log Error',
       color = 'Comparison Type') +
  theme_bw() +
  theme(legend.position = 'bottom')

ggsave('writeup/2023JDS/log-error-midmeans.png', width = 8, height = 4)





library(lme4)
mod = lmer(response ~ (1|subject) + ratio + type + ratio:plot,
           data = res)
anova(mod)
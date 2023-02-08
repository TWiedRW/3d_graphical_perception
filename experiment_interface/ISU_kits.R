con <- dbConnect(SQLite(), 'experiment_interface/20230209-graphicsGroup.db')
user = dbReadTable(con, 'user')
res = dbReadTable(con, 'results')
dbDisconnect(con)


str(res)
View(res)
View(user)






myFun = function(x){
  x %>% 
    bind_rows(.id = 'plot') %>% 
    mutate(file = paste0('data/pilot/Set85/', gsub('.csv', '', file))) %>% 
    filter(plot == '3dPrint') %>% 
    select(file)
}

lapply(kits, myFun)

saveKit <- list()
for(i in 1:21){
  saveKit[[i]] <- kits[[i]] %>% 
    bind_rows(.id = 'plot') %>% 
    mutate(file = paste0('data/pilot/Set85/', gsub('.csv', '', file)),
           kit = i) %>% 
    filter(plot == '3dPrint') %>% 
    ungroup() %>% 
    select(kit, set85id, file)
}

colors = data.frame(ratio = c(1,2,3,4,5,6,9), color = c('cyan','green','red','yellow','blue','orange','purple'))

set.seed(513)
res = bind_rows(saveKit) %>% 
  left_join(colors, by = c('set85id' = 'ratio')) %>% 
  select(-set85id) %>% 
  filter(kit %in% sample(1:21, 2))

write.csv(res, 'kits_for_3D_printed_charts_ISU.csv', row.names = F)



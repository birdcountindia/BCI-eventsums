

cur_date <- "2023-06-16" %>% as_date() %>% floor_date(unit = "month")
rel_date <- "2023-05-16" %>% as_date() %>% floor_date(unit = "month")



cur_date <- "2023-04-16" %>% as_date() %>% floor_date(unit = "month")
rel_date <- "2023-03-16" %>% as_date() %>% floor_date(unit = "month")

cur_event <- list_multiregional[1,] 
cur_event <- list_national[1,] 


###########

## subset Nepal data for their app

nep = data %>% filter(ST_NM == "NEPAL")
nep1 = nep[,-c(2,4,10,11,25,26,27,28,30,33,34,35,36,37)]
names(nep1)[22:23] = c("day","year")
write.csv(nep1,"nepal_data.csv", row.names = F)
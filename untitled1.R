
####### Campus bird count

######################################### Plot campus map with info

statemap1 = gBuffer(statemap1, byid=TRUE, width=0)
finalmap = statemap1

temp = datafinal %>% 
  group_by(campus) %>%
  summarize(Checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

temp1 = datafinal %>% 
  filter(CATEGORY %in% c("species","issf")) %>%
  filter(!EXOTIC.CODE %in% c("X")) %>%
  group_by(campus) %>%
  summarize(Species = n_distinct(COMMON.NAME))

temp2 = datafinal %>% 
  group_by(campus) %>%
  summarize(Participants = n_distinct(OBSERVER.ID))

temp3 = datafinal %>% 
  group_by(campus) %>%
  summarize(UniqueChecklists = n_distinct(group.id))

temp4 = datafinal %>%
  group_by(COMMON.NAME,group.id) %>% slice(1) %>% ungroup %>%
  group_by(campus) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  group_by(campus,COMMON.NAME) %>% summarize(freq = n()/max(lists)) %>% ungroup %>%
  group_by(campus) %>% arrange(desc(freq)) %>% slice(1) %>% select(campus,COMMON.NAME)

names(temp4)[2] = "MostCommon"

tempx = left_join(temp,temp1)
tempx = left_join(tempx,temp2)
tempx = left_join(tempx,temp3)
tempx = left_join(tempx,temp4)

temp = tempx %>% arrange(desc(Checklists))

write.csv(temp,"CBC2022results.csv", row.names=FALSE)

names(temp)[1] = "Campus"
latlong = datafinal %>% distinct(campus,LONGITUDE,LATITUDE)
names(latlong)[3] = "Campus"
temp = left_join(temp,latlong)
coordinates(temp) = ~LONGITUDE + LATITUDE

proj4string(temp) = "+proj=longlat +datum=WGS84"
proj4string(finalmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
a1 = mapView(finalmap, map.types = c("Esri.WorldImagery"), alpha.regions = 0, popup = FALSE,
             highlight = FALSE, legend = FALSE, label = NA)

a = mapView(temp, zcol = c("Checklists"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Checklists"), 
            popup = leafpop::popupTable(temp,c("Campus","Checklists","Species",
                                               "Participants","UniqueChecklists","MostCommon"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,5,10,20,50,max(na.omit(temp$Checklists)))))

b = a1+a
mapshot(b, "CBC2022.html")
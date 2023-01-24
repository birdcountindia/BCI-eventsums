# adapted from https://github.com/ashwinv2005/repeated-eBirding-event-analyses/blob/master/GBBC%202022.R


require(lubridate)
require(tidyverse)
require(sp)
require(rgeos)
require(ggfortify)
require(rgdal)
require(sf)
require(mapview)
require(leaflet)
require(rmapshaper)

# for each event
cur_event <- list_national 

# filtering for event dates
data0 <- data %>% filter(OBSERVATION.DATE %in% 
                           seq(cur_event$START.DATE, cur_event$END.DATE, by = "days"))




# create and write a file with common names and scientific names of all species
# useful for mapping
temp = data %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(!EXOTIC.CODE %in% c("X")) %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)

write.csv(temp,"GBBCspecieslist2022.csv", row.names=FALSE)


load("maps.RData")

districtmap1 = gSimplify(districtmap, tol=0.01, topologyPreserve=TRUE)
d1 = districtmap@data
districtmap1 = sp::SpatialPolygonsDataFrame(districtmap1, d1)
statemap1 = gSimplify(statemap, tol=0.01, topologyPreserve=TRUE)
s1 = statemap@data
statemap1 = sp::SpatialPolygonsDataFrame(statemap1, s1)

load("clips.RData")

data = data %>% filter(!is.na(group.id))

# add columns with DISTRICT and ST_NM to main data 

temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84"
temp = over(temp,districtmap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp = temp[,1:2]
temp$group.id = rownames(temp) # add column to join with the main data
data = left_join(temp,data)

# add columns with protected area name to main data 

temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84"
temp = over(temp,pamap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$group.id = rownames(temp) # add column to join with the main data
data = left_join(temp,data)


# add columns with GRID ATTRIBUTES to main data

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg1)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg1"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg2)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg2"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg3)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg3"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,gridmapg4)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "gridg4"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,g2clip)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "g2clip"

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,g3clip)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "g3clip"


############ Add first and last name

data$obs.id.num <- gsub("[[:alpha:]]", "", data$OBSERVER.ID)

eBird.users = read.delim("ebd_users_relDec-2021.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                         na.strings = c(""," ",NA))
names(eBird.users) = c("USER_ID","FIRST.NAME","LAST.NAME")
eBird.users$obs.id.num = gsub("[[:alpha:]]", "", eBird.users$USER_ID)

data = left_join(data, eBird.users)
data$FIRST.NAME[data$obs.id.num == "1034924"] = "Monal"
data$LAST.NAME[data$obs.id.num == "1034924"] = "Trivedi"
data$FIRST.NAME[data$obs.id.num == "2874499"] = "Chayan"
data$LAST.NAME[data$obs.id.num == "2874499"] = "Debnath"



#rem = unique(data[is.na(data$FIRST.NAME),]$obs.id.num)


############ Top 10 checklist uploaders

datax = data %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14)
datax = datax %>%
  group_by(FIRST.NAME,LAST.NAME,OBSERVER.ID) %>% summarize(lists15mins = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% ungroup %>%
  arrange(desc(lists15mins)) %>% slice(1:39)
datax = datax[-c(1,2,3,4,5,6,8,24,36),]

write.csv(datax,"GBBCtop30birders2022.csv", row.names=FALSE)

########## Top birder in every state

datax = data %>% filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>%
  filter(!obs.id.num %in% c(852457,879790,974048,2962056,841406,2220798,2215179,2972613,949737,
                            2953613,2965098,3000697,807390,2991603,2965359,1277497))
datax = datax %>%
  group_by(ST_NM,FIRST.NAME,LAST.NAME,obs.id.num) %>% summarize(lists15mins = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% ungroup %>%
  group_by(ST_NM) %>% arrange(desc(lists15mins)) %>% slice(1) %>% ungroup %>% arrange(desc(lists15mins)) %>%
  filter(!is.na(ST_NM))

write.csv(datax,"GBBCtopbirderperstate2022.csv", row.names=FALSE)


############ birders per state

datax = data %>%
  #filter(!obs.id.num %in% c(852457,879790,974048,2962056,841406,2220798,2215179,2972613,949737,
  #                          2953613,2965098,3000697,807390,2991603,2965359,1277497)) %>%
  distinct(ST_NM,FIRST.NAME,LAST.NAME) %>%
  arrange(ST_NM) %>%   filter(!is.na(ST_NM))
write.csv(datax,"GBBCbirdersperstate2022.csv", row.names=FALSE) ###### needs to be rerun

############ state and district level table of checklists

datax = data %>%
  #filter(!obs.id.num %in% c(852457,879790,974048,2962056,841406,2220798,2215179,2972613,949737,
  #                          2953613,2965098,3000697,807390,2991603,2965359,1277497)) %>%
  group_by(DISTRICT) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(lists)) %>% filter(!is.na(DISTRICT))
write.csv(datax,"GBBClistsperdistrict2022.csv", row.names=FALSE)

datax = data %>%
  #filter(!obs.id.num %in% c(852457,879790,974048,2962056,841406,2220798,2215179,2972613,949737,
  #                          2953613,2965098,3000697,807390,2991603,2965359,1277497)) %>%
  group_by(ST_NM) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  arrange(desc(lists)) %>% filter(!is.na(ST_NM))
write.csv(datax,"GBBClistsperstate2022.csv", row.names=FALSE)

############ daywise checklists

datax = data %>%
  group_by(daym) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER), birders = n_distinct(OBSERVER.ID)) %>%
  arrange(desc(lists))
write.csv(datax,"GBBClistsperday2022.csv", row.names=FALSE)

############ day and state wise checklists

datax = data %>%
  group_by(ST_NM,daym) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER), birders = n_distinct(OBSERVER.ID)) %>%
  arrange(ST_NM,daym) %>% filter(!is.na(ST_NM))
write.csv(datax,"GBBClistsperdaystate2022.csv", row.names=FALSE)

############# common species

datax = data %>%
  group_by(COMMON.NAME,group.id) %>% slice(1) %>% ungroup %>%
  mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME) %>% summarize(freq = n()/max(lists)) %>%
  arrange(desc(freq))
write.csv(datax,"GBBCcommonspecies2022.csv", row.names=FALSE)


######################################### Plot district map with info

districtmap1 = gBuffer(districtmap1, byid=TRUE, width=0)
finalmap = districtmap1


temp = data %>% 
  filter(CATEGORY %in% c("species","issf")) %>%
  filter(is.na(EXOTIC.CODE)) %>%
  group_by(DISTRICT) %>%
  summarize(Species = n_distinct(COMMON.NAME))

temp1 = data %>% 
  group_by(DISTRICT) %>%
  summarize(Checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

temp2 = data %>% 
  group_by(DISTRICT) %>%
  summarize(Participants = n_distinct(OBSERVER.ID))

temp3 = data %>% 
  group_by(DISTRICT) %>%
  summarize(Locations = n_distinct(LOCALITY.ID))

temp = left_join(temp,temp1)
temp = left_join(temp,temp2)
temp = left_join(temp,temp3)


temp$DISTRICT = as.character(temp$DISTRICT)

effortmap = merge(finalmap,temp, by =  "DISTRICT")
names(effortmap)[1:2] = c("District","State")


proj4string(finalmap) = "+proj=longlat +datum=WGS84"
proj4string(effortmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
a = mapView(effortmap, zcol = c("Checklists"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Checklists - Districts"), 
            popup = leafpop::popupTable(effortmap,c("District","Checklists",
                                                    "Participants","Locations","Species"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,10,50,100,250,1000,max(na.omit(effortmap$Checklists)))), alpha.regions = 0.6)
mapshot(a, "GBBC_districts_2022.html")


######################################### Plot state map with info

statemap1 = gBuffer(statemap1, byid=TRUE, width=0)
finalmap = statemap1

temp = data %>% 
  filter(CATEGORY %in% c("species","issf")) %>%
  filter(is.na(EXOTIC.CODE)) %>%
  group_by(ST_NM) %>%
  summarize(Species = n_distinct(COMMON.NAME))

temp1 = data %>% 
  group_by(ST_NM) %>%
  summarize(Checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

temp2 = data %>% 
  group_by(ST_NM) %>%
  summarize(Participants = n_distinct(OBSERVER.ID))

temp3 = data %>% 
  group_by(ST_NM) %>%
  summarize(Locations = n_distinct(LOCALITY.ID))

temp = left_join(temp,temp1)
temp = left_join(temp,temp2)
temp = left_join(temp,temp3)


temp$ST_NM = as.character(temp$ST_NM)
temp$GBBCweb = '<a href = "https://ebird.org/gbbc/region/IN/regions?yr=EBIRD_GBBC_2022&m="> Click on the State for more information </a>'

effortmap = merge(finalmap,temp, by =  "ST_NM")
names(effortmap)[1] = c("State")



proj4string(finalmap) = "+proj=longlat +datum=WGS84"
proj4string(effortmap) = "+proj=longlat +datum=WGS84"

mapviewOptions(fgb = FALSE)
b = mapView(effortmap, zcol = c("Checklists"), map.types = c("Esri.WorldImagery"),
            layer.name = c("Checklists - States"), 
            popup = leafpop::popupTable(effortmap,c("State","Checklists",
                                                    "Participants","Locations","Species"), 
                                        feature.id=FALSE, 
                                        row.numbers=FALSE),
            at = rev(c(0,20,100,200,500,1000,3000,5000,max(na.omit(effortmap$Checklists)))), alpha.regions = 0.6)
mapshot(b, "GBBC_states_2022.html")

c = a + b

mapshot(c, "GBBC_states_districts_2022.html")



############################ Common species

north = c("PUNJAB","HARYANA","UTTAR PRADESH","DELHI","BIHAR","CHANDIGARH")

west = c("GUJARAT","RAJASTHAN","DADRA & NAGAR HAVE","DAMAN & DIU")

himalayas = c("JAMMU & KASHMIR","LADAKH","UTTARAKHAND","HIMACHAL PRADESH")

central = c("MADHYA PRADESH","CHHATTISGARH","MAHARASHTRA","JHARKHAND","ODISHA")

south = c("ANDHRA PRADESH","TELANGANA","KARNATAKA","KERALA","TAMIL NADU","GOA","PUDUCHERRY")

east = c("ARUNACHAL PRADESH","NAGALAND","MANIPUR","TRIPURA","MIZORAM","SIKKIM","WEST BENGAL","ASSAM","MEGHALAYA")

an = c("ANDAMAN & NICOBAR","LAKSHADWEEP")

data2 = data %>%
  filter(!is.na(ST_NM)) %>%
  mutate(region = "") %>%
  mutate(region = ifelse(ST_NM %in% north, "North", region)) %>%
  mutate(region = ifelse(ST_NM %in% west, "West", region)) %>%
  mutate(region = ifelse(ST_NM %in% himalayas, "Himalayas", region)) %>%
  mutate(region = ifelse(ST_NM %in% central, "Central", region)) %>%
  mutate(region = ifelse(ST_NM %in% south, "South", region)) %>%
  mutate(region = ifelse(ST_NM %in% east, "East", region)) %>%
  mutate(region = ifelse(ST_NM %in% an, "A&N", region)) %>%
  filter(region != "")

specs = data2 %>%
  filter(CATEGORY %in% c("species","issf")) %>%
  filter(is.na(EXOTIC.CODE)) %>%
  group_by(region) %>% summarize(nspecies = n_distinct(COMMON.NAME))

lists = data2 %>%
  group_by(region) %>% summarize(checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

regionsummary = left_join(lists,specs)

write.csv(regionsummary,"GBBC_REGIONsummary_2022.csv",row.names = F)

cosp = data2 %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(DISTRICT) %>% mutate(distlist = n_distinct(group.id)) %>% ungroup %>%
  filter(distlist > 10) %>%
  group_by(COMMON.NAME,region,DISTRICT) %>% summarize(freq = n_distinct(group.id)/max(distlist)) %>% ungroup %>%
  group_by(region) %>% mutate(ndist = n_distinct(DISTRICT)) %>% ungroup %>%
  group_by(COMMON.NAME,region) %>% summarize(freq = sum(freq)/max(ndist)) %>% ungroup %>%
  group_by(region) %>% arrange(desc(freq), .by_group = T) %>% ungroup %>%
  group_by(region) %>% slice(1:5)

write.csv(cosp,"GBBC_CommonSpeciesbyRegion_2022.csv",row.names = F)


################################### plot points on map


data4 = data %>% distinct(LOCALITY.ID,LATITUDE,LONGITUDE)

ggp = ggplot() +
  geom_polygon(data = statemap1, aes(x=long, y=lat, group=group), colour = "white", fill = "black")+  
  geom_point(data = data4, aes(x=LONGITUDE,y=LATITUDE), colour = "#fcfa53", size = 1) +
  #scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0,0)) +
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        #panel.border = element_blank(),
        plot.background = element_rect(fill = "black",colour = NA),
        panel.background = element_rect(fill = "black", colour = NA),
        plot.title = element_text(hjust = 0.5))+
  coord_map()

n1 = "GBBCpointmap2022.jpg"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11)





################################################## plot region map

require(ggthemes)
theme_set(theme_tufte())

fstatemap = fortify(statemap1, region = c("ST_NM"))
data5 = data2 %>% distinct(ST_NM,region)
extra = data5[1:2,]
extra$ST_NM = c("DAMAN & DIU","LAKSHADWEEP")
extra$region = c("West","A&N")
data5 = rbind(data5,extra)
fstatemap = left_join(fstatemap,data5,by = c('id' = "ST_NM"))
fstatemap = fstatemap %>% filter(!is.na(region))

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

ns = 7

cols1 = cols[c(1:ns)]
bks1 = c("A&N","Central","East","Himalayas","North","South","West")

plotindiamap = ggplot() +
  #geom_polygon(data = indiamap, aes(x=long, y=lat, group=group), colour = NA, fill = NA)+  
  geom_polygon(data = fstatemap, aes(x=long, y=lat,group=group,fill=region,colour=region))+  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  scale_fill_manual(breaks = bks1, values = cols1)+
  scale_colour_manual(breaks = bks1, values = cols1)+
  coord_map()

ggp = plotindiamap +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12))

n1 = "GBBCregionmap_2022.jpg"

print(ggp)
ggsave(file=n1, units="in", width=10, height=7)


########### overall stats

participants = length(unique(data$OBSERVER.ID))
checklists = length(unique(data$SAMPLING.EVENT.IDENTIFIER))
uniquechecklists = length(unique(data$group.id))   
temp = data %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(is.na(EXOTIC.CODE)) %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)
species = length(unique(temp$COMMON.NAME))



################################ year summaries

load("pastGBBCdata1.RData")
#pastdata = add_column(pastdata, EXOTIC.CODE = NA, .before = 16)
#dataall = rbind(pastdata,data)
#pastdata = dataall
#save(pastdata,file = "pastGBBCdata1.RData")
dataall = pastdata

overyears1 = dataall %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>%
  group_by(cyear) %>% summarize(num = sum(na.omit(DURATION.MINUTES/60)))
overyears1$type = "Person hours"

overyears2 = dataall %>%
  group_by(cyear) %>% summarize(num = n_distinct(SAMPLING.EVENT.IDENTIFIER))
overyears2$type = "Total checklists"

overyears3 = dataall %>%
  group_by(cyear) %>% summarize(num = n_distinct(group.id))
overyears3$type = "Unique checklists"

overyears4 = dataall %>%
  group_by(cyear) %>% summarize(num = n_distinct(OBSERVER.ID))
overyears4$type = "Participants"

overyears5 = dataall %>%
  group_by(cyear) %>% summarize(num = n_distinct(DISTRICT))
overyears5$type = "Districts"

overyears6 = dataall %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(is.na(EXOTIC.CODE)) %>%
  group_by(cyear) %>% summarize(num = n_distinct(COMMON.NAME))
overyears6$type = "Species"


overyears = rbind(overyears1,overyears2,overyears3,overyears4,overyears5,overyears6)


cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

is.extrafont.installed <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
    To enable full font support, run: 
      install.packages('extrafont') 
      font_import()")
    return(F)
  }
}

base_font_family_tufte <- function(){
  if(is.extrafont.installed()){
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
  }else{
    tuftefont <- "serif"
  }
  return(tuftefont)
}

theme_tufte_revised <- function(base_size = 11, base_family = base_font_family_tufte(), ticks = TRUE) {
  
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(vjust = -0.3), 
      axis.title.y = element_text(vjust = 0.8),
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.title = element_text(face="plain"),
      panel.background = element_blank(), 
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank()
    )
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  
  ret
} 

require(extrafont)

pd = position_dodge(0.2)

ggp = ggplot(data = overyears[overyears$type %in% c("Person hours","Total checklists","Unique checklists"),], 
             aes(x = cyear, y = num, col = type)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  xlab("Years") +
  ylab("")+
  theme_tufte_revised()

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        legend.position = "bottom") +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c("Unique checklists","Total checklists","Person hours"), 
                      values = cols[c(1:3)]) +
  scale_x_continuous(breaks = c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)) +
  scale_y_continuous(breaks = c(0,6000,12000,18000,24000,30000,36000,42000), 
                     labels = c("0","6,000","12,000","18,000","24,000","30,000","36,000","42,000"),
                     limits = c(0,43000))

ggp = ggplot(data = overyears[overyears$type %in% c("Participants"),], 
             aes(x = cyear, y = num, col = type)) +
  geom_point(size = 3, position = pd) +
  geom_line(size = 1, position = pd) +
  xlab("Years") +
  ylab("")+
  theme_tufte_revised()

ggp2 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        legend.position = "bottom") +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c("Participants"), 
                      values = cols[c(1)]) +
  scale_x_continuous(breaks = c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)) +
  scale_y_continuous(breaks = seq(0,4000,500), labels = c("0","500","1000","1500","2000","2,500","3,000","3,500","4,000"))

ggp = ggplot(data = overyears[overyears$type %in% c("Species"),], 
             aes(x = cyear, y = num, col = type)) +
  geom_point(size = 3, position = pd) +
  geom_line(size = 1, position = pd) +
  geom_hline(yintercept = 1000, linetype = "dotted") +
  xlab("Years") +
  ylab("")+
  theme_tufte_revised()

ggp3 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        legend.position = "bottom") +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c("Species"), 
                      values = cols[c(1)]) +
  scale_x_continuous(breaks = c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)) +
  scale_y_continuous(breaks = seq(600,1000,100), labels = c("600","700","800","900","1,000"),
                     limits = c(580,1050))

ggp = ggplot(data = overyears[overyears$type %in% c("Districts"),], 
             aes(x = cyear, y = num, col = type)) +
  geom_point(size = 3, position = pd) +
  geom_line(size = 1, position = pd) +
  xlab("Years") +
  ylab("")+
  theme_tufte_revised()

ggp4 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        legend.position = "bottom") +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c("Districts"), 
                      values = cols[c(1)]) +
  scale_x_continuous(breaks = c(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)) +
  scale_y_continuous(breaks = seq(100,500,50), labels = c("100","150","200","250","300","350","400","450","500"),
                     limits = c(100,500))



freqs = dataall %>%
  filter(year > 2015) %>%
  group_by(DISTRICT) %>% mutate(distlist = n_distinct(group.id)) %>% ungroup %>%
  filter(distlist > 10) %>%
  group_by(cyear,COMMON.NAME,DISTRICT) %>% summarize(freq = n_distinct(group.id)/max(distlist)) %>% ungroup %>%
  group_by(cyear) %>% mutate(ndist = n_distinct(DISTRICT)) %>% ungroup %>%
  group_by(COMMON.NAME,cyear) %>% summarize(freq = sum(freq)/max(ndist)) %>% ungroup 

ggp = ggplot(data = freqs[freqs$COMMON.NAME %in% c("Common Myna","Rock Pigeon","Red-vented Bulbul"),], 
             aes(x = year, y = freq*100, col = COMMON.NAME)) +
  geom_point(size = 3, position = pd) +
  geom_line(size = 1, position = pd) +
  xlab("Years") +
  ylab("Frequency (%)")+
  theme_tufte_revised()

ggp5 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12), 
        legend.position = "bottom") +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c("Common Myna","Rock Pigeon","Red-vented Bulbul"), 
                      values = cols[c(1:5)]) +
  scale_x_continuous(breaks = c(2016,2017,2018,2019,2020,2021,2022))


print(ggp1)
ggsave(file="effort2022.jpg", units="in", width=10, height=7)

print(ggp2)
ggsave(file="participation2022.jpg", units="in", width=10, height=7)

print(ggp3)
ggsave(file="species2022.jpg", units="in", width=10, height=7)

print(ggp4)
ggsave(file="spread2022.jpg", units="in", width=10, height=7)



####### Campus bird count


campus = read.csv("2022campushotspots.csv")
names(campus)[1] = "LOCALITY.ID"
names(campus)[2] = "campus"

datafinal = left_join(data,campus)
datafinal = datafinal %>% filter(!is.na(campus))

###### campus statistics

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
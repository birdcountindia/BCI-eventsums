# adapted from https://github.com/ashwinv2005/repeated-eBirding-event-analyses/blob/master/HBC%202022.R

require(lubridate)
require(tidyverse)
require(sf)
require(mapview)
require(leaflet)
require(writexl)
require(ggthemes)
require(osmdata)


# paths
cur_outpath <- glue("outputs/{cur_event$SHORT.CODE}/{cur_event$EDITION}/")
if (!dir.exists(cur_outpath)) (dir.create(cur_outpath, recursive = T))

source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/summaries.R")
source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/mapping.R")


# loading BT and NP data --------------------------------------------------

if (cur_event$SHORT.CODE == "HBC"){
  
  bhu_zippath <-  glue("../ebird-datasets/EBD/ebd_BT_{rel_year}{str_pad(rel_month_num, width=2, pad='0')}_{rel_year}{str_pad(rel_month_num, width=2, pad='0')}_rel{rel_month_lab}-{rel_year}.zip")
  nep_zippath <-  glue("../ebird-datasets/EBD/ebd_NP_{rel_year}{str_pad(rel_month_num, width=2, pad='0')}_{rel_year}{str_pad(rel_month_num, width=2, pad='0')}_rel{rel_month_lab}-{rel_year}.zip")
  bhu_rawfile <-  glue("ebd_BT_{rel_year}{str_pad(rel_month_num, width=2, pad='0')}_{rel_year}{str_pad(rel_month_num, width=2, pad='0')}_rel{rel_month_lab}-{rel_year}.txt")
  nep_rawfile <-  glue("ebd_NP_{rel_year}{str_pad(rel_month_num, width=2, pad='0')}_{rel_year}{str_pad(rel_month_num, width=2, pad='0')}_rel{rel_month_lab}-{rel_year}.txt")
  bhu_rawpath <-  glue("../ebird-datasets/EBD/{bhu_rawfile}.txt")
  nep_rawpath <-  glue("../ebird-datasets/EBD/{nep_rawfile}.txt")
  
  preimp <- c("CATEGORY","EXOTIC.CODE","COMMON.NAME","OBSERVATION.COUNT",
              "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","LAST.EDITED.DATE",
              "STATE","STATE.CODE","COUNTY","COUNTY.CODE",
              "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
              "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","BREEDING.CODE",
              "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER",
              "TRIP.COMMENTS","SPECIES.COMMENTS", "HAS.MEDIA")
  
  
  # unzipping EBD downloads (if not done already) 
  
  if (!file.exists(bhu_rawpath) & file.exists(bhu_zippath)) {
    unzip(zipfile = bhu_zippath, files = bhu_rawfile, exdir = "../ebird-datasets/EBD") # don't add trailing slash in path
    print("Bhutan data download unzipped.")
  } else if (!file.exists(rawpath) & !file.exists(zippath)) {
    print("Latest Bhutan data download does not exist!")
  } else {
    print("Bhutan data download already unzipped.")
  }
  
  if (!file.exists(nep_rawpath) & file.exists(nep_zippath)) {
    unzip(zipfile = nep_zippath, files = nep_rawfile, exdir = "../ebird-datasets/EBD") # don't add trailing slash in path
    print("Nepal data download unzipped.")
  } else if (!file.exists(rawpath) & !file.exists(zippath)) {
    print("Latest Nepal data download does not exist!")
  } else {
    print("Nepal data download already unzipped.")
  }
  
  
  # main data processing steps 
  
  nms <- names(read.delim(bhu_rawpath, nrows = 1, sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ", NA)))
  nms[!(nms %in% preimp)] <- "NULL"
  nms[nms %in% preimp] <- NA
  
  data_BT <- read.delim(bhu_rawpath, colClasses = nms, sep = "\t", header = T, quote = "",
                        stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
    mutate(COUNTRY == "Bhutan") %>% 
    mutate(BREEDING.CODE = str_trim(BREEDING.CODE)) %>% 
    # group ID and dates
    mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER), 
           OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
           YEAR = year(OBSERVATION.DATE), 
           MONTH = month(OBSERVATION.DATE),
           DAY.M = day(OBSERVATION.DATE)) %>% 
    # migratory year and month information
    mutate(M.YEAR = if_else(MONTH > 5, YEAR, YEAR-1), # from June to May
           M.MONTH = if_else(MONTH > 5, MONTH-5, 12-(5-MONTH))) %>% 
    # joining user details
    left_join(eBird_users, by = "OBSERVER.ID")
  
  data_NP <- read.delim(nep_rawpath, colClasses = nms, sep = "\t", header = T, quote = "",
                        stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
    mutate(COUNTRY == "Nepal") %>% 
    mutate(BREEDING.CODE = str_trim(BREEDING.CODE)) %>% 
    # group ID and dates
    mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER), 
           OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
           YEAR = year(OBSERVATION.DATE), 
           MONTH = month(OBSERVATION.DATE),
           DAY.M = day(OBSERVATION.DATE)) %>% 
    # migratory year and month information
    mutate(M.YEAR = if_else(MONTH > 5, YEAR, YEAR-1), # from June to May
           M.MONTH = if_else(MONTH > 5, MONTH-5, 12-(5-MONTH))) %>% 
    # joining user details
    left_join(eBird_users, by = "OBSERVER.ID")
  
  
  # loading maps
  load("../india-maps/outputs/maps_BT_NP_sf.RData")

}


# preparing data ----------------------------------------------------------

# filtering for loc & dates
if (cur_event$SHORT.CODE == "HBC"){
  
  bt_np <- bind_rows(data_BT, data_NP) %>% 
    join_BT_NP_sf()

  
  data0 <- data %>% 
    filter(OBSERVATION.DATE %in% seq(cur_event$START.DATE, cur_event$END.DATE, 
                                     by = "days")) %>% 
    mutate(COUNTRY = "India") 
  
  # Joining mapvars to data
  sf_use_s2(FALSE)
  data0 <- join_map_sf(data0)
  
  
  # combining all countries
  data0 <- data0 %>% bind_rows(bt_np)

  
  # filtering for EBD and HBC separately 
  cur_dists_sf_EBD <- dists_sf
  cur_states_sf_EBD <- states_sf
  
  data0_EBD <- data0 %>% filter(COUNTRY == "India")
  
  
  cur_dists_sf_HBC <- dists_sf %>%
    filter(DISTRICT.NAME %in% c("Darjeeling","Kalimpong","Alipurduar","Hoshiarpur","Rupnagar",
                                "Pilibhit","Lakhimpur Kheri","Kokrajhar","Chirang","Baksa",
                                "Sonitpur","Dhemaji","Lakhimpur","Panchkula","Udalguri","Biswanath",
                                "Kheri","Pathankot","Una","Sahibzada Ajit Singh Nagar","Saharanpur",
                                "Yamunanagar","Bahraich","Shrawasti","Balrampur","Pashchim Champaran",
                                "Purba Champaran")) %>% 
    bind_rows(bt_dists_sf, np_dists_sf)
  cur_states_sf_HBC <- states_sf %>%
    filter(STATE.NAME %in% c("Ladakh","Jammu and Kashmir","Himachal Pradesh","Uttarakhand",
                             "Sikkim","Arunachal Pradesh","Bhutan","Nepal")) %>% 
    bind_rows(bt_states_sf, np_states_sf)
  
  data0_HBC <- data0 %>%
    filter(STATE.NAME %in% cur_states_sf_HBC$STATE.NAME | 
             DISTRICT.NAME %in% cur_dists_sf_HBC$DISTRICT.NAME & 
             (STATE.NAME != "Uttar Pradesh" | DISTRICT.NAME != "Hamirpur") &
             (STATE.NAME != "Chhattisgarh"))
  
  
  regions_EBD <- cur_states_sf_EBD %>% 
    st_drop_geometry() %>% 
    dplyr::select(STATE.NAME) %>% 
    mutate(REGION1 = case_when(STATE.NAME %in% c("Punjab", "Haryana", "Uttar Pradesh", 
                                                 "Delhi", "Bihar", "Chandigarh")
                               ~ "North",
                               STATE.NAME %in% c("Gujarat","Rajasthan",
                                                 "Dadra and Nagar Haveli","Daman and Diu")
                               ~ "West",
                               STATE.NAME %in% c("Jammu and Kashmir", "Ladakh", 
                                                 "Uttarakhand", "Himachal Pradesh")
                               ~ "Himalaya",
                               STATE.NAME %in% c("Madhya Pradesh", "Chhattisgarh", 
                                                 "Maharashtra", "Jharkhand", "Odisha")
                               ~ "Central",
                               STATE.NAME %in% c("Andhra Pradesh", "Telangana", "Karnataka",
                                                 "Kerala", "Tamil Nadu", "Goa", "Puducherry")
                               ~ "South",
                               STATE.NAME %in% c("Arunachal Pradesh", "Nagaland", "Manipur",
                                                 "Tripura", "Mizoram", "Sikkim", 
                                                 "West Bengal", "Assam", "Meghalaya")
                               ~ "East",
                               STATE.NAME %in% c("Andaman and Nicobar Islands", 
                                                 "Lakshadweep")
                               ~ "A&N")) %>% 
    mutate(REGION1 = factor(REGION1, 
                            levels = c("A&N", "Central", "East", "Himalaya", 
                                       "North", "South", "West")))
  
  no_regions_EBD <- n_distinct(regions_EBD$REGION1)
  
  regions_HBC <- cur_states_sf_HBC %>% 
    st_drop_geometry() %>% 
    dplyr::select(STATE.NAME) %>% 
    mutate(REGION1 = case_when(STATE.NAME %in% c("Jammu and Kashmir", "Ladakh", 
                                                 "Himachal Pradesh", "Punjab", "Haryana")
                               ~ "Western Region",
                               STATE.NAME %in% c("Uttarakhand", "Uttar Pradesh", 
                                                 "Nepal", "Bihar")
                               ~ "Central Region",
                               STATE.NAME %in% c("Arunachal Pradesh", "Sikkim",
                                                 "West Bengal", "Assam", "Bhutan")
                               ~ "Eastern Region")) %>% 
    mutate(REGION1 = factor(REGION1, 
                            levels = c("Western Region", "Central Region", "Eastern Region")))
  
  no_regions_HBC <- n_distinct(regions_HBC$REGION1)
  
} 

# sf for regions
regions_sf_EBD <- regions_EBD %>% 
  left_join(cur_states_sf_EBD %>% dplyr::select(-AREA)) %>% 
  st_as_sf() %>%
  st_make_valid() %>% # otherwise below results in TopologyException error
  # joins multiple polygons into one for each region
  group_by(REGION1) %>% 
  dplyr::summarise()

regions_sf_HBC <- regions_HBC %>% 
  left_join(cur_states_sf_HBC %>% dplyr::select(-AREA)) %>% 
  st_as_sf() %>%
  st_make_valid() %>% # otherwise below results in TopologyException error
  # joins multiple polygons into one for each region
  group_by(REGION1) %>% 
  dplyr::summarise()

# adding regions to data
data0_EBD <- data0_EBD %>% left_join(regions_EBD)
data0_HBC <- data0_HBC %>% left_join(regions_HBC)


# create and write a file with common and scientific names of all species
# useful for mapping
temp_EBD <- data0_EBD %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(!EXOTIC.CODE %in% c("X")) %>%
  distinct(COMMON.NAME)

write.csv(temp_EBD, row.names = FALSE, 
          file = glue("{cur_outpath}{cur_event$FULL.CODE}_speclist_EBD.csv"))
rm(temp_EBD)

temp_HBC <- data0_HBC %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(!EXOTIC.CODE %in% c("X")) %>%
  distinct(COMMON.NAME)

write.csv(temp_HBC, row.names = FALSE, 
          file = glue("{cur_outpath}{cur_event$FULL.CODE}_speclist_HBC.csv"))
rm(temp_HBC)


# stats -----------------------------------------------------------

# overall stats
overall_stats <- basic_stats(data0)

# top 30 checklist uploaders (eligible list filter)
top30 <- data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(OBSERVER.ID, FULL.NAME) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  ungroup() %>%
  arrange(desc(NO.LISTS)) %>% 
  slice(1:30)

# top birders per state
top_state <- data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  filter(!is.na(STATE)) %>% 
  group_by(STATE.NAME, OBSERVER.ID, FULL.NAME) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  ungroup() %>% 
  complete(STATE.NAME = cur_states_sf$STATE.NAME, 
           fill = list(NO.LISTS = 0)) %>% 
  group_by(STATE.NAME) %>%
  arrange(desc(NO.LISTS)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(desc(NO.LISTS)) 

# list of birders per state
birder_state <- data0 %>%
  # some lists in eBird data have no state name
  filter(!is.na(STATE)) %>% 
  distinct(STATE.NAME, OBSERVER.ID) %>%
  # joining names
  left_join(eBird_users) %>% 
  arrange(STATE.NAME)

# district-wise summary
dist_sum <- data0 %>%
  # some lists in eBird data have no state name
  filter(!is.na(STATE)) %>% 
  group_by(DISTRICT.NAME) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.BIRDERS = n_distinct(OBSERVER.ID)) %>%
  complete(DISTRICT.NAME = cur_dists_sf$DISTRICT.NAME, 
           fill = list(NO.LISTS = 0,
                       NO.BIRDERS = 0)) %>% 
  arrange(desc(NO.LISTS))

# state-wise summary
state_sum <- data0 %>%
  # some lists in eBird data have no state name
  filter(!is.na(STATE)) %>% 
  group_by(STATE.NAME) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.BIRDERS = n_distinct(OBSERVER.ID)) %>%
  complete(STATE.NAME = cur_states_sf$STATE.NAME, 
           fill = list(NO.LISTS = 0,
                       NO.BIRDERS = 0)) %>% 
  arrange(desc(NO.LISTS))

# day-wise summary
day_sum <- data0 %>%
  group_by(DAY.M) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.BIRDERS = n_distinct(OBSERVER.ID)) %>%
  arrange(desc(NO.LISTS))

# state-day-wise summary
state_day_sum <- data0 %>%
  # some lists in eBird data have no state name
  filter(!is.na(STATE)) %>% 
  group_by(STATE.NAME, DAY.M) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.BIRDERS = n_distinct(OBSERVER.ID)) %>%
  ungroup() %>% 
  complete(STATE.NAME = cur_states_sf$STATE.NAME, 
           DAY.M = seq(cur_event$START.DATE, cur_event$END.DATE, by = "days") %>% mday(),
           fill = list(NO.LISTS = 0,
                       NO.BIRDERS = 0)) %>% 
  arrange(STATE.NAME, DAY.M)


write_xlsx(x = list("Overall stats" = overall_stats, 
                    "Top 30 checklist uploaders" = top30, 
                    "Top birders per state" = top_state,
                    "Birders per state" = birder_state,
                    "Summary per district" = dist_sum,
                    "Summary per state" = state_sum,
                    "Summary per day" = day_sum,
                    "Summary per state-day" = state_day_sum),
           path = glue("{cur_outpath}{cur_event$FULL.CODE}_stats.xlsx"))


# regional summaries and common species -----------------------------------

reg_stats <- data0 %>% 
  filter(!is.na(REGION1)) %>% 
  group_by(REGION1) %>% 
  basic_stats(prettify = F) %>% 
  # keeping only necessary
  dplyr::select(SPECIES, LISTS.ALL) %>% 
  ungroup() 


com_spec <- data0 %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  # taking only districts with sufficient (10) lists to calculate REPFREQ
  group_by(DISTRICT.NAME) %>% 
  mutate(LISTS.D = n_distinct(GROUP.ID)) %>% 
  ungroup() %>%
  filter(LISTS.D > 10) %>%
  # repfreq
  group_by(COMMON.NAME, REGION1, DISTRICT.NAME) %>% 
  summarise(REP.FREQ = 100*n_distinct(GROUP.ID)/max(LISTS.D)) %>% 
  # averaging repfreq across different districts in region
  group_by(REGION1) %>% 
  mutate(NO.DIST = n_distinct(DISTRICT.NAME)) %>% 
  group_by(COMMON.NAME, REGION1) %>% 
  summarise(REP.FREQ = sum(REP.FREQ)/max(NO.DIST)) %>% 
  group_by(REGION1) %>% 
  # top 5 per region
  arrange(desc(REP.FREQ), .by_group = T) %>% 
  slice(1:5) %>% 
  ungroup()


write_xlsx(x = list("Stats" = reg_stats, 
                    "Common species" = com_spec),
           path = glue("{cur_outpath}{cur_event$FULL.CODE}_regions.xlsx"))


# plot districtwise stats on map ----------------------------------------------------

dist_stats <- data0 %>% 
  group_by(DISTRICT.NAME) %>% 
  basic_stats(pipeline = T, prettify = F) %>% 
  # function retains grouping
  ungroup() %>% 
  # keeping only necessary
  dplyr::select(DISTRICT.NAME, SPECIES, LISTS.ALL, PARTICIPANTS, LOCATIONS) %>% 
  complete(DISTRICT.NAME = cur_dists_sf$DISTRICT.NAME, 
           fill = list(SPECIES = 0,
                       LISTS.ALL = 0,
                       PARTICIPANTS = 0,
                       LOCATIONS = 0)) %>% 
  magrittr::set_colnames(c("District", "Species", "Total checklists", "Participants",
                           "Locations")) %>% 
  right_join(cur_dists_sf %>% dplyr::select(-AREA),
             by = c("District" = "DISTRICT.NAME")) %>% 
  st_as_sf()


# different breakpoints in visualisation

max_lists <- max(na.omit(dist_stats$`Total checklists`))
break_at <- if (max_lists %in% 500:1000) {
  rev(c(0, 30, 100, 200, 400, max_lists))
} else if (max_lists %in% 1000:2000) {
  rev(c(0, 10, 50, 100, 250, 1000, max_lists))
} else if (max_lists %in% 2000:4000) {
  rev(c(0, 30, 80, 200, 500, 1000, 2000, max_lists))
} else if (max_lists %in% 4000:8000) {
  rev(c(0, 30, 100, 200, 500, 1000, 2000, 4000, max_lists))
} else if (max_lists > 8000) {
  rev(c(0, 50, 200, 500, 1000, 3000, 6000, max_lists))
} 


mapviewOptions(fgb = FALSE)
map_effort_dist <- mapView(dist_stats, 
                           zcol = c("Total checklists"), 
                           map.types = c("Esri.WorldImagery"),
                           layer.name = c("Checklists per district"), 
                           popup = leafpop::popupTable(dist_stats,
                                                       zcol = c("District", "Total checklists", 
                                                                "Participants", "Locations", "Species"), 
                                                       feature.id = FALSE,
                                                       row.numbers = FALSE),
                           at = break_at, 
                           alpha.regions = 0.6)

# webshot::install_phantomjs()
mapshot(map_effort_dist, 
        url = glue("{cur_outpath}{cur_event$FULL.CODE}_distseffortmap.html"))


# plot statewise stats on map ----------------------------------------------------

state_stats <- data0 %>% 
  group_by(STATE.NAME) %>% 
  basic_stats(pipeline = T, prettify = F) %>% 
  # function retains grouping
  ungroup() %>% 
  # keeping only necessary
  dplyr::select(STATE.NAME, SPECIES, LISTS.ALL, PARTICIPANTS, LOCATIONS) %>% 
  complete(STATE.NAME = cur_states_sf$STATE.NAME, 
           fill = list(SPECIES = 0,
                       LISTS.ALL = 0,
                       PARTICIPANTS = 0,
                       LOCATIONS = 0)) %>% 
  magrittr::set_colnames(c("State", "Species", "Total checklists", "Participants",
                           "Locations")) %>% 
  right_join(cur_states_sf %>% dplyr::select(-AREA),
             by = c("State" = "STATE.NAME")) %>% 
  st_as_sf()


# different breakpoints in visualisation

max_lists <- max(na.omit(state_stats$`Total checklists`))
break_at <- if (max_lists %in% 500:1000) {
  rev(c(0, 30, 100, 200, 400, max_lists))
} else if (max_lists %in% 1000:2000) {
  rev(c(0, 10, 50, 100, 250, 1000, max_lists))
} else if (max_lists %in% 2000:4000) {
  rev(c(0, 30, 80, 200, 500, 1000, 2000, max_lists))
} else if (max_lists %in% 4000:8000) {
  rev(c(0, 30, 100, 200, 500, 1000, 2000, 4000, max_lists))
} else if (max_lists > 8000) {
  rev(c(0, 50, 200, 500, 1000, 3000, 6000, max_lists))
} 


mapviewOptions(fgb = FALSE)
map_effort_state <- mapView(state_stats, 
                            zcol = c("Total checklists"), 
                            map.types = c("Esri.WorldImagery"),
                            layer.name = c("Checklists per state"), 
                            popup = leafpop::popupTable(state_stats,
                                                        zcol = c("State", "Total checklists", 
                                                                 "Participants", "Locations", "Species"), 
                                                        feature.id = FALSE,
                                                        row.numbers = FALSE),
                            at = break_at, 
                            alpha.regions = 0.6)

# webshot::install_phantomjs()
mapshot(map_effort_state, 
        url = glue("{cur_outpath}{cur_event$FULL.CODE}_stateseffortmap.html"))


# plot point map ----------------------------------------------------------

point_map <- data0 %>% 
  distinct(LOCALITY.ID, LATITUDE, LONGITUDE) %>% 
  cov_point_map_plain(., poly_sf = cur_states_sf, 
                      poly_bound_col = "white",
                      point_size = 1, point_alpha = 0.3)

ggsave(filename = glue("{cur_outpath}{cur_event$FULL.CODE}_pointmap.png"), 
       plot = point_map, 
       device = png, units = "in", width = 8, height = 8, bg = "black", dpi = 300)


# plot region map ---------------------------------------------------------

theme_set(theme_tufte())


palette_vals <- palette[1:no_regions]


region_map <- regions_sf %>% 
  ggplot() +
  geom_sf(aes(fill = REGION1, geometry = STATE.GEOM), colour = NA) +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.background = element_blank(),
        legend.title = element_blank(), 
        legend.text = element_text(size = 12)) +
  scale_fill_manual(values = palette_vals) +
  scale_colour_manual(values = palette_vals) 

ggsave(filename = glue("{cur_outpath}{cur_event$FULL.CODE}_regionmap.png"), 
       plot = region_map, 
       device = png, units = "in", width = 10, height = 7, dpi = 300)


# comparison with prev: yearly summaries --------------------------------------------------------

over_time <- data_all %>% 
  group_by(YEAR) %>% 
  basic_stats(pipeline = T, prettify = T)


data1 <- over_time %>%
  # keeping only necessary
  dplyr::select(YEAR, `person hours`, `lists (all types)`, `unique lists`) %>% 
  magrittr::set_colnames(c("YEAR", "Person hours", "Total checklists", "Unique checklists")) %>% 
  pivot_longer(!matches("YEAR"), names_to = "STAT", values_to = "VALUES") %>% 
  ungroup()

data2 <- over_time %>% 
  # keeping only necessary
  dplyr::select(YEAR, `eBirders`) %>% 
  magrittr::set_colnames(c("YEAR", "Participants")) %>% 
  pivot_longer(!matches("YEAR"), names_to = "STAT", values_to = "VALUES") %>% 
  ungroup()

data3 <- over_time %>% 
  # keeping only necessary
  dplyr::select(YEAR, `species`) %>% 
  magrittr::set_colnames(c("YEAR", "Species")) %>% 
  pivot_longer(!matches("YEAR"), names_to = "STAT", values_to = "VALUES") %>% 
  ungroup()

# number of districts calc
data4 <- data_all %>% 
  group_by(YEAR) %>% 
  summarise(VALUES = n_distinct(DISTRICT.NAME),
            STAT = "Districts") 


require(extrafont)
pos_dodge <- position_dodge(0.2)
source("scripts/functions_plot.R")


plot_breaks <- seq(0, 56000, 8000)
plot1 <- ggplot(data1, aes(x = YEAR, y = VALUES, col = STAT)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(x = "Years", y = "") +
  theme_mod_tufte() +
  scale_colour_manual(breaks = c("Unique checklists", "Total checklists", "Person hours"), 
                      values = palette) +
  scale_x_continuous(breaks = 2013:2023) +
  scale_y_continuous(breaks = plot_breaks, 
                     labels = scales::label_comma()(plot_breaks),
                     limits = c(min(plot_breaks), max(plot_breaks + 1000)))


plot_breaks <- seq(0, 4000, 500)
plot2 <- ggplot(data2, aes(x = YEAR, y = VALUES, col = STAT)) +
  geom_point(size = 3, position = pos_dodge) +
  geom_line(size = 1, position = pos_dodge) +
  labs(x = "Years", y = "") +
  theme_mod_tufte() +
  scale_colour_manual(breaks = c("Participants"), 
                      values = palette) +
  scale_x_continuous(breaks = 2013:2023) +
  scale_y_continuous(breaks = plot_breaks, 
                     labels = scales::label_comma()(plot_breaks))


plot_breaks <- seq(600, 1100, 100)
plot3 <- ggplot(data3, aes(x = YEAR, y = VALUES, col = STAT)) +
  geom_point(size = 3, position = pos_dodge) +
  geom_line(size = 1, position = pos_dodge) +
  geom_hline(yintercept = 1000, linetype = "dotted") +
  labs(x = "Years", y = "") +
  theme_mod_tufte() +
  scale_colour_manual(breaks = c("Species"), 
                      values = palette) +
  scale_x_continuous(breaks = 2013:2023) +
  scale_y_continuous(breaks = plot_breaks, 
                     labels = scales::label_comma()(plot_breaks),
                     limits = c(580, 1100))


plot_breaks <- seq(100, 500, 50)
plot4 <- ggplot(data4, aes(x = YEAR, y = VALUES, col = STAT)) +
  geom_point(size = 3, position = pos_dodge) +
  geom_line(size = 1, position = pos_dodge) +
  labs(x = "Years", y = "") +
  theme_mod_tufte() +
  scale_colour_manual(breaks = c("Districts"), 
                      values = palette) +
  scale_x_continuous(breaks = 2013:2023) +
  scale_y_continuous(breaks = plot_breaks, 
                     # labels = scales::label_comma()(plot_breaks),
                     limits = c(100, 500))


ggsave(filename = glue("{cur_outpath}{cur_event$FULL.CODE}_overtime_effort.png"), 
       plot = plot1, 
       device = png, units = "in", width = 10, height = 7, dpi = 300)

ggsave(filename = glue("{cur_outpath}{cur_event$FULL.CODE}_overtime_participation.png"), 
       plot = plot2, 
       device = png, units = "in", width = 10, height = 7, dpi = 300)

ggsave(filename = glue("{cur_outpath}{cur_event$FULL.CODE}_overtime_species.png"), 
       plot = plot3, 
       device = png, units = "in", width = 10, height = 7, dpi = 300)

ggsave(filename = glue("{cur_outpath}{cur_event$FULL.CODE}_overtime_spread.png"), 
       plot = plot4, 
       device = png, units = "in", width = 10, height = 7, dpi = 300)

# comparison with prev: common species --------------------------------------------------------

yearly_com_spec <- data_all %>%
  filter(YEAR > 2015,
         ALL.SPECIES.REPORTED == 1) %>%
  # taking only districts with sufficient (10) lists to calculate REPFREQ
  group_by(DISTRICT.NAME) %>% 
  mutate(LISTS.D = n_distinct(GROUP.ID)) %>% 
  ungroup() %>%
  filter(LISTS.D > 10) %>%
  # repfreq
  group_by(YEAR, COMMON.NAME, DISTRICT.NAME) %>% 
  summarise(REP.FREQ = 100*n_distinct(GROUP.ID)/max(LISTS.D)) %>% 
  # averaging repfreq across different districts in region
  group_by(YEAR) %>% 
  mutate(NO.DIST = n_distinct(DISTRICT.NAME)) %>% 
  group_by(COMMON.NAME, YEAR) %>% 
  summarise(REP.FREQ = sum(REP.FREQ)/max(NO.DIST)) %>% 
  ungroup() %>% 
  filter(COMMON.NAME %in% c("Common Myna","Rock Pigeon","Red-vented Bulbul"))

plot_breaks <- seq(100, 500, 50)
plot5 <- ggplot(yearly_com_spec, aes(x = YEAR, y = REP.FREQ, col = COMMON.NAME)) +
  geom_point(size = 3, position = pos_dodge) +
  geom_line(size = 1, position = pos_dodge) +
  labs(x = "Years", y = "Frequency (%)") +
  theme_mod_tufte() +
  theme(axis.title.y = element_text(angle = 90, size = 16)) +
  scale_x_continuous(breaks = 2015:2023) +
  scale_colour_manual(breaks = c("Common Myna", "Rock Pigeon", "Red-vented Bulbul"), 
                      values = palette) 


# Campus Bird Count -------------------------------------------------------

# plot campus-wise stats on map

# most common species
mostcom <- data_campus %>% 
  group_by(COMMON.NAME, GROUP.ID) %>% 
  slice(1) %>% 
  group_by(CAMPUS) %>% 
  mutate(LISTS.C = n_distinct(GROUP.ID)) %>% 
  group_by(CAMPUS, COMMON.NAME) %>% 
  summarise(REP.FREQ = 100*n_distinct(GROUP.ID)/max(LISTS.C)) %>% 
  arrange(desc(REP.FREQ)) %>% 
  slice(1) %>% 
  distinct(CAMPUS, COMMON.NAME) %>% 
  rename(MOST.COM = COMMON.NAME)

campus_stats <- data_campus %>% 
  group_by(CAMPUS) %>% 
  basic_stats(pipeline = T, prettify = F) %>% 
  # function retains grouping
  ungroup() %>% 
  # keeping only necessary
  dplyr::select(CAMPUS, SPECIES, LISTS.ALL, LISTS.U, PARTICIPANTS) %>% 
  complete(CAMPUS = unique(data_campus$CAMPUS), 
           fill = list(SPECIES = 0,
                       LISTS.ALL = 0,
                       LISTS.U = 0,
                       PARTICIPANTS = 0)) %>% 
  left_join(mostcom) %>% 
  arrange(desc(LISTS.ALL))

write.csv(campus_stats, row.names = FALSE, 
          file = glue("{cur_outpath}{cur_event$FULL.CODE}_CBC.csv"))


campus_sf <- data_campus %>% 
  distinct(CAMPUS, LONGITUDE, LATITUDE) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"))

campus_stats <- campus_stats %>% 
  # renaming for map
  rename(Campus = CAMPUS,
         Species = SPECIES,
         `Total checklists` = LISTS.ALL,
         `Unique checklists` = LISTS.U,
         Participants = PARTICIPANTS,
         `Most common` = MOST.COM) %>% 
  right_join(campus_sf, by = c("Campus" = "CAMPUS")) %>% 
  st_as_sf()


# different breakpoints in visualisation
max_lists <- max(na.omit(campus_stats$`Total checklists`))
break_at <- if (max_lists %in% 200:300) {
  rev(c(0, 20, 50, 90, 150, max_lists))
} else if (max_lists %in% 300:500) {
  rev(c(0, 30, 80, 150, 250, max_lists))
} else if (max_lists %in% 500:1000) {
  rev(c(0, 30, 100, 200, 400, max_lists))
} else if (max_lists %in% 1000:2000) {
  rev(c(0, 10, 50, 100, 250, 1000, max_lists))
} else if (max_lists %in% 2000:4000) {
  rev(c(0, 30, 80, 200, 500, 1000, 2000, max_lists))
} else if (max_lists %in% 4000:8000) {
  rev(c(0, 30, 100, 200, 500, 1000, 2000, 4000, max_lists))
} else if (max_lists > 8000) {
  rev(c(0, 50, 200, 500, 1000, 3000, 6000, max_lists))
} 

mapviewOptions(fgb = FALSE)
map_effort <- ( # state outlines
  mapView(cur_states_sf, 
          map.types = c("Esri.WorldImagery"), 
          popup = FALSE, highlight = FALSE, legend = FALSE, 
          label = NA, alpha.regions = 0)) +
  (mapView(campus_stats, 
           zcol = c("Total checklists"), 
           map.types = c("Esri.WorldImagery"),
           layer.name = c("Checklists per campus"), 
           popup = leafpop::popupTable(campus_stats,
                                       zcol = c("Campus", "Total checklists", "Unique checklists",
                                                "Participants", "Species", "Most common"), 
                                       feature.id = FALSE,
                                       row.numbers = FALSE),
           at = break_at))

# webshot::install_phantomjs()
mapshot(map_effort, 
        url = glue("{cur_outpath}{cur_event$FULL.CODE}_CBC.html"))

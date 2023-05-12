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
  
  bhu_zippath <-  glue("../ebird-datasets/EBD/ebd_BT_rel{rel_month_lab}-{rel_year}.zip")
  nep_zippath <-  glue("../ebird-datasets/EBD/ebd_NP_rel{rel_month_lab}-{rel_year}.zip")
  bhu_rawfile <-  glue("ebd_BT_rel{rel_month_lab}-{rel_year}.txt")
  nep_rawfile <-  glue("ebd_NP_rel{rel_month_lab}-{rel_year}.txt")
  bhu_rawpath <-  glue("../ebird-datasets/EBD/{bhu_rawfile}")
  nep_rawpath <-  glue("../ebird-datasets/EBD/{nep_rawfile}")
  
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
  } else if (!file.exists(bhu_rawpath) & !file.exists(bhu_zippath)) {
    print("Latest Bhutan data download does not exist!")
  } else {
    print("Bhutan data download already unzipped.")
  }
  
  if (!file.exists(nep_rawpath) & file.exists(nep_zippath)) {
    unzip(zipfile = nep_zippath, files = nep_rawfile, exdir = "../ebird-datasets/EBD") # don't add trailing slash in path
    print("Nepal data download unzipped.")
  } else if (!file.exists(nep_rawpath) & !file.exists(nep_zippath)) {
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
    mutate(COUNTRY = "Bhutan") %>% 
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
    mutate(COUNTRY = "Nepal") %>% 
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

data0 <- data %>% 
  # filter(OBSERVATION.DATE %in% rel_date) 
  filter(OBSERVATION.DATE %in% seq(cur_event$START.DATE, cur_event$END.DATE, 
                                   by = "days"))

# previous years' data
sched_event <- sched0 %>% filter(SHORT.CODE == cur_event$SHORT.CODE)

data_all <- data %>% 
  filter(YEAR %in% sched_event$EDITION) %>% 
  group_by(YEAR) %>% 
  left_join(sched_event %>% 
              dplyr::select(EDITION, START.DATE, END.DATE), 
            by = c("YEAR" = "EDITION")) %>% 
  filter(OBSERVATION.DATE >= START.DATE & OBSERVATION.DATE <= END.DATE) %>% 
  # filter(OBSERVATION.DATE == rel_date) %>% 
  ungroup() %>% 
  mutate(START.DATE = NULL, END.DATE = NULL)


if (cur_event$SHORT.CODE == "HBC"){
  
  bt_np <- bind_rows(data_BT, data_NP) %>% 
    filter(OBSERVATION.DATE %in% seq(cur_event$START.DATE, cur_event$END.DATE,
                                     by = "days")) %>%
    # filter(OBSERVATION.DATE %in% rel_date) %>% 
    join_BT_NP_sf()
  
  bt_np_all <- bind_rows(data_BT, data_NP) %>% 
    filter(YEAR %in% sched_event$EDITION) %>% 
    group_by(YEAR) %>% 
    left_join(sched_event %>% 
                dplyr::select(EDITION, START.DATE, END.DATE), 
              by = c("YEAR" = "EDITION")) %>% 
    filter(OBSERVATION.DATE >= START.DATE & OBSERVATION.DATE <= END.DATE) %>% 
    ungroup() %>% 
    mutate(START.DATE = NULL, END.DATE = NULL)

  
  data0 <- data0 %>% mutate(COUNTRY = "India") 
  data_all <- data_all %>% mutate(COUNTRY = "India") 
  
  
  # Joining mapvars to data
  sf_use_s2(FALSE)
  data0 <- join_map_sf(data0)
  data_all <- join_map_sf(data_all)
  
  
  cur_dists_sf <- dists_sf %>%
    dplyr::select(-AREA) %>% 
    filter(DISTRICT.NAME %in% c("Darjeeling","Kalimpong","Alipurduar","Hoshiarpur","Rupnagar",
                                "Pilibhit","Lakhimpur Kheri","Kokrajhar","Chirang","Baksa",
                                "Sonitpur","Dhemaji","Lakhimpur","Panchkula","Udalguri","Biswanath",
                                "Kheri","Pathankot","Una","Sahibzada Ajit Singh Nagar","Saharanpur",
                                "Yamunanagar","Bahraich","Shrawasti","Balrampur","Pashchim Champaran",
                                "Purba Champaran") |
             STATE.NAME %in% c("Ladakh","Jammu and Kashmir","Himachal Pradesh","Uttarakhand",
                               "Sikkim","Arunachal Pradesh")) %>% 
    # retain Hamirpur in HP and remove the one in UP
    filter((STATE.NAME != "Uttar Pradesh" | DISTRICT.NAME != "Hamirpur") &
             # remove CT completely because Bilaspur is found in CT as well as HP
             (STATE.NAME != "Chhattisgarh")) %>% 
    mutate(COUNTRY = "India") %>% 
    bind_rows(bt_dists_sf, np_dists_sf)
  
  cur_states_sf <- states_sf %>%
    dplyr::select(-AREA) %>% 
    filter(STATE.NAME %in% cur_dists_sf$STATE.NAME) %>% 
    mutate(COUNTRY = "India") %>% 
    bind_rows(bt_states_sf, np_states_sf)
  
  
  # combining all countries
  data0 <- data0 %>% 
    bind_rows(bt_np) %>%
    filter((STATE.NAME %in% cur_states_sf$STATE.NAME | 
             DISTRICT.NAME %in% cur_dists_sf$DISTRICT.NAME) & 
             # retain Hamirpur in HP and remove the one in UP
             (STATE.NAME != "Uttar Pradesh" | DISTRICT.NAME != "Hamirpur") &
             # remove CT completely because Bilaspur is found in CT as well as HP
             (STATE.NAME != "Chhattisgarh"))
  
  data_all <- data_all %>% 
    bind_rows(bt_np_all) %>%
    filter((STATE.NAME %in% cur_states_sf$STATE.NAME | 
              DISTRICT.NAME %in% cur_dists_sf$DISTRICT.NAME) & 
             # retain Hamirpur in HP and remove the one in UP
             (STATE.NAME != "Uttar Pradesh" | DISTRICT.NAME != "Hamirpur") &
             # remove CT completely because Bilaspur is found in CT as well as HP
             (STATE.NAME != "Chhattisgarh"))
  
  
  
  regions <- cur_dists_sf %>% 
    st_drop_geometry() %>% 
    dplyr::select(DISTRICT.NAME, STATE.NAME, COUNTRY) %>% 
    mutate(REGION1 = case_when(STATE.NAME %in% c("Jammu and Kashmir", "Ladakh", 
                                                 "Himachal Pradesh", "Punjab", "Haryana")
                               ~ "Western Region",
                               STATE.NAME %in% c("Uttarakhand", "Uttar Pradesh", "Bihar") |
                                 COUNTRY == "Nepal"
                               ~ "Central Region",
                               STATE.NAME %in% c("Arunachal Pradesh", "Sikkim",
                                                 "West Bengal", "Assam") |
                                 COUNTRY == "Bhutan"
                               ~ "Eastern Region")) %>% 
    mutate(REGION1 = factor(REGION1, 
                            levels = c("Western Region", "Central Region", "Eastern Region")))
  
  no_regions <- n_distinct(regions$REGION1)
  
} 

regions_sf <- regions %>% 
  left_join(cur_dists_sf %>% dplyr::select(-COUNTRY), by = c("STATE.NAME", "DISTRICT.NAME")) %>% 
  st_as_sf() %>%
  st_make_valid() %>% # otherwise below results in TopologyException error
  # joins multiple polygons into one for each region
  group_by(REGION1) %>% 
  dplyr::summarise()

# adding regions to data
data0 <- data0 %>% left_join(regions)
data_all <- data_all %>% left_join(regions)


# create and write a file with common and scientific names of all species
# useful for mapping
temp <- data0 %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(!EXOTIC.CODE %in% c("X")) %>%
  distinct(COMMON.NAME)

write.csv(temp, row.names = FALSE, 
          file = glue("{cur_outpath}{cur_event$FULL.CODE}_speclist.csv"))
rm(temp)


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


overall_com_spec <- data0 %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  # taking only districts with sufficient (10) lists to calculate REPFREQ
  group_by(STATE.NAME) %>% 
  mutate(LISTS.ST = n_distinct(GROUP.ID)) %>% 
  ungroup() %>%
  filter(LISTS.ST > 10) %>%
  # repfreq
  group_by(COMMON.NAME, STATE.NAME) %>% 
  summarise(REP.FREQ = 100*n_distinct(GROUP.ID)/max(LISTS.ST)) %>% 
  # averaging repfreq across states
  ungroup() %>% 
  mutate(NO.STATES = n_distinct(STATE.NAME)) %>% 
  group_by(COMMON.NAME) %>% 
  summarise(REP.FREQ = sum(REP.FREQ)/max(NO.STATES)) %>% 
  ungroup() %>% 
  # top 5 per region
  arrange(desc(REP.FREQ))

write_xlsx(x = list("Overall stats" = overall_stats, 
                    "Top 30 checklist uploaders" = top30, 
                    "Top birders per state" = top_state,
                    "Birders per state" = birder_state,
                    "Summary per district" = dist_sum,
                    "Summary per state" = state_sum,
                    "Summary per day" = day_sum,
                    "Summary per state-day" = state_day_sum,
                    "Overall common species" = overall_com_spec),
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
  right_join(cur_dists_sf,
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
  # we only want to plot full states
  filter(!STATE.NAME %in% c("Punjab", "Haryana", "West Bengal", "Assam", "Uttar Pradesh", "Bihar")) %>% 
  magrittr::set_colnames(c("State", "Species", "Total checklists", "Participants",
                           "Locations")) %>% 
  left_join(cur_states_sf,
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
  geom_sf(aes(fill = REGION1, geometry = DISTRICT.GEOM), colour = NA) +
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




# unknown -----------------------------------------------------------------
# 
# ## subset Nepal data for their app
# 
# nep = data %>% filter(ST_NM == "NEPAL")
# nep1 = nep[,-c(2,4,10,11,25,26,27,28,30,33,34,35,36,37)]
# names(nep1)[22:23] = c("day","year")
# write.csv(nep1,"nepal_data.csv", row.names = F)

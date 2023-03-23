# adapted from https://github.com/ashwinv2005/repeated-eBirding-event-analyses/blob/master/GBBC%202022.R

require(lubridate)
require(tidyverse)
require(sf)
require(mapview)
require(leaflet)
require(writexl)
require(ggthemes)


# paths
cur_outpath <- glue("outputs/{cur_event$SHORT.CODE}/{cur_event$EDITION}/")
if (!dir.exists(cur_outpath)) (dir.create(cur_outpath, recursive = T))

source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/summaries.R")
source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/mapping.R")


# preparing data ----------------------------------------------------------

# filtering for loc & dates
if (cur_event$SHORT.CODE == "GBBC"){
  
  data0 <- data %>% 
    filter(OBSERVATION.DATE %in% seq(cur_event$START.DATE, cur_event$END.DATE, 
                                     by = "days"))
  
  
  # previous years' data for GBBC
  sched_event <- sched %>% filter(SHORT.CODE == cur_event$SHORT.CODE)
  
  data_all <- data %>% 
    filter(YEAR %in% sched_event$EDITION) %>% 
    group_by(YEAR) %>% 
    left_join(sched_event %>% 
                dplyr::select(EDITION, START.DATE, END.DATE), 
              by = c("YEAR" = "EDITION")) %>% 
    filter(OBSERVATION.DATE %in% seq(START.DATE, END.DATE, by = "days")) %>% 
    ungroup() %>% 
    mutate(START.DATE = NULL, END.DATE = NULL) %>% 
    mutate(YEAR = factor(YEAR))
  

  # joining campus information to data for CBC
  campus <- read_sheet("googlesheet") 
  # names(campus)[1] = "LOCALITY.ID"
  # names(campus)[2] = "CAMPUS"
  
  data_campus<- data0 %>% 
    left_join(campus, by = "LOCALITY.ID") %>% 
    filter(!is.na(CAMPUS))
  

  # Joining mapvars to data
  sf_use_s2(FALSE)
  data0 <- join_map_sf(data0)
  data_all <- join_map_sf(data_all)
  data_campus <- join_map_sf(data_campus)
  
  
  cur_dists_sf <- dists_sf
  cur_states_sf <- states_sf
  
  
  regions <- cur_dists_sf %>% 
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
  
  no_regions <- n_distinct(regions$REGION1)
  
} 

# sf for regions
regions_sf <- regions %>% 
  left_join(cur_states_sf) %>% 
  st_as_sf() %>% 
  # joins multiple polygons into one for each region
  group_by(REGION1) %>% 
  dplyr::summarise()

# adding regions to data
data0 <- data0 %>% left_join(regions)
data_all <- data_all %>% left_join(regions)
data_campus <- data_campus %>% left_join(regions)


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
  right_join(cur_dists_sf %>% dplyr::select(-AREA)) %>% 
  st_as_sf()


# different breakpoints in visualisation

max_lists <- max(na.omit(dist_stats$LISTS.ALL))
break_at <- if (max_lists %in% 50:100) {
  rev(c(0, 5, 10, 20, 30, max_lists))
} else if (max_lists %in% 100:200) {
  rev(c(0, 10, 25, 50, 90, max_lists))
} else if (max_lists %in% 200:300) {
  rev(c(0, 20, 50, 90, 150, max_lists))
} else if (max_lists %in% 300:500) {
  rev(c(0, 30, 80, 150, 250, max_lists))
} else if (max_lists %in% 500:1000) {
  rev(c(0, 30, 100, 200, 400, max_lists))
} else if (max_lists > 1000) {
  rev(c(0, 10, 50, 100, 250, 1000, max_lists))
} 


mapviewOptions(fgb = FALSE)
map_effort_dist <- mapView(dist_stats, 
                      zcol = c("LISTS.ALL"), 
                      map.types = c("Esri.WorldImagery"),
                      layer.name = c("Checklists per district"), 
                      popup = leafpop::popupTable(dist_stats,
                                                  zcol = c("DISTRICT.NAME", "LISTS.ALL",
                                                           "PARTICIPANTS", "LOCATIONS", "SPECIES"), 
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
  right_join(cur_states_sf %>% dplyr::select(-AREA)) %>% 
  st_as_sf()


# different breakpoints in visualisation

max_lists <- max(na.omit(state_stats$LISTS.ALL))
break_at <- if (max_lists %in% 500:1000) {
  rev(c(0, 30, 100, 200, 400, max_lists))
} else if (max_lists %in% 1000:2000) {
  rev(c(0, 10, 50, 100, 250, 1000, max_lists))
} else if (max_lists %in% 2000:5000) {
  rev(c(0, 30, 80, 200, 500, 1000, 2000, max_lists))
} else if (max_lists > 5000) {
  rev(c(0, 30, 100, 200, 500, 1000, 3000, 5000, max_lists))
} 


mapviewOptions(fgb = FALSE)
map_effort_state <- mapView(state_stats, 
                      zcol = c("LISTS.ALL"), 
                      map.types = c("Esri.WorldImagery"),
                      layer.name = c("Checklists per state"), 
                      popup = leafpop::popupTable(dist_stats,
                                                  zcol = c("STATE.NAME", "LISTS.ALL",
                                                           "PARTICIPANTS", "LOCATIONS", "SPECIES"), 
                                                  feature.id = FALSE,
                                                  row.numbers = FALSE),
                      at = break_at, 
                      alpha.regions = 0.6)

# webshot::install_phantomjs()
mapshot(map_effort_state, 
        url = glue("{cur_outpath}{cur_event$FULL.CODE}_stateseffortmap.html"))



# combining dist and state maps
map_effort <- map_effort_dist + map_effort_state

mapshot(map_effort, url = glue("{cur_outpath}{cur_event$FULL.CODE}_statesdistseffortmap.html"))

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
  dplyr::select(`person hours`, `lists (all types)`, `unique lists`) %>% 
  magrittr::set_colnames("Person hours", "Total checklists", "Unique checklists") %>% 
  pivot_longer(everything(), names_to = "STAT", values_to = "VALUES")
  
data2 <- over_time %>% 
  # keeping only necessary
  dplyr::select(`eBirders`) %>% 
  magrittr::set_colnames("Participants") %>% 
  pivot_longer(everything(), names_to = "STAT", values_to = "VALUES")

data3 <- over_time %>% 
  # keeping only necessary
  dplyr::select(`species`) %>% 
  magrittr::set_colnames("Species") %>% 
  pivot_longer(everything(), names_to = "STAT", values_to = "VALUES")

# number of districts calc
data4 <- data_all %>% 
  mutate(YEAR = factor(YEAR)) %>% 
  group_by(YEAR) %>% 
  summarise(VALUES = n_distinct(DISTRICT.NAME),
            STAT = "Districts")


require(extrafont)
pos_dodge <- position_dodge(0.2)


plot_breaks <- c(0,6000,12000,18000,24000,30000,36000,42000)
plot1 <- ggplot(data1, aes(x = YEAR, y = VALUES, col = STAT)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(x = "Years", y = "") +
  theme_mod_tufte() +
  scale_colour_manual(breaks = c("Unique checklists", "Total checklists", "Person hours"), 
                      values = palette) +
  # scale_x_continuous(breaks = 2013:2023) +
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
  # scale_x_continuous(breaks = 2013:2023) +
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
  # scale_x_continuous(breaks = 2013:2023) +
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
  # scale_x_continuous(breaks = 2013:2023) +
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
  # scale_x_continuous(breaks = 2015:2023) +
  scale_colour_manual(breaks = c("Common Myna", "Rock Pigeon", "Red-vented Bulbul"), 
                      values = palette) 


# Campus Bird Count -------------------------------------------------------

# plot campus-wise stats on map

campus_stats <- data_campus %>% 
  group_by(CAMPUS) %>% 
  basic_stats(pipeline = T, prettify = F) %>% 
  # function retains grouping
  ungroup() %>% 
  # keeping only necessary
  dplyr::select(CAMPUS, SPECIES, LISTS.ALL, LISTS.U, PARTICIPANTS) %>% 
  complete(CAMPUS = campus$Name, 
           fill = list(SPECIES = 0,
                       LISTS.ALL = 0,
                       LISTS.U = 0,
                       PARTICIPANTS = 0,
                       LOCATIONS = 0)) %>% 
  right_join(cur_dists_sf %>% dplyr::select(-AREA)) %>% 
  st_as_sf()


# different breakpoints in visualisation

max_lists <- max(na.omit(dist_stats$LISTS.ALL))
break_at <- if (max_lists %in% 50:100) {
  rev(c(0, 5, 10, 20, 30, max_lists))
} else if (max_lists %in% 100:200) {
  rev(c(0, 10, 25, 50, 90, max_lists))
} else if (max_lists %in% 200:300) {
  rev(c(0, 20, 50, 90, 150, max_lists))
} else if (max_lists %in% 300:500) {
  rev(c(0, 30, 80, 150, 250, max_lists))
} else if (max_lists %in% 500:1000) {
  rev(c(0, 30, 100, 200, 400, max_lists))
} else if (max_lists > 1000) {
  rev(c(0, 10, 50, 100, 250, 1000, max_lists))
} 


mapviewOptions(fgb = FALSE)
map_effort_dist <- mapView(dist_stats, 
                           zcol = c("LISTS.ALL"), 
                           map.types = c("Esri.WorldImagery"),
                           layer.name = c("Checklists per district"), 
                           popup = leafpop::popupTable(dist_stats,
                                                       zcol = c("DISTRICT.NAME", "LISTS.ALL",
                                                                "PARTICIPANTS", "LOCATIONS", "SPECIES"), 
                                                       feature.id = FALSE,
                                                       row.numbers = FALSE),
                           at = break_at, 
                           alpha.regions = 0.6)

# webshot::install_phantomjs()
mapshot(map_effort_dist, 
        url = glue("{cur_outpath}{cur_event$FULL.CODE}_distseffortmap.html"))

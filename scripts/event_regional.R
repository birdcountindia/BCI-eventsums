
require(lubridate)
require(tidyverse)
# require(rgeos)
# require(ggfortify)
require(sf)
require(mapview)
require(leaflet)
# require(rmapshaper)
require(writexl)


# paths
cur_outpath <- glue("outputs/{cur_event$SHORT.CODE}/{cur_event$EDITION)}/")



# preparing data ----------------------------------------------------------

# filtering for loc & dates
if (cur_event$SHORT.CODE == "BiBC"){
  
  data0 <- data %>% 
    filter(OBSERVATION.DATE %in% seq(cur_event$START.DATE, cur_event$END.DATE, 
                                     by = "days")) %>% 
    filter(STATE.CODE == "IN-AS")
  
  cur_dists_sf <- dists_sf %>% filter(STATE.NAME == "Assam")
  
  
  regions <- dists_sf %>% 
    st_drop_geometry() %>% 
    dplyr::select(DISTRICT.NAME) %>% 
    mutate(REGION1 = case_when(DISTRICT.NAME %in% c("Udalguri", "Darrang", "Sonitpur", 
                                                    "Biswanath", "Lakhimpur", "Dhemaji")
                               ~ "North Assam Division",
                               DISTRICT.NAME %in% c("Dhubri", "Kokrajhar", "Bongaigaon", 
                                                    "Goalpara", "Baksa", "Chirang", "Barpeta", 
                                                    "Nalbari", "Kamrup", "Kamrup Metropolitan", 
                                                    "South Salmara Mankachar")
                               ~ "Lower Assam Division",
                               DISTRICT.NAME %in% c("Dima Hasao", "Karbi Anglong", 
                                                    "West Karbi Anglong", "Nagaon", 
                                                    "Morigaon", "Hojai")
                               ~ "Central Assam Division",
                               DISTRICT.NAME %in% c("Dibrugarh", "Tinsukia", "Sivasagar", 
                                                    "Jorhat", "Golaghat", "Charaideo", 
                                                    "Majuli")
                               ~ "Upper Assam Division",
                               DISTRICT.NAME %in% c("Cachar", "Hailakandi", "Karimganj")
                               ~ "Barak Valley Division"),
           REGION2 = case_when(DISTRICT.NAME %in% c("Dhubri","South Salmara Mancachar",
                                                    "Kokrajhar","Goalpara","Bongaigaon",
                                                    "Chirang","Baksa","Barpeta",
                                                    "Nalbari","Kamrup")
                               ~ "West",
                               DISTRICT.NAME %in% c("Kamrup Metropolitan","Darrang",
                                                    "Udalguri","Morigaon","Nagaon",
                                                    "Sonitpur","Biswanath")
                               ~ "Central",
                               DISTRICT.NAME %in% c("West Karbi Anglong","Hojai",
                                                    "Karbi Anglong","Dima Hasao","Cachar",
                                                    "Karimganj","Hailakandi","Golaghat")
                               ~ "South",
                               DISTRICT.NAME %in% c("Lakhimpur","Majuli","Jorhat",
                                                    "Sivasagar","Dhemaji","Dibrugarh",
                                                    "Charaideo","Tinsukia")
                               ~ "East"))
  
  
} else if (cur_event$SHORT.CODE == "PBC"){
  
  data0 <- data %>% 
    filter(OBSERVATION.DATE %in% seq(cur_event$START.DATE, cur_event$END.DATE, 
                                     by = "days")) %>% 
    filter(STATE.CODE %in% c("IN-TN","IN-PY")) %>% 
    filter(!is.na(COUNTY.CODE) & !(COUNTY.CODE %in% c("IN-PY-YA","IN-PY-MA")))
  
  cur_dists_sf <- dists_sf %>% 
    filter(STATE.NAME %in% c("Tamil Nadu", "Puducherry"),
           !(DISTRICT.NAME %in% c("Mahe","Yanam")))
  
  
  regions <- dists_sf %>% 
    st_drop_geometry() %>% 
    dplyr::select(DISTRICT.NAME) %>% 
    mutate(REGION1 = case_when(DISTRICT.NAME %in% c("Vellore","Tiruvannamalai","Krishnagiri",
                                                    "Dharmapuri","Salem","Namakkal","Erode",
                                                    "The Nilgiris","Tirupathur")
                               ~ "West",
                               DISTRICT.NAME %in% c("Perambalur","Karur","Dindigul",
                                                    "Thiruvarur","Coimbatore","Tiruppur",
                                                    "Tiruchirappalli")
                               ~ "Central",
                               DISTRICT.NAME %in% c("Theni","Madurai","Sivaganga","Pudukkottai",
                                                    "Ramanathapuram","Virudhunagar","Thoothukkudi",
                                                    "Tirunelveli","Kanniyakumari","Tenkasi")
                               ~ "South",
                               DISTRICT.NAME %in% c("Tiruvallur","Chennai","Kancheepuram",
                                                    "Viluppuram","Cuddalore","Nagapattinam",
                                                    "Kallakurichi","Ariyalur","Thanjavur",
                                                    "Chengalpattu","Ranipet","Thiruvarur",
                                                    "Puducherry","Karaikal")
                               ~ "East"))
  
}



# create and write a file with common and scientific names of all species
# useful for mapping
temp <- data0 %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  distinct(COMMON.NAME, SCIENTIFIC.NAME)

write.csv(temp, row.names = FALSE, 
          file = glue("{cur_outpath}{cur_event$FULL.CODE}_speclist.csv"))
rm(temp)



# stats -----------------------------------------------------------

# overall stats
source("https://raw.githubusercontent.com/birdcountindia/bci-functions/main/summaries.R")
overall_stats <- basic_stats(data0)

# top 10 checklist uploaders (eligible list filter)
top10 <- data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1, DURATION.MINUTES >= 14) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  filter(!any(OBSERVATION.COUNT == "X")) %>% 
  ungroup() %>% 
  group_by(OBSERVER.ID, FULL.NAME) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% 
  ungroup() %>%
  arrange(desc(NO.LISTS)) %>% 
  slice(1:10)

# list of birders per district
birder_dist <- data0 %>%
  distinct(DISTRICT.NAME, OBSERVER.ID) %>%
  # joining names
  left_join(eBird_users) %>% 
  arrange(DISTRICT.NAME)

# district-wise summary
dist_sum <- data0 %>%
  group_by(DISTRICT.NAME) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.BIRDERS = n_distinct(OBSERVER.ID)) %>%
  arrange(desc(NO.LISTS))

# day-wise summary
day_sum <- data0 %>%
  group_by(DAY.M) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.BIRDERS = n_distinct(OBSERVER.ID)) %>%
  arrange(desc(NO.LISTS))

# district-day-wise summary
dist_day_sum <- data0 %>%
  group_by(DISTRICT.NAME, DAY.M) %>% 
  summarise(NO.LISTS = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            NO.BIRDERS = n_distinct(OBSERVER.ID)) %>%
  arrange(DISTRICT.NAME, DAY.M)


write_xlsx(x = list("Overall stats" = overall_stats, 
                    "Top 10 checklist uploaders" = top10, 
                    "Birders per district" = birder_dist,
                    "Summary per district" = dist_sum,
                    "Summary per day" = day_sum,
                    "Summary per district-day" = dist_day_sum),
           path = glue("{cur_outpath}{cur_event$FULL.CODE}_stats.xlsx"))



# plot districtwise stats on map ----------------------------------------------------

dist_stats <- data0 %>% group_by(DISTRICT.NAME) %>% basic_stats(prettify = F) %>% 
  # keeping only necessary
  dplyr::select(SPECIES, LISTS.ALL, PARTICIPANTS, LOCATIONS) %>% 
  ungroup() %>% 
  right_join(dists_sf)


# different breakpoints in visualisation
if (cur_event$SHORT.CODE == "BiBC"){
  
  break_at <- rev(c(0, 5, 10, 20, 30, max(na.omit(dist_stats$CHECKLISTS))))
  
} else if (cur_event$SHORT.CODE == "PBC"){
  
  break_at <- rev(c(0, 10, 50, 100, 250, max(na.omit(dist_stats$CHECKLISTS))))
  
}


mapviewOptions(fgb = FALSE)
map_effort <- mapView(effortmap, 
                      zcol = c("CHECKLISTS"), 
                      map.types = c("Esri.WorldImagery"),
                      layer.name = c("CHECKLISTS - DISTRICTS"), 
                      popup = leafpop::popupTable(dist_stats,
                                                  zcol = c("DISTRICT.NAME", "CHECKLISTS",
                                                           "PARTICIPANTS", "LOCATIONS", "SPECIES"), 
                                                  feature.id = FALSE,
                                                  row.numbers = FALSE),
                      at = break_at, 
                      alpha.regions = 0.6)
mapshot(map_effort, 
        file = glue("{cur_outpath}{cur_event$FULL.CODE}_dists_effort.html"))



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
  # taking only districts with sufficient (5) lists to calculate REPFREQ
  group_by(DISTRICT.NAME) %>% 
  mutate(LISTS.D = n_distinct(GROUP.ID)) %>% 
  ungroup() %>%
  filter(LISTS.D > 5) %>%
  # repfreq
  group_by(COMMON.NAME, REGION1, DISTRICT.NAME) %>% 
  summarise(REP.FREQ = n_distinct(GROUP.ID)/max(LISTS.D)) %>% 
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


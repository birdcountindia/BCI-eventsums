require(lubridate)
require(tidyverse)
require(glue)
require(sf)

### required files in this directory: ###
# - spatial data (pre-processed) as "maps_sf.RData" file 
#   (see https://github.com/birdcountindia/india-maps)
###   ###

# preparing data ----------------------------------------------------------

load("data/maps_sf.RData")
load(maindatapath)


# user info
eBird_users <- read.delim(userspath, sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
  transmute(OBSERVER.ID = observer_id,
            FULL.NAME = paste(first_name, last_name, sep = " "))

# list of group accounts to be filtered
groupaccs <- read_csv(groupaccspath) %>%
  mutate(CATEGORY = case_when(GA.1 == 1 ~ "GA.1", GA.2 == 1 ~ "GA.2", TRUE ~ "NG"))
filtGA <- groupaccs %>% 
  # both categories need to be filtered because this is birder-related 
  filter(CATEGORY == "GA.1" | CATEGORY == "GA.2") %>% 
  select(OBSERVER.ID)

# including some group accounts for GBBC like Salem School Students accounts
if (any(list_national$SHORT.CODE == "GBBC")){
 
  exceptions <- groupaccs %>% 
    filter(str_detect(FULL.NAME, "Salem") & str_detect(FULL.NAME, "School Students") |
             str_detect(FULL.NAME, "Salem Ornithological Foundation"))
    
  filtGA <- filtGA %>% anti_join(exceptions)

  }

data <- data %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID")


# palette for plots -------------------------------------------------------

palette <- c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", 
             "#31954E", "#493F3D", "#CC6666", "#9999CC", "#000000", "#66CC99")

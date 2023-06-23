require(lubridate)
require(tidyverse)
require(glue)

# automated parameters ----------------------------------------------------

# paths to latest versions of user & GA info, and sensitive species data
load(url("https://github.com/birdcountindia/ebird-datasets/raw/main/EBD/latest_non-EBD_paths.RData"))
userspath <- glue("../ebird-datasets/{userspath}")
senspath <- glue("../ebird-datasets/{senspath}")
groupaccspath <- glue("../ebird-datasets/{groupaccspath}")

# date under consideration for current leaderboard
cur_date <- if (today() %>% day() <= 16) { 
  (today() - months(1)) %>% floor_date(unit = "month")
} else {
  today() %>% floor_date(unit = "month")
}

rel_date <- if (today() %>% day() <= 16) {
  ((today() - months(1)) - months(1)) %>%
    floor_date(unit = "month")
} else {
  (today() - months(1)) %>%
    floor_date(unit = "month")
}

cur_year <- cur_date %>% year()
cur_month_num <- cur_date %>% month()
cur_month_lab <- cur_date %>% month(label = T, abbr = T)
# cur_month_num <- (cur_date - months(1)) %>% month()
# cur_month_lab <- (cur_date - months(1)) %>% month(label = T, abbr = T)

rel_year <- rel_date %>% year()
rel_month_num <- rel_date %>% month()
rel_month_lab <- rel_date %>% month(label = T, abbr = T)
# rel_month_num <- (rel_date - months(1)) %>% month()
# rel_month_lab <- (rel_date - months(1)) %>% month(label = T, abbr = T) 


maindatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{rel_month_lab}-{rel_year}.RData")
slicedatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{rel_month_lab}-{rel_year}_slice.RData")


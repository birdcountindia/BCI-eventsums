require(lubridate)
require(tidyverse)
require(glue)
require(skimmr)

# automated parameters ----------------------------------------------------

# paths to latest versions of user & GA info, and sensitive species data
userspath <- glue("../ebird-datasets/EBD/ebd_users_relFeb-2024.txt")
senspath <- glue("../ebird-datasets/EBD/ebd_sensitive_relFeb-2024_IN.txt")
groupaccspath <- glue("../ebird-datasets/group-accounts/ebd_users_GA_relFeb-2024.csv")

# get_param()
get_param(date_currel = "2024-02-01")


maindatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{currel_month_lab}-{currel_year}.RData")
slicedatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{currel_month_lab}-{currel_year}_slice.RData")


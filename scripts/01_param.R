require(lubridate)
require(tidyverse)
require(glue)
require(skimmr)

# automated parameters ----------------------------------------------------

# paths to latest versions of user & GA info, and sensitive species data
userspath <- glue("../ebird-datasets/EBD/ebd_users_relNov-2023.txt")
senspath <- glue("../ebird-datasets/EBD/ebd_sensitive_relNov-2023_IN.txt")
groupaccspath <- glue("../ebird-datasets/group-accounts/ebd_users_GA_relNov-2023.csv")

get_param()


maindatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{currel_month_lab}-{currel_year}.RData")
slicedatapath <-  glue("../ebird-datasets/EBD/ebd_IN_rel{currel_month_lab}-{currel_year}_slice.RData")


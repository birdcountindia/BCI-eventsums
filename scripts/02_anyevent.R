# checking whether any events in current time period
# if so, run those analyses

require(tidyverse)
require(lubridate)
require(googlesheets4)
require(glue)


## authorising google sheets
gs4_auth(email = "birdcountindia@ncf-india.org")


## getting events schedule 

sched0 <- read_sheet("https://docs.google.com/spreadsheets/d/1Yf_STtmbtPZHybwT544cKkY8jQ5DuoZrM-jiZiG5oiU/edit?usp=sharing") %>% 
  # creating full event code string
  mutate(FULL.CODE = ifelse(is.na(TYPE.CODE), 
                            glue("{SHORT.CODE}_{EDITION}"),
                            glue("{SHORT.CODE}_{TYPE.CODE}_{EDITION}"))) %>% 
  mutate(across(contains(".DATE"), ~ as.Date(.)),
         START.MONTH = month(START.DATE),
         END.MONTH = month(END.DATE))

sched <- sched0 %>% 
  # filtering for current month of interest
  filter(EDITION == rel_year,
         START.MONTH %in% rel_month_num | END.MONTH %in% rel_month_num)


## is there any event?

if (rlang::is_empty(sched$FULL.NAME)) {
  anyevent <- FALSE
} else {
  anyevent <- TRUE
}

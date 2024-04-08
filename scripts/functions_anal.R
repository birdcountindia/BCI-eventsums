
# basic functions -------------------------------------------------------------------

get_dates <- function(dates_cur = lubridate::today()) {
  
  # dates_cur can also be a vector of dates (for multi-day events)
  
  if (!is.Date(dates_cur)) {
    dates_cur <- dates_cur %>% lubridate::as_date()
  }
  
  dates_prev <- dates_cur - lubridate::years(1)
  
  # return objects to environment
  list("dates_cur" = dates_cur,
       "dates_prev" = dates_prev) %>%
    list2env(envir = .GlobalEnv)
  
}


# add SoIB 2023 info
map_to_soib <- function(data) {
  
  ebird_changes <- read_xlsx("data/eBird_2023_taxonomy_changes_IN.xlsx") %>% 
    dplyr::select("OLD: eBird Name (default)", "NEW: eBird: English (default) Name") %>% 
    magrittr::set_colnames(c("eBird.English.Name.2022", "eBird.English.Name.2023"))
  
  soib_2023 <- read_csv("../soib_v2/01_analyses_full/results/SoIB_main.csv") %>% 
    dplyr::select(eBird.English.Name.2022, SOIBv2.Priority.Status, India.Endemic) 
  
  mapped <- data %>% 
    left_join(ebird_changes, by = c("COMMON.NAME" = "eBird.English.Name.2023")) %>% 
    mutate(eBird.English.Name.2022 = case_when(
      is.na(eBird.English.Name.2022) ~ COMMON.NAME,
      TRUE ~ eBird.English.Name.2022
    )) %>% 
    left_join(soib_2023) %>% 
    dplyr::select(COMMON.NAME, SOIBv2.Priority.Status, India.Endemic) %>% 
    mutate(SOIBv2.Priority.Status = factor(SOIBv2.Priority.Status,
                                           levels = c("High", "Moderate", "Low"))) %>% 
    arrange(SOIBv2.Priority.Status, desc(India.Endemic))
  
  return(mapped)
}


# eBird API functions ---------------------------------------------------------------

# admin units based on eBird 
get_admin_codes <- function(unit_code, hi_arch = TRUE) {
  
  if (!exists("all_units")) {
    
    list_countries <- ebirdsubregionlist("country", key = myebirdtoken)
    
    parent_code <- str_sub(unit_code, 1, 2)
    list_states <- ebirdsubregionlist("subnational1", parent_code, key = myebirdtoken)
    list_districts <- ebirdsubregionlist("subnational2", parent_code, key = myebirdtoken)
    
    all_units <- list_countries %>% 
      bind_rows(list_states) %>% 
      bind_rows(list_districts) 
    
    list("list_countries" = list_countries,
         "parent_code" = parent_code,
         "list_states" = list_states,
         "list_districts" = list_districts,
         "all_units" = all_units) %>% 
      list2env(envir = .GlobalEnv)
    
  }
  
  
  if (!unit_code %in% all_units$code) {
    return("Input admin unit code is not a valid code!")
  }
  
  # if country, we want only subnational1
  req_adm2 <- if (unit_code %in% list_countries$code) FALSE else TRUE
  
  if (!hi_arch) {
    
    return(unit_code)
    
  } else {
    
    req_units <- all_units %>% 
      filter(str_detect(code, unit_code)) %>% 
      {if (req_adm2) {
        .
      } else {
        anti_join(., list_districts, by = c("code", "name"))
      }} %>% 
      pull(code)
    
    return(req_units)
    
  }
  
}

get_admin_names <- function(region_input) {
  
  if (!exists("all_units")) {
    list_countries <- ebirdsubregionlist("country", key = myebirdtoken)
    
    parent_code <- str_sub(unit_code, 1, 2)
    list_states <- ebirdsubregionlist("subnational1", parent_code, key = myebirdtoken)
    list_districts <- ebirdsubregionlist("subnational2", parent_code, key = myebirdtoken)
    
    all_units <- list_countries %>% 
      bind_rows(list_states) %>% 
      bind_rows(list_districts) 
    
    list("list_countries" = list_countries,
         "parent_code" = parent_code,
         "list_states" = list_states,
         "list_districts" = list_districts,
         "all_units" = all_units) %>% 
      list2env(envir = .GlobalEnv)
  }
  
  region_names <- all_units %>% 
    filter(code %in% get_admin_codes(region_input, hi_arch = TRUE)) %>% 
    magrittr::set_colnames(c("REGION", "REGION.NAME"))
  
  return(region_names)
  
}


# summary functions -------------------------------------------------------

gen_spec_list_nonAPI <- function(data, regions, dates, repfreq = FALSE) {
  
  # regardless if regions = cur_region or regions = cur_region_children, parent 
  # will be cur_region
  parent_code <- cur_region
  
  which_regions <- if (str_count(regions[1], "-") == 0) "country" else {
    if (str_count(regions[1], "-") == 1) "state" else "district"
  }
  
  data1 <- data %>% 
  {if (which_regions == "state") {
    rename(., REGION = STATE.CODE)
  } else if (which_regions == "district") {
    rename(., REGION = COUNTY.CODE)
  } else {
    mutate(., REGION = "IN")
  }} %>% 
    filter(REGION %in% regions) %>% 
    filter(CATEGORY %in% c("species", "issf"))
  
  spec_rf <- data1 %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(REGION) %>% 
    mutate(LISTS = n_distinct(GROUP.ID)) %>% 
    ungroup() %>%
    # repfreq
    group_by(COMMON.NAME, REGION) %>% 
    summarise(REP.FREQ = 100*n_distinct(GROUP.ID)/max(LISTS)) %>% 
    ungroup() %>% 
    # top per region
    arrange(desc(REP.FREQ))
  
  
  list_spec <- data1 %>% 
    distinct(REGION, COMMON.NAME, OBSERVATION.DATE) %>% 
    mutate(OBSERVATION.DATE = as_date(OBSERVATION.DATE)) %>% 
    group_by(OBSERVATION.DATE) %>% 
    mutate(DAY.NO = cur_group_id()) %>% 
    ungroup() %>% 
    left_join(ebd_tax, by = c("COMMON.NAME" = "ENGLISH.NAME")) %>% 
    arrange(DAY.NO, REGION, SORT) %>% 
    left_join(get_admin_names(parent_code), by = "REGION") %>% 
    dplyr::select(DAY.NO, REGION, REGION.NAME, COMMON.NAME) %>%  # remove DATE for pivot
    mutate(PRESENT = 1) %>% 
    pivot_wider(names_from = "DAY.NO", names_glue = "DAY{DAY.NO}", values_from = "PRESENT") %>% 
    mutate(across(starts_with("DAY"), ~ replace_na(., replace = 0))) %>% 
    arrange(REGION, across(starts_with("DAY"), desc)) %>% 
    {if (repfreq == TRUE) {
      left_join(., spec_rf, by = c("COMMON.NAME", "REGION"))
    } else {
      .
    }}
  
  return(list_spec)
  
}


gen_part_summ_nonAPI <- function(data, regions, dates) {
  
  # regardless if regions = cur_region or regions = cur_region_children, parent 
  # will be cur_region
  parent_code <- cur_region
  
  which_regions <- if (str_count(regions[1], "-") == 0) "country" else {
    if (str_count(regions[1], "-") == 1) "state" else "district"
  }
  
  data1 <- data %>% 
    {if (which_regions == "state") {
      rename(., REGION = STATE.CODE)
    } else if (which_regions == "district") {
      rename(., REGION = COUNTY.CODE)
    } else {
      mutate(., REGION = "IN")
    }} %>% 
    filter(REGION %in% regions)
  
  summary_part <- data1 %>% 
    group_by(REGION, OBSERVATION.DATE) %>% 
    basic_stats(pipeline = TRUE, prettify = FALSE) %>% 
    ungroup() %>% 
    dplyr::select(REGION, OBSERVATION.DATE, PARTICIPANTS, LISTS.C, SPECIES) %>% 
    magrittr::set_colnames(c("REGION", "OBSERVATION.DATE", 
                             "OBSERVERS", "CHECKLISTS", "SPECIES")) %>% 
    mutate(OBSERVATION.DATE = as_date(OBSERVATION.DATE)) %>% 
    group_by(OBSERVATION.DATE) %>% 
    mutate(DAY.NO = cur_group_id()) %>% 
    ungroup() %>% 
    arrange(DAY.NO, desc(OBSERVERS), desc(SPECIES)) %>% 
    left_join(get_admin_names(parent_code), by = "REGION") %>% 
    dplyr::select(DAY.NO, REGION, REGION.NAME, OBSERVERS, CHECKLISTS, SPECIES) %>%  # remove DATE for pivot
    pivot_longer(c(OBSERVERS, CHECKLISTS, SPECIES),
                 names_to = "TOTAL", values_to = "VALUE") %>% 
    pivot_wider(names_from = "DAY.NO", names_glue = "DAY{DAY.NO}", values_from = "VALUE") %>% 
    mutate(across(starts_with("DAY"), ~ replace_na(., replace = 0))) %>% 
    arrange(REGION)

  # adding all-day totals
  tot_summary <- data1 %>% 
    group_by(REGION) %>% 
    basic_stats(pipeline = TRUE, prettify = FALSE) %>% 
    ungroup() %>% 
    dplyr::select(REGION, PARTICIPANTS, LISTS.C, SPECIES) %>% 
    magrittr::set_colnames(c("REGION", 
                             "OBSERVERS", "CHECKLISTS", "SPECIES")) %>% 
    pivot_longer(c(OBSERVERS, CHECKLISTS, SPECIES),
                 names_to = "TOTAL", values_to = "ALL.DAYS") 
  
  summary_part <- summary_part %>% 
    left_join(tot_summary, by = c("REGION", "TOTAL"))
  
  return(summary_part)
  
}


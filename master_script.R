## getting automated parameters
source("scripts/01_param.R")


## checking for events in month of interest
source("scripts/02_anyevent.R")

if (anyevent == FALSE) {
  
  print("There are no events that need summaries this month. Quitting from script.")
  
} else {
  
  list_national <- sched %>% filter(CATEGORY == "National")
  
  list_regional <- sched %>% filter(CATEGORY == "Regional")
  
  list_multiregional <- sched %>% filter(CATEGORY == "Multi-regional")
  
  source("scripts/03_data-import.R")
  
  
  
## running corresponding analyses depending on which events present in this month
  
  if (rlang::is_empty(list_national$FULL.NAME)) {
    
    print("No national events.")
    
  } else {
    
    # for each event
    for (i in 1:length(list_national$FULL.NAME)) {
      
      cur_event <- list_national[i,] 
      tictoc::tic(glue("Completed analysis {i}/{length(list_national$FULL.NAME)}: {cur_event$FULL.CODE}"))
      source("scripts/event_national.R")
      tictoc::toc()
      
    }
    
  }
  
  
  if (rlang::is_empty(list_regional$FULL.NAME)) {
    
    print("No regional events.")
    
  } else {
    
    # for each event
    for (i in 1:length(list_regional$FULL.NAME)) {
      
      cur_event <- list_regional[i,] 
      tictoc::tic(glue("Completed analysis {i}/{length(list_regional$FULL.NAME)}: {cur_event$FULL.CODE}"))
      source("scripts/event_regional.R")
      tictoc::toc()
      
    }
    
  }
  

  if (rlang::is_empty(list_multiregional$FULL.NAME)) {
    
    print("No multi-regional events.")
    
  } else {
    
    # for each event
    for (i in 1:length(list_multiregional$FULL.NAME)) {
      
      cur_event <- list_multiregional[i,] 
      tictoc::tic(glue("Completed analysis {i}/{length(list_multiregional$FULL.NAME)}: {cur_event$FULL.CODE}"))
      source("scripts/event_multiregional.R")
      tictoc::toc()
      
    }
    
  }
  
  
}




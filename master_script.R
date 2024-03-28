## getting automated parameters
source("scripts/01_param.R")


## checking for events in month of interest
source("scripts/02_anyevent.R")

if (anyevent == FALSE) {
  
  print("There are no events that need summaries this month. Quitting from script.")
  
} else {
  
  list_simple <- sched %>% filter(!(SHORT.CODE %in% c("GBBC", "HBC")))
  # only GBBC and HBC need complex summaries
  list_national <- sched %>% filter(SHORT.CODE == "GBBC")
  list_multiregional <- sched %>% filter(SHORT.CODE == "HBC")
  
  source("scripts/03_data-import.R")
  

  ## running corresponding analyses depending on which events present in this month
  
  
  if (rlang::is_empty(list_simple$FULL.NAME)) {

    message("No simple-summary events.")

  } else {

    # for each event
    for (i in 1:length(list_simple$FULL.NAME)) {

      cur_event <- list_simple[i,] # paths
      cur_outpath <- glue("outputs/{cur_event$SHORT.CODE}/{cur_event$EDITION}/")
      if (!dir.exists(cur_outpath)) (dir.create(cur_outpath, recursive = T))

      tictoc::tic(glue("Completed analysis {i}/{length(list_simple$FULL.NAME)}: {cur_event$FULL.CODE}"))
      rmarkdown::render("event_simple_summary.Rmd",
                        output_dir = cur_outpath, output_file = "summary_post")
      tictoc::toc()

    }

  }
  
  
  if (rlang::is_empty(list_national$FULL.NAME)) {
    
    message("No national events.")
    
  } else {
    
    # for each event
    for (i in 1:length(list_national$FULL.NAME)) {
      
      cur_event <- list_national[i,] 
      tictoc::tic(glue("Completed analysis {i}/{length(list_national$FULL.NAME)}: {cur_event$FULL.CODE}"))
      source("scripts/event_national.R")
      tictoc::toc()
      
    }
    
  }
  

  if (rlang::is_empty(list_multiregional$FULL.NAME)) {
    
    message("No multi-regional events.")
    
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


library(shiny)


# Define UI for app that draws a histogram ----

ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello eBirder!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Date selection (value always yyyy-mm-dd, even if display format different) ----
      helpText(h4("Select the start and end dates of interest")),
      
      dateInput(inputId = "startdate", 
                label = "Start date", 
                value = "2022-11-01", format = "dd/mm/yy"),
      
      dateInput(inputId = "enddate", 
                label = "End date", 
                value = "2022-11-04", format = "dd/mm/yy"),
      
      # Input: Event code for save file name ----
      helpText(h4("Provide a short event code (for file downloads)")),
      textInput(inputId = "event_code", 
                value = "event_code", label = NULL), 
      
      # Input: download button ----
      helpText(h4("Download your data!")),
      downloadButton("downloadData", "Download",
                     label = "Data summary"),
      downloadButton("downloadViz", "Download",
                     label = "Visual summary")

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Lists per day ----
      plotOutput(outputId = "sumPlot"),
      
      # Output: Summary table ----
      tableOutput("table")
      
    )
    
  )

)


# Define server logic required to draw a histogram ----

server <- function(input, output) {
  
  library(tidyverse)
  library(lubridate)
  library(glue)

  # importing latest month's EBD data
  # this app's directory should be at same level as ebird-datasets/
  load("../ebird-datasets/EBD/ebd_IN_relNov-2022_NOV.RData")

  
  # Reactive value for selected dataset (filter, summarise) ----
  data_sum <- reactive({
    
    # startdate <- "2022-11-01"
    # enddate <- "2022-11-04"
    startdate <- input$startdate
    enddate <- input$enddate
    
    # converting input date selection to lubridates
    filt_time <- data.frame(DATE.START = startdate %>% ymd(),
                            DATE.END = enddate %>% ymd()) %>% 
      mutate(YEAR.START = year(DATE.START),
             YEAR.END = year(DATE.END),
             MONTH.START = month(DATE.START),
             MONTH.END = month(DATE.END),
             DAY.M.START = day(DATE.START),
             DAY.M.END = day(DATE.END))
    
    # the output for this data_filt object
    data_mc %>% 
      # time filter
      filter(YEAR %in% filt_time$YEAR.START:filt_time$YEAR.END,
             MONTH %in% filt_time$MONTH.START:filt_time$MONTH.END,
             DAY.M %in% filt_time$DAY.M.START:filt_time$DAY.M.END) %>% 
      group_by(DAY.M) %>% 
      summarise(NO.LISTS = n_distinct(GROUP.ID))
    
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    
    data_sum()
      
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    
    filename = function(){glue("{input$event_code}_data.csv")},
    content = function(file) {
      write_csv(data_sum(), file)
    }
    
  )

  
  # Summary visualisation of number of lists per day ----
  
  output$sumPlot <- renderPlot({

    # plotting 
    data_filt %>% 
      group_by(DAY.M) %>% 
      summarise(NO.LISTS = n_distinct(GROUP.ID)) %>% 
      ggplot() +
      theme_classic() +
      geom_col(aes(DAY.M, NO.LISTS))
    
    
  })
  
  # Download maps ----
  output$downloadViz <- downloadHandler(
    
    filename = function(){glue("{input$event_code}_map1.png")},
    content = function(file) {
      (sumPlot()) %>% 
        ggsave(file)
    }
    
  )
  
}


# Define app ----

shinyApp(ui = ui, server = server)
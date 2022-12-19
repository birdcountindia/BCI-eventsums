library(shiny)


# Define UI for app that draws a histogram ----

ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello eBirder!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      # Input: Date selection (value always yyyy-mm-dd, even if display format different) ----
      helpText(h4("Select the start and end dates of interest")),
      
      dateInput(inputId = "startdate", 
                label = "Start date", 
                value = "2023-01-01", format = "dd/mm/yy"),
      
      dateInput(inputId = "enddate", 
                label = "End date", 
                value = "2023-01-04", format = "dd/mm/yy")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      
      # Output: Lists per day ----
      plotOutput(outputId = "sumPlot")
      
    )
  )
)


# Define server logic required to draw a histogram ----

server <- function(input, output) {
  
  library(tidyverse)
  library(lubridate)

  # importing latest month's EBD data
  # this app's directory should be at same level as ebird-datasets/
  load("../ebird-datasets/EBD/ebd_IN_relNov-2022_NOV.RData")
  
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })

  
  # Summary visualisation of number of lists per day ----
  
  output$sumPlot <- renderPlot({
    
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
    
    data_filt <- data_mc %>% 
      # time filter
      filter(YEAR %in% filt_time$YEAR.START:filt_time$YEAR.END,
             MONTH %in% filt_time$MONTH.START:filt_time$MONTH.END,
             DAY.M %in% filt_time$DAY.M.START:filt_time$DAY.M.END)
    
    # plotting 
    data_filt %>% 
      group_by(DAY.M) %>% 
      summarise(NO.LISTS = n_distinct(GROUP.ID)) %>% 
      ggplot() +
      theme_classic() +
      geom_col(aes(DAY.M, NO.LISTS))
    
    
  })
  
}


# Define app ----

shinyApp(ui = ui, server = server)
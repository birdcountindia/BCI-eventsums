
# to determine appropriate plot breaks ------------------------------------

gen_plot_breaks <- function(values) {
  
  val_range <- ceiling(max(values) - min(values))
  
  if (val_range %in% 0:100) {
    scale_by <- 20 # breaks should be by 100s
    round_to <- -1 # rounding to Tens
  } else if (val_range %in% 101:500) {
    scale_by <- 100 # breaks should be by 100s
    round_to <- -2 # rounding to Hundreds
  } else if (val_range %in% 501:1000) {
    scale_by <- 200 # breaks should be by 500s
    round_to <- -2 # rounding to Hundreds
  } else if (val_range %in% 1001:2000) {
    scale_by <- 500 # breaks should be by 500s
    round_to <- -2 # rounding to Hundreds
  } else if (val_range %in% 2001:10000) {
    scale_by <- 1000 # breaks should be by 1000s
    round_to <- -3 # rounding to Thousands
  } else {
    scale_by <- min(values)*floor((max(values) %/% min(values))/5)
    round_to <- -3
  }
  
  pad_low <- round_to*(-2/3)
  pad_high <- round_to*(-5/9)
  
  if (pad_low >= 1 & pad_high >= 1) {
    val_low <- round(min(values)/pad_low, round_to)
    val_high <- round(max(values)*pad_high, round_to)
  } else {
    val_low <- round(min(values) - 50, round_to)
    val_high <- round(max(values) + 50, round_to)
  }
  
  plot_breaks <- seq(val_low, val_high, by = scale_by)
  
  return(plot_breaks)
  
}

# Is extrafont installed? -------------------------------------------------

is.extrafont.installed <- function(){
  
  if (is.element("extrafont", installed.packages()[, 1])) {
    
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
    
  } else{
    
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
    To enable full font support, run: 
      install.packages('extrafont') 
      font_import()")
    
    return(F)
    
  }
  
}


# base font family (adapting tufte) ---------------------------------------

base_ff <- function(){
  
  if (is.extrafont.installed()) {
    
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", 
                               "Verdana", "serif"), 
                             quiet = FALSE)  
  
  } else{
    
    tuftefont <- "serif"
    
  }
  return(tuftefont)
  
}


# ggplot theme (adapting tufte) -------------------------------------------

theme_mod_tufte <- function(base_size = 11, base_family = base_ff(), ticks = TRUE) {
  
  new_theme <- theme_bw(base_family = base_family, 
                  base_size = base_size) + 
    theme(axis.line = element_line(color = 'black'),
          axis.title.x = element_text(vjust = -0.3), 
          axis.title.y = element_text(vjust = 0.8),
          legend.background = element_blank(), 
          legend.key = element_blank(), 
          legend.title = element_text(face="plain"),
          panel.background = element_blank(), 
          panel.border = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_blank(),
          strip.background = element_blank()) +
    theme(axis.title.x = element_text(size = 16), 
          axis.title.y = element_blank(), 
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 14),
          legend.title = element_blank(), 
          legend.text = element_text(size = 12), 
          legend.position = "bottom",
          text = element_text(family = "Gill Sans MT"))
  
  if (!ticks) {
    new_theme <- new_theme + theme(axis.ticks = element_blank())
  }
  
  new_theme
  
} 

# ggplot themes -----------------------------------------------------------




# -------------------------------------------------------------------------



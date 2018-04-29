#' @title All Centered Case Study Theme
#' @description Theme for case study graphs with titles, subtitles and captions centered.
#' @author Stefanie Molin
#' 
#' @import ggplot2
#'
#' @return A ggplot theme
#' 
#' @export
#' 

case_study_theme <- function(){
  ggplot2::theme(
    panel.background = element_rect(fill = "white")
    , plot.background = element_rect(fill = "transparent", colour = NA)
    , panel.grid.minor = element_blank()
    , legend.background = element_rect(fill = "transparent", colour = NA)
    , legend.box.background = element_rect(fill = "transparent", colour = NA)
    , legend.key = element_blank()
    , panel.grid = element_blank()
    , panel.border = element_blank()
    , axis.line = element_line(color = "black")
    , plot.caption = element_text(size = 6, hjust = 0.5)
    , plot.title = element_text(size = 10, hjust = 0.5)
    , plot.subtitle = element_text(size = 9, hjust = 0.5)
    , axis.title = element_text(size = 9)
    , axis.text = element_text(size = 8)
    , legend.text = element_text(size = 8)
    , legend.title = element_text(size = 9)
  )
}

#' @title Color Scheme for Lookback Window ROC Curves
#' @description Color scheme for qualitative uses
#' @author Stefanie Molin
#' 
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
#'
#' @return A ggplot color scheme
#' 
#' @export
#' 

lookback_window_color_scheme <- function(){
  ggplot2::scale_color_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Dark2")[4:6])
}

#' @title Color Scheme for Time of Month
#' @description Color scheme for beginning, middle, and end of month
#' @author Stefanie Molin
#' 
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
#' 
#' @param reverse Boolean indicating whether or not to reverse the color order for the chart (useful when some of the charts will use coord_flip() but the legend will be shared).
#' Defaults to FALSE
#'
#' @return A ggplot color scheme
#' 
#' @export
#' 

time_of_month_color_scheme <- function(reverse = FALSE){
  color_set <- "Pastel2"
  if(reverse){
    ggplot2::scale_fill_manual(values = rev(RColorBrewer::brewer.pal(n = 3, name = color_set)))
  } else{
    ggplot2::scale_fill_brewer(palette = color_set, breaks = time_order_for_plots())
  }
}

#' @title Facet Wrap Theme
#' @description Formatting of facet wrap for case study
#' @author Stefanie Molin
#' 
#' @import ggplot2
#' @importFrom grid unit
#'
#' @return A ggplot formatting
#' 
#' @export
#' 

facet_wrap_theme <- function(){
  ggplot2::theme(
    strip.background = element_blank()
    , strip.text = element_text(size = 9)
    , panel.spacing = grid::unit(1, "cm")
  ) 
}

#' @title Time of Month Factor
#' @description Factor of ordered time of month for graphing
#' @author Stefanie Molin
#' 
#' @param reverse Boolean whether or not to reverse the order. Defaults to FALSE
#' 
#' @return A vector
#' 
#' @export
#'
time_order_for_plots <- function(reverse = FALSE){
  time_order <- c("Beginning", "Middle", "End")
  
  if(reverse){
    time_order <- rev(time_order)
  }
  return(time_order)
}
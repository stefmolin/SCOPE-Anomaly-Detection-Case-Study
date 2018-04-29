#' @title Control Chart
#' @description Plots a control chart with colored lines for zones.
#' @author Stefanie Molin
#' 
#' @import dplyr
#' @import ggplot2
#'
#' @param data Dataframe with 2 columns: "index" and "value",
#'  where "index" is the number of the row and "value" is the value to plot
#'
#' @return A ggplot object
#'
#' @note More info here: \url{https://en.wikipedia.org/wiki/Western_Electric_rules} and here: 
#' \url{https://en.wikipedia.org/wiki/Nelson_rules}
#' 
#' @export
#' 

control_chart <- function(data){
  mean <- mean(data$value)
  sd <- sd(data$value)
  
  ucl <- mean + 3 * sd
  lcl <- mean - 3 * sd
  
  plot <- data %>% ggplot2::ggplot(aes(x = index, y = value)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = mean, color = "lightsteelblue1") +
    ggplot2::geom_text(aes(1, mean, label = "mu", vjust = -0.7), color = "lightsteelblue1", parse = TRUE) + 
    ggplot2::geom_hline(yintercept = ucl, color = "lightsteelblue4") + 
    ggplot2::geom_text(aes(1, ucl, label = "UCL", vjust = -0.7), color = "lightsteelblue4") +
    ggplot2::geom_hline(yintercept = lcl, color = "lightsteelblue4") + 
    ggplot2::geom_text(aes(1, lcl, label = "LCL", vjust = 1.3), color = "lightsteelblue4") +
    ggplot2::geom_hline(yintercept = mean + 2 * sd, color = "lightsteelblue3", linetype = 3) + 
    ggplot2::geom_text(aes(1, mean + 2 * sd, label = "+2*sigma", vjust = -0.7), color = "lightsteelblue3", parse = TRUE) +
    ggplot2::geom_hline(yintercept = mean - 2 * sd, color = "lightsteelblue3", linetype = 3) + 
    ggplot2::geom_text(aes(1, mean - 2 * sd, label = "-2*sigma", vjust = 1.3), color = "lightsteelblue3", parse = TRUE) +
    ggplot2::geom_hline(yintercept = mean + sd, color = "lightsteelblue2", linetype = 3) + 
    ggplot2::geom_text(aes(1, mean + sd, label = "+sigma", vjust = -0.7), color = "lightsteelblue2", parse = TRUE) +
    ggplot2::geom_hline(yintercept = mean - sd, color = "lightsteelblue2", linetype = 3) +
    ggplot2::geom_text(aes(1, mean - sd, label = "-sigma", vjust = 1.3), color = "lightsteelblue2", parse = TRUE) +
    ggplot2::scale_y_continuous(limits = c(min(lcl*.8, lcl*1.2, min(data$value)), max(ucl*1.2, ucl*.8, max(data$value)))) +
    ggplot2::theme(
      panel.background = element_rect(fill = "white")
      , plot.background = element_rect(fill = "transparent", colour = NA)
      , panel.grid.minor = element_blank()
      , legend.background = element_rect(fill = "transparent", colour = NA)
      , legend.box.background = element_rect(fill = "transparent", colour = NA)
      , legend.key = element_blank()
      , panel.grid = element_blank()
      , panel.border = element_blank()
      , axis.title = element_blank()
      , axis.ticks = element_blank()
      , axis.text = element_blank()
      , axis.line = element_blank()
      , plot.caption = element_text(size = 6, hjust = 0, margin = margin(t = 15))
      , plot.title = element_text(size = 10)
      , plot.subtitle = element_text(size = 8)
    )
 
 return(plot)
}
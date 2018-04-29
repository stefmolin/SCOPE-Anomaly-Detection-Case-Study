#' @title Standard Normal Plot
#' @description Plots a standard normal with colored sections for standard deviations
#' @author Stefanie Molin
#' 
#' @import ggplot2
#'
#' @return A ggplot object
#'
#' @export
#'
standard_normal_plot <- function(){
  p <- ggplot2::ggplot(data.frame(x = seq(-4, 4, 0.1)), aes(x = x)) + 
    ggplot2::stat_function(fun = dnorm) + 
    ggplot2::stat_function(fun = dnorm, geom = "area", fill = "lightsteelblue2", alpha = 1, 
                           xlim = c(-1, 1)) +
    ggplot2::stat_function(fun = dnorm, geom = "area", fill = "lightsteelblue3", alpha = 1, 
                           xlim = c(-2, -1)) +
    ggplot2::stat_function(fun = dnorm, geom = "area", fill = "lightsteelblue3", alpha = 1, 
                           xlim = c(1, 2)) +
    ggplot2::stat_function(fun = dnorm, geom = "area", fill = "lightsteelblue4", alpha = 1,
                           xlim = c(-3, -2)) +
    ggplot2::stat_function(fun = dnorm, geom = "area", fill = "lightsteelblue4", alpha = 1,
                           xlim = c(2, 3)) +
    ggplot2::geom_text(x = 0, y = dnorm(0.9), size = 3, 
                       label = paste0(round(pnorm(1) - pnorm(-1), 4) * 100, "%")) +
    ggplot2::geom_segment(aes(x = -1, y = dnorm(-1), xend = 1, yend = dnorm(1)), 
                          arrow = arrow(ends = "both", type = "open",
                                        length = unit(0.045, "inches"))) +
    ggplot2::geom_text(x = 0, y = dnorm(1.8), size = 3, 
                       label = paste0(round(pnorm(2) - pnorm(-2), 4) * 100, "%")) +
    ggplot2::geom_segment(aes(x = -2, y = dnorm(-2), xend = 2, yend = dnorm(2)), 
                          arrow = arrow(ends = "both", type = "open",
                                        length = unit(0.045, "inches"))) +
    ggplot2::geom_text(x = 0, y = dnorm(2.3), size = 3, 
                       label = paste0(round(pnorm(3) - pnorm(-3), 4) * 100, "%")) +
    ggplot2::geom_segment(aes(x = -3, y = dnorm(-3), xend = 3, yend = dnorm(3)),
                          arrow = arrow(ends = "both", type = "open",
                                        length = unit(0.045, "inches"))) +
    ggplot2::scale_x_continuous(breaks = c(-3:3), 
                                labels = c(expression("-3"*sigma), expression("-2"*sigma), 
                                           expression("-"*sigma), expression(mu), expression("+"*sigma),
                                           expression("+2"*sigma), expression("+3"*sigma))) + 
    ggplot2::labs(x = "", y = "", title = "Standard Normal Distribution", 
                  subtitle = expression(mu*" = mean | "*sigma*" = standard deviation"),
                  caption = expression("This graph is N(0,1) ~ mean = 0, standard deviation = 1; however, these percentages will apply to all normal distributions.")) +
    ggplot2::theme(panel.background = element_rect(fill = "white")
                   , plot.background = element_rect(fill = "transparent", colour = NA)
                   , panel.grid.minor = element_blank()
                   , legend.background = element_rect(fill = "transparent", colour = NA)
                   , legend.box.background = element_rect(fill = "transparent", colour = NA)
                   , legend.key = element_blank()
                   , panel.grid = element_blank()
                   , panel.border = element_blank()
                   , axis.line.x = element_line(color = "black")
                   , axis.line.y = element_blank()
                   , axis.text.y = element_blank()
                   , axis.ticks.y = element_blank()
                   , plot.caption = element_text(size = 6, hjust = 0.5)
                   , plot.title = element_text(size = 10, hjust = 0.5)
                   , plot.subtitle = element_text(size = 8, hjust = 0.5)
                   , axis.title = element_text(size = 9))
  
  return(p)
}
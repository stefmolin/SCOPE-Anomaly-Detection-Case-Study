library(ggplot2)
library(dplyr)

# sample data sets to test rules
beyond_control_limits <- data.frame(index = 1:15, value = c(1, 10, 2, 9, 3, 8, 4, 7, 5, 6, 2, 8, 6, 4, 35))
break_rule_2 <- data.frame(index = 1:15, value = c(1, 10, 2, 9, 3, 8, 4, 7, 5, 6, 2, 8, 36, 4, 35))
break_rule_3 <- data.frame(index = 1:15, value = c(1, 1, 2, 1, 3, 8, 4, 1, 5, 6, 20, 8, 16, 14, 15))
break_rule_4 <- data.frame(index = 1:15, value = c(-11, -11, 2, 1, 3, 2, 40, 11, 15, 16, 20, 18, 16, 14, 15))
break_stratification_rule <- data.frame(index = 1:16, value = c(4, 2, 3, 2, 3, 2, 2, 3, 3, 3, 2, 2, 2, 2, 3, 3))
break_mixture_rule <- data.frame(index = 1:15, value = c(6, 4, 5, 6, 3, 5, 4, 9, 9, 9, 2, 9, 1, 10, 1))
break_trend_rule <- data.frame(index = 1:15, value = c(6, 4, 5, 6, 3, 5, 4, 9, 9, 8, 7, 5, 4, 3, 1))
break_noise_rule <- data.frame(index = 1:15, value = c(1, 10, 2, 9, 3, 8, 4, 7, 5, 6, 2, 8, -36, -34, -35))
no_issues <- data.frame(index = 1:15, value = c(1, 10, 2, 9, 3, 8, 4, 7, 5, 6, 2, 8, 6, 4, 5))

# select a dataset to plot
sample_data <- break_trend_rule

mean <- mean(sample_data$value)
sd <- sd(sample_data$value)

ucl <- mean + 3 * sd
lcl <- mean - 3 * sd

control_plot <- sample_data %>% ggplot2::ggplot(aes(x = index, y = value)) +
  ggplot2::geom_line() +
  ggplot2::geom_hline(yintercept = mean, color = "darkgreen") +
  ggplot2::geom_text(aes(1, mean, label = "mu", vjust = -1), color = "darkgreen", parse = TRUE) + 
  ggplot2::geom_hline(yintercept = ucl, color = "red") + 
  ggplot2::geom_text(aes(1, ucl, label = "UCL", vjust = -1), color = "red") +
  ggplot2::geom_hline(yintercept = lcl, color = "red") + 
  ggplot2::geom_text(aes(1, lcl, label = "LCL", vjust = 1.5), color = "red") +
  ggplot2::geom_hline(yintercept = mean + 2 * sd, color = "orangered", linetype = 3) + 
  ggplot2::geom_text(aes(1, mean + 2 * sd, label = "+2*sigma", vjust = -1), color = "orangered", parse = TRUE) +
  ggplot2::geom_hline(yintercept = mean - 2 * sd, color = "orangered", linetype = 3) + 
  ggplot2::geom_text(aes(1, mean - 2 * sd, label = "-2*sigma", vjust = 1.5), color = "orangered", parse = TRUE) +
  ggplot2::geom_hline(yintercept = mean + sd, color = "springgreen3", linetype = 3) + 
  ggplot2::geom_text(aes(1, mean + sd, label = "+sigma", vjust = -1), color = "springgreen3", parse = TRUE) +
  ggplot2::geom_hline(yintercept = mean - sd, color = "springgreen3", linetype = 3) +
  ggplot2::geom_text(aes(1, mean - sd, label = "-sigma", vjust = 1.5), color = "springgreen3", parse = TRUE) +
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

control_plot
##################################################
######        EDA Functions Library         ######
######         by: Stefanie Molin           ######
####                                          ####
## Functions for exploratory data analysis.     ##
##################################################

#' @title Mode
#' @description Calculate the mode of a vector
#' @author Stefanie Molin
#'
#' @param x Vector of data to calculate the mode from
#'
#' @return The mode (most common value)
#'
#' @export
calculate_mode <- function(x){
  unique_values <- unique(x)
  counts <- tabulate(match(x, unique_values))
  return(unique_values[which(counts == max(counts))])
}

#' @title Coefficient of Variation
#' @description Calculate the coefficient of variation of a vector
#' @author Stefanie Molin
#'
#' @param x Vector of data
#'
#' @return The coefficient of variation
#'
#' @export
coefficient_of_variation_stats <- function(x){
  variance <- var(x)
  standard_deviation <- sqrt(variance)
  average <- mean(x)
  coefficient_of_variation <- standard_deviation / average
  return(coefficient_of_variation)
}

#' @title EDA Data Prep for AS Data
#' @description Prepare the AS data for EDA
#' @author Stefanie Molin
#' 
#' @import dplyr
#'
#' @param df Dataframe of AS data
#' @param beginning_dates Dates to marked as "beginning" period
#' @param middle_dates Dates to marked as "middle" period
#' @param end_dates Dates to marked as "end" period
#'
#' @return A list with client-level and campaign-level dataframes
#'
#' @export
EDA_data_prep_AS <- function(df, beginning_dates, middle_dates, end_dates){
  df <- df %>% 
    dplyr::mutate(period = ifelse(run_date %in% as.Date(beginning_dates), "beginning",
                                  ifelse(run_date %in% as.Date(middle_dates), "middle", "end"))) 
  
  return(list(client_df = df %>% 
                dplyr::filter(as.character(series) == as.character(client_name)),
              campaign_df = df %>% 
                dplyr::filter(as.character(series) != as.character(client_name))))
}

#' @title EDA Data Prep for TS Data
#' @description Prepare the TS data for EDA
#' @author Stefanie Molin
#' 
#' @import dplyr
#'
#' @param df Dataframe of TS data
#' @param beginning_dates Dates to marked as "beginning" period
#' @param middle_dates Dates to marked as "middle" period
#' @param end_dates Dates to marked as "end" period
#'
#' @return A dataframe
#'
#' @export
EDA_data_prep_TS <- function(df, beginning_dates, middle_dates, end_dates){
  df <- df %>% 
    dplyr::mutate(period = ifelse(run_date %in% as.Date(beginning_dates), "beginning",
                                  ifelse(run_date %in% as.Date(middle_dates), "middle", "end"))) 
  
  return(df)
}

#' @title EDA Data Prep for RexT Data
#' @description Prepare the RexT data for EDA
#' @author Stefanie Molin
#' 
#' @import dplyr
#'
#' @param df Dataframe of RexT data
#' @param beginning_dates Dates to marked as "beginning" period
#' @param middle_dates Dates to marked as "middle" period
#' @param end_dates Dates to marked as "end" period
#'
#' @return A dataframe
#'
#' @export
EDA_data_prep_RexT <- function(df, beginning_dates, middle_dates, end_dates){
  df <- df %>% 
    dplyr::mutate(period = ifelse(run_date %in% as.Date(beginning_dates), "beginning",
                                  ifelse(run_date %in% as.Date(middle_dates), "middle", "end")),
                  series = ifelse(as.character(subregion) == "N/A", as.character(region),
                                  ifelse(as.character(country) == "N/A", as.character(subregion), as.character(country)))) 
  
  return(df)
}

#' @title Violin Plot for AS Data
#' @description Draw a violin plot for AS data
#' @author Stefanie Molin
#' 
#' @import ggplot2
#' @import dplyr
#'
#' @param df Dataframe to build the plot with
#'
#' @return A ggplot object
#'
#' @export
violin_plot_AS <- function(df){
  df <- df %>% 
    dplyr::distinct(series, kpi, day, value)
  p <- ggplot2::ggplot(df, aes(x = kpi, y = value)) +
    ggplot2::geom_violin(fill = "lightsteelblue2", draw_quantiles = c(0.25, 0.5, 0.75)) +
    # ggplot2::geom_boxplot(width = 0.025, fill = "lightsteelblue2") +
    ggplot2::stat_summary(fun.data=mean_sdl, geom="pointrange", color = "black") +
    ggplot2::scale_y_log10() + 
    ggplot2::labs(x = "KPI", y = "Log base 10 of value", 
                  caption = "Violin plot contains client-level data only.\nHorizontal lines are quartiles and vertical line with dot is mean +/- standard deviation") +
    case_study_theme() + 
    ggplot2::theme(axis.text.y = element_blank(), axis.line.y = element_blank(), 
                   axis.title.y = element_blank(), axis.ticks.y = element_blank())
  
  return(p)
}

#' @title Violin Plot for TS Data
#' @description Draw Violin Plots for TS data
#' @author Stefanie Molin
#' 
#' @import ggplot2
#' @import dplyr
#'
#' @param df Dataframe to build the plot with
#'
#' @return A list of ggplot objects (one for event name plots and one for site type plots)
#'
#' @export
violin_plot_TS <- function(df){
  df <- df %>% 
    dplyr::distinct(series, event_name, site_type, day, value)
  
  event_name_plot <- ggplot2::ggplot(df, aes(x = event_name, y = value)) +
    ggplot2::geom_violin(fill = "lightsteelblue2", draw_quantiles = c(0.25, 0.5, 0.75)) +
    # ggplot2::geom_boxplot(width = 0.025, fill = "lightsteelblue2") +
    ggplot2::stat_summary(fun.data=mean_sdl, geom="pointrange", color = "black") +
    ggplot2::scale_y_log10() + 
    ggplot2::labs(x = "Event Name", y = "Log base 10 of value", 
                  subtitle = "Violin plots by event name",
                  caption = "Horizontal lines are quartiles and vertical line with dot is mean +/- standard deviation") +
    case_study_theme() + 
    ggplot2::theme(axis.text.y = element_blank(), axis.line.y = element_blank(), 
                   axis.title.y = element_blank(), axis.ticks.y = element_blank())
  
  site_type_plot <- ggplot2::ggplot(df, aes(x = site_type, y = value)) +
    ggplot2::geom_violin(fill = "lightsteelblue2", draw_quantiles = c(0.25, 0.5, 0.75)) +
    # ggplot2::geom_boxplot(width = 0.025, fill = "lightsteelblue2") +
    ggplot2::stat_summary(fun.data=mean_sdl, geom="pointrange", color = "black") +
    ggplot2::scale_y_log10() + 
    ggplot2::labs(x = "Site Type", y = "Log base 10 of value", 
                  subtitle = "Violin plots by site type",
                  caption = "Horizontal lines are quartiles and vertical line with dot is mean +/- standard deviation") +
    case_study_theme() + 
    ggplot2::theme(axis.text.y = element_blank(), axis.line.y = element_blank(), 
                   axis.title.y = element_blank(), axis.ticks.y = element_blank())
  
  return(list(event_name = event_name_plot, site_type = site_type_plot))
}

#' @title Violin Plot for RexT Data
#' @description Draw a violin plot for RexT data
#' @author Stefanie Molin
#' 
#' @import ggplot2
#' @import dplyr
#'
#' @param df Dataframe to build the plot with
#'
#' @return A ggplot object
#'
#' @export
violin_plot_RexT <- function(df){
  df <- df %>% 
    dplyr::distinct(series, day, value)
  p <- ggplot2::ggplot(df, aes(x = series, y = value)) +
    ggplot2::geom_violin(fill = "lightsteelblue2", draw_quantiles = c(0.25, 0.5, 0.75)) +
    # ggplot2::geom_boxplot(width = 0.025, fill = "lightsteelblue2") +
    ggplot2::stat_summary(fun.data=mean_sdl, geom="pointrange", color = "black") +
    ggplot2::scale_y_log10() + 
    ggplot2::labs(x = "Territory", y = "Log base 10 of value", 
                  caption = "Horizontal lines are quartiles and vertical line with dot is mean +/- standard deviation") +
    case_study_theme() + 
    ggplot2::theme(axis.text.y = element_blank(), axis.line.y = element_blank(), 
                   axis.title.y = element_blank(), axis.ticks.y = element_blank())
  
  return(p)
}

#' @title AS Violin Plots Arrangement
#' @description Creates arrangement of violin plots of AS data
#' @author Stefanie Molin
#' 
#' @importFrom cowplot plot_grid
#' @importFrom dplyr filter %>% 
#' @importFrom ggplot2 ggtitle
#' 
#' @param data AS dataframe
#'
#' @return A plot object
#' 
#' @export
#' 
arrange_AS_violin_plots <- function(data){
  col_one <- cowplot::plot_grid(data %>% 
                                  dplyr::filter(kpi %in% c("client_rext", "spend", "rext_euro", "tac", "margin", "order_value")) %>% 
                                  violin_plot_AS() + 
                                  ggplot2::ggtitle("Distribution of AS Data on log scale"),  
                                ncol = 1, nrow = 1)
  col_two <- cowplot::plot_grid(data %>% 
                                  dplyr::filter(kpi %in% c("cos", "cr", "ctr")) %>% 
                                  violin_plot_AS(), 
                                data %>% 
                                  dplyr::filter(kpi %in% c("clicks", "conversions", "displays")) %>% 
                                  violin_plot_AS(), 
                                ncol = 2, nrow = 1)
  cowplot::plot_grid(col_one, col_two, ncol = 1, nrow = 2)
}

#' @title TS Violin Plots Arrangement
#' @description Creates arrangement of violin plots of TS data
#' @author Stefanie Molin
#' 
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 ggtitle
#' 
#' @param data TS dataframe
#'
#' @return A plot object
#' 
#' @export
#' 
arrange_TS_violin_plots <- function(data){
  plots <- violin_plot_TS(data)
  col_one <- cowplot::plot_grid(plots$event_name + 
                                  ggplot2::ggtitle("Distribution of TS Data on log scale"),  
                                ncol = 1, nrow = 1)
  col_two <- cowplot::plot_grid(plots$site_type, 
                                ncol = 1, nrow = 1)
  cowplot::plot_grid(col_one, col_two, ncol = 1, nrow = 2)
}

#' @title RexT Violin Plots Arrangement
#' @description Creates arrangement of violin plots of RexT data
#' @author Stefanie Molin
#' 
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 ggtitle
#' @importFrom dplyr filter %>% 
#' 
#' @param data RexT dataframe
#'
#' @return A plot object
#' 
#' @export
#' 
arrange_RexT_violin_plots <- function(data){
  col_one <- cowplot::plot_grid(data %>% 
                                  dplyr::filter(as.character(country) != "N/A") %>% 
                                  violin_plot_RexT() + 
                                  ggplot2::ggtitle("Distribution of RexT Data on log scale"),  
                                ncol = 1, nrow = 1)
  col_two <- cowplot::plot_grid(data %>% 
                                  dplyr::filter(series == "AMERICAS") %>% 
                                  violin_plot_RexT(),
                                data %>% 
                                  dplyr::filter(as.character(country) == "N/A" &
                                                  as.character(subregion) != "N/A") %>% 
                                  violin_plot_RexT(), 
                                ncol = 2, nrow = 1, rel_widths = c(1, 2))
  cowplot::plot_grid(col_one, col_two, ncol = 1, nrow = 2, rel_heights = c(1, 0.6))
}

#' @title Coefficient of Variation Violin Plots
#' @description Creates violin plots showing the distribution of the coefficient of variation across the given series and metrics
#' @author Stefanie Molin
#' 
#' @import ggplot2
#' @import dplyr 
#' @importFrom reshape2 melt
#' @importFrom cowplot plot_grid
#' 
#' @param data Wide dataframe (unmelted)
#' @param series_column String name of the column containing the unique identifier of the series
#' @param metrics Vector of the metrics that are to be shown
#' @param last_index The index of the last metric to show in the table. 
#' Defaults to NULL meaning you will only be returned one graph. 
#' If you choose to split, you will be returned a plot object of multiple plots.
#' @param preprocess Boolean indicating whether or not to prepocess the data, use this if the data won't work as is. Defaults to FALSE.
#'
#' @return A plot object
#' 
#' @export
#' 
coefficient_of_variation_violin_plots <- function(data, series_column, metrics, last_index = NULL, preprocess = FALSE){
  if(preprocess){
    data <- data %>% 
      dplyr::select_(.dots = c("day", series_column, metrics)) %>% 
      dplyr::distinct_() %>% 
      dplyr::select_(.dots = c(series_column, metrics)) %>% 
      dplyr::group_by_(series_column) %>% 
      dplyr::summarize_all(coefficient_of_variation_stats) %>% 
      reshape2::melt(id = series_column, var = "metric")
  } else{
    data <- data %>% 
      dplyr::group_by_(series_column, "metric") %>% 
      dplyr::summarize(value = coefficient_of_variation_stats(value))
  }
  
  plot_function <- function(data) {
    p <- ggplot2::ggplot(data, aes(x = metric, y = value)) +
      ggplot2::geom_violin(fill = "lightsteelblue2", draw_quantiles = c(0.25, 0.5, 0.75)) +
      ggplot2::stat_summary(fun.data = mean_sdl, geom = "pointrange", color = "black") +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::labs(x = "Metric", y = "Coefficient of Variation", 
                    caption = "Horizontal lines are quartiles and vertical line with dot is mean +/- standard deviation") +
      case_study_theme()
    return(p)
  }
  
  if(!is.null(last_index)){
    df1 <- data %>% 
      dplyr::filter(metric %in% metrics[1:last_index])
    df2 <- data %>% 
      dplyr::filter(metric %in% metrics[last_index + 1:length(metrics)])
    
    result <- cowplot::plot_grid(plot_function(df1), plot_function(df2), ncol = 1)
  } else{
    result <- plot_function(data)
  }
  
  return(result)
}

#' @title TS Data Reshape for EDA
#' @description Reshapes the TS data for EDA purposes only
#' @author Stefanie Molin
#' 
#' @import dplyr 
#' 
#' @param data Wide dataframe (unmelted)
#' 
#' @return A dataframe
#' 
#' @export
#' 
reshape_TS_data_for_EDA <- function(data){
  site_type <- data %>% 
    dplyr::mutate(metric = site_type) %>% 
    dplyr::select(series, period, metric, day, value)
  
  event_name <- data %>% 
    dplyr::mutate(metric = event_name) %>% 
    dplyr::select(series, period, metric, day, value)
  
  results <- dplyr::union(site_type, event_name)
  
  return(results)
}

#' @title Seasonality Violin Plots
#' @description Creates violin plots showing the distribution of the metrics by day of week
#' @author Stefanie Molin
#' 
#' @import ggplot2
#' @import dplyr 
#' @importFrom reshape2 melt
#' @importFrom cowplot plot_grid
#' 
#' @param AS_data Long dataframe of AS data
#' @param TS_data Dataframe of TS data
#' @param RexT_data Territory RexT dataframe
#'
#' @return A plot object
#' 
#' @export
#' 
seasonality_violin_plots <- function(AS_data, TS_data, RexT_data){
  week_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  kpi_levels <- c("client_rext", "margin", "rext_euro", "spend", "tac", "order_value",
                  "cos", "cr", "ctr", "clicks", "conversions", "displays", 
                  "basket", "homepage", "listing", "product", "sales", "search",
                  "aa", "aios", "d", "m", "t", "SITE LEVEL", "", 
                  "ARGENTINA", "BRAZIL", "CANADA", "CHILE", "COLOMBIA", "MEXICO", 
                  "SAM OTHER", "US", "LATAM", "NORTH AMERICA", "AMERICAS")
  
  AS_data <- AS_data %>% 
    dplyr::distinct(series, day, kpi, value)  %>% 
    dplyr::select(day, kpi, value)
  
  TS_data <- TS_data %>% 
    dplyr::distinct(series, day, metric, value) %>% 
    dplyr::rename(kpi = metric) %>% 
    dplyr::select(day, kpi, value)
  
  RexT_data <- RexT_data %>% 
    dplyr::distinct(series, day, value) %>% 
    dplyr::rename(kpi = series) %>% 
    dplyr::select(day, kpi, value)
  
  data <- rbind(AS_data, TS_data, RexT_data)  %>% 
    dplyr::mutate(day_of_week = factor(strftime(day, format = "%a"), 
                                       levels = week_levels, 
                                       ordered = TRUE))
  
  
  p <- ggplot2::ggplot(data, aes(x = day_of_week, y = value)) +
    ggplot2::geom_violin(fill = "lightsteelblue2", draw_quantiles = c(0.25, 0.5, 0.75)) +
    ggplot2::facet_wrap(~ factor(kpi, levels = kpi_levels, ordered = TRUE), drop = FALSE, scales = "free_y", ncol = 3) +
    ggplot2::stat_summary(fun.data = mean_sdl, geom = "pointrange", color = "black") +
    ggplot2::scale_y_log10() +
    ggplot2::labs(x = "Day of Week", y = "Log of value", 
                  caption = "Horizontal lines are quartiles and vertical line with dot is mean +/- standard deviation") +
    case_study_theme()
  
  return(p)
}

#' @title Correlation Calculations
#' @description Correlations of AS, TS, and RexT metrics
#' @author Stefanie Molin
#' 
#' @import dplyr 
#' @importFrom reshape2 dcast
#' 
#' @param AS_data Wide dataframe of AS data
#' @param TS_data Dataframe of TS data
#' @param RexT_data Territory RexT dataframe
#'
#' @return A list of correlation data
#' 
#' @export
#' 
metrics_correlation_data <- function(AS_data, TS_data, RexT_data) {
  AS_correlations <- AS_data %>% 
    dplyr::select(-run_date) %>% 
    dplyr::distinct() %>% 
    dplyr::select(clicks:tac) %>% 
    cor()
  
  TS_correlations_site_type <- TS_data %>% 
    dplyr::filter(metric %in% c("d", "t", "m", "SITE LEVEL", "aa", "aios")) %>% 
    dplyr::mutate(series = substr(series, 1, 3)) %>% 
    reshape2::dcast(series + day ~ metric, fun.aggregate = sum) %>% 
    dplyr::select(-series, -day) %>% 
    cor()
  
  TS_correlations_event_name <- TS_data %>% 
    dplyr::filter(!(metric %in% c("d", "t", "m", "SITE LEVEL", "aa", "aios"))) %>% 
    dplyr::mutate(series = substr(series, 1, 3)) %>%
    reshape2::dcast(series + day ~ metric, fun.aggregate = sum) %>% 
    dplyr::select(-series, -day) %>% 
    cor()
  
  RexT_correlations <- RexT_data %>% 
    reshape2::dcast(day ~ series, fun.aggregate = sum) %>% 
    dplyr::select(-day) %>% 
    cor()
  
  return(list(AS = AS_correlations, 
              event_name = TS_correlations_event_name,
              site_type = TS_correlations_site_type,
              RexT = RexT_correlations))
}

#' @title Correlation Matrix
#' @description Correlation of AS, TS, and RexT metrics
#' @author Stefanie Molin
#' 
#' @importFrom corrplot corrplot
#' 
#' @param data Correlation data
#'
#' @return A correlation matrix
#' 
#' @export
#' 
correlation_matrix <- function(data) {
  corrplot::corrplot(data, method = "ellipse", addCoef.col = "#1a1a1a", 
                     diag = FALSE, number.cex = 0.75, mar = c(0,0,0,0), 
                     tl.offset = 0.2, tl.cex = 0.5, tl.col = "black")
}
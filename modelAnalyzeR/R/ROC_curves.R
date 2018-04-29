##################################################
######          ROC Curves Library          ######
######          by: Stefanie Molin          ######
####                                          ####
## Functions for drawing ROC curves for models  ##
## with and without threholds to trigger alerts.##
##################################################

#' @title Get False Positive Rate
#' @description Calculate false positive rate (FPR)
#' @author Stefanie Molin
#'
#' @param false_positives Number of false positives
#' @param true_negatives Number of true negatives
#'
#' @return FPR
#'
false_positive_rate <- function(false_positives, true_negatives){
  return(false_positives/(false_positives + true_negatives))
}

#' @title Get True Positive Rate
#' @description Calculate true positive rate (TPR)
#' @author Stefanie Molin
#'
#' @param true_positives Number of true positives
#' @param false_negatives Number of false negatives
#'
#' @return TPR
#'
true_positive_rate <- function(true_positives, false_negatives){
  return(true_positives/(true_positives + false_negatives))
}

#' @title Get False Positive Rate using Thresholds
#' @description Calculate false positive rate (FPR)
#' @author Stefanie Molin
#'
#' @param data Dataframe with a prediction and is_alert column
#' @param threshold Threshold for the prediction to be an alert (greater than or equal to)
#' @param prediction_column Column name of the prediction values
#'
#' @return FPR
#'
fpr_threshold <- function(data, threshold, prediction_column){
  false_positives <- sum(data[, prediction_column] >= threshold & !(data$is_alert))
  true_negatives <- sum(data[, prediction_column] < threshold & !(data$is_alert))
  return(false_positive_rate(false_positives = false_positives, true_negatives = true_negatives))
}

#' @title Get True Positive Rate using Thresholds
#' @description Calculate true positive rate (TPR)
#' @author Stefanie Molin
#'
#' @param data Dataframe with a prediction and is_alert column
#' @param threshold Threshold for the prediction to be an alert (greater than or equal to)
#' @param prediction_column Column name of the prediction values
#'
#' @return TPR
#'
tpr_threshold <- function(data, threshold, prediction_column){
  true_positives <- sum(data[, prediction_column] >= threshold & data$is_alert)
  false_negatives <- sum(data[, prediction_column] < threshold & data$is_alert)
  return(true_positive_rate(true_positives = true_positives, false_negatives = false_negatives))
}

#' @title Prep ROC Data for points graph
#' @description Format data for ROC points graph
#' @author Stefanie Molin
#'
#' @import dplyr
#' 
#' @param data Dataframe to use for ROC calculation
#' @param name What to name the series in the graph
#' @param facet_column The name of the column to use for the facets
#'
#' @return TPR
#'
prep_ROC_data <- function(data, name, facet_column = NULL){
  data <- data %>% 
    mutate(series = name, FPR = false_positive_rate(FP, TN), TPR = true_positive_rate(TP, FN))
  
  if(is.null(facet_column)){
    data <- data %>% 
      select(series, FPR, TPR)
  } else{
    data <- data %>% 
      select_(facet_column, "series", "FPR", "TPR")
  }
  
  return(data)
}

#' @title Plot ROC points
#' @description Graph results on ROC as points when thresholds aren't used by the model to determine alerts
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import ggplot2
#' 
#' @param AS_data Comparison results of AS alerts to the logs.
#' @param TS_data Comparison results of TS alerts to the logs.
#' @param exec_data Comparison results of exec alerts to the logs.
#' @param method_name Name of the method of triggering alerts. Used as subtitle for graph.
#'
#' @return ROC Curve plot
#'
#' @export
ROC_curve_points <- function(AS_data, TS_data, exec_data, method_name){
  AS_data <- prep_ROC_data(AS_data, "AS")
  TS_data <- prep_ROC_data(TS_data, "TS")
  exec_data <- prep_ROC_data(exec_data, 'RexT')
  
  p <- rbind(AS_data, TS_data, exec_data) %>% 
    ggplot2::ggplot(aes(x = FPR, y = TPR, col = series)) + 
    ggplot2::geom_point() + 
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) + 
    ggplot2::geom_abline(slope = 1, intercept = 0, color = 'gray', linetype = 'dashed') +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = 'False Positive Rate', y = 'True Positive Rate', col = '', 
                  title = 'ROC Curve by Product', subtitle = method_name,
                  caption = '*Note: Dashed line represents ROC curve of randomly triggering anomalies.') +
    case_study_theme()
  
  return(p)
}

#' @title Plot ROC points Faceted
#' @description Graph results on ROC as points when thresholds aren't used by the model to determine alerts
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import ggplot2
#' 
#' @param AS_data Comparison results of AS alerts to the logs.
#' @param TS_data Comparison results of TS alerts to the logs.
#' @param exec_data Comparison results of exec alerts to the logs.
#' @param facet_column The name of the column to use for the facets
#' @param method_name Name of the method of triggering alerts. Used as subtitle for graph.
#'
#' @return ROC Curve plot
#'
#' @export
ROC_curve_points_facet <- function(AS_data, TS_data, exec_data, facet_column, method_name){
  AS_data <- prep_ROC_data(AS_data, "AS", facet_column = facet_column)
  TS_data <- prep_ROC_data(TS_data, "TS", facet_column = facet_column)
  exec_data <- prep_ROC_data(exec_data, 'RexT', facet_column = facet_column)
  
  p <- rbind(AS_data, TS_data, exec_data) %>% 
    ggplot2::ggplot(aes(x = FPR, y = TPR, col = series)) + 
    ggplot2::geom_point() + 
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) + 
    ggplot2::geom_abline(slope = 1, intercept = 0, color = 'gray', linetype = 'dashed') +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = 'False Positive Rate', y = 'True Positive Rate', col = '', 
                  title = 'ROC Curve by Product', subtitle = method_name,
                  caption = '*Note: Dashed line represents ROC curve of randomly triggering anomalies.') +
    case_study_theme() +
    facet_wrap_theme() +
    ggplot2::facet_wrap(as.formula(paste("~", facet_column)), nrow = 1)
  
  return(p)
}

#' @title Plot ROC Curve
#' @description Graph results on ROC curve for given thresholds
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import ggplot2
#' 
#' @param data Data with column for metric used with threshold to determine alert
#' @param logs Log data from Metis.
#' @param curve_name_column String name of the column to use as a series (each line of the curve).
#' @param prediction_column String name of the column to use as a the prediction.
#' @param method_name Name of the method of triggering alerts. Used as subtitle for graph.
#' @param graph_title Title of the graph. Defaults to "ROC Curve by Product"
#' @param thresholds Vector of thresholds to use for drawing the ROC curve. Defaults to 0-100\%.
#'
#' @return ROC Curve plot
#'
#' @export
ROC_curve <- function(data, logs, curve_name_column, prediction_column, method_name, graph_title = "ROC Curve by Product", thresholds = seq(from = 0, to = 1, by = 0.01)){
  # join data to logs data
  all_data <- data %>% 
    dplyr::inner_join(logs %>% select(series, run_date, kpi, is_alert), 
                      by = c("series", "run_date", "kpi")) %>% 
    dplyr::select_(curve_name_column, prediction_column, "is_alert")
  
  # calculate the points for the ROC curve
  roc <- data.frame()
  for(curve_identifier in unique(all_data[, curve_name_column])){
    df <- all_data %>% dplyr::filter_(paste0(curve_name_column, " == '", curve_identifier, "'"))
    results <- data.frame(series = curve_identifier, alert_threshold = thresholds)
    results$FPR <- sapply(thresholds, function(x) fpr_threshold(df, x, prediction_column))
    results$TPR <- sapply(thresholds, function(x) tpr_threshold(df, x, prediction_column))
    roc <- rbind(roc, results)
  }

  p <- roc %>%
    ggplot2::ggplot(aes(x = FPR, y = TPR, col = series)) +
    ggplot2::geom_line(size = 0.8) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = 'gray', linetype = 'dashed') +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = 'False Positive Rate', y = 'True Positive Rate', col = '',
                  title = graph_title, subtitle = method_name,
                  caption = '*Note: Dashed line represents ROC curve of randomly triggering anomalies.') +
    case_study_theme()
  
  return(list(roc = roc, plot = p))
}

#' @title Plot ROC Curve with Facets
#' @description Graph results on ROC curve for given thresholds
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import ggplot2
#' 
#' @param data Data with column for metric used with threshold to determine alert
#' @param logs Log data from Metis.
#' @param facet_column The name of the column to use for the facets
#' @param curve_name_column String name of the column to use as a series (each line of the curve).
#' @param prediction_column String name of the column to use as a the prediction.
#' @param method_name Name of the method of triggering alerts. Used as subtitle for graph.
#' @param graph_title Title of the graph. Defaults to "ROC Curve by Product"
#' @param thresholds Vector of thresholds to use for drawing the ROC curve. Defaults to 0-100\%.
#'
#' @return ROC Curve plot
#'
#' @export
ROC_curve_facet <- function(data, logs, facet_column, curve_name_column, prediction_column, method_name, graph_title = "ROC Curve by Product", thresholds = seq(from = 0, to = 1, by = 0.01)){
  # join data to logs data
  all_data <- data %>% 
    dplyr::inner_join(logs %>% select(series, run_date, kpi, is_alert), 
                      by = c("series", "run_date", "kpi")) %>% 
    dplyr::select_(facet_column, curve_name_column, prediction_column, "is_alert")
  
  # calculate the points for the ROC curve
  roc <- data.frame()
  for(facet in unique(all_data[, facet_column])){
    for(curve_identifier in unique(all_data[, curve_name_column])){
      df <- all_data %>% dplyr::filter_(paste0(curve_name_column, " == '", curve_identifier, "'"), paste0(facet_column, " == '", facet, "'"))
      results <- data.frame(facet = facet, series = curve_identifier, alert_threshold = thresholds)
      results$FPR <- sapply(thresholds, function(x) fpr_threshold(df, x, prediction_column))
      results$TPR <- sapply(thresholds, function(x) tpr_threshold(df, x, prediction_column))
      roc <- rbind(roc, results)
    }
  }
  
  
  p <- roc %>%
    ggplot2::ggplot(aes(x = FPR, y = TPR, col = series)) +
    ggplot2::geom_line(size = 0.8) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = 'gray', linetype = 'dashed') +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = 'False Positive Rate', y = 'True Positive Rate', col = '',
                  title = graph_title, subtitle = method_name,
                  caption = '*Note: Dashed line represents ROC curve of randomly triggering anomalies.') +
    case_study_theme() +
    facet_wrap_theme() +
    ggplot2::facet_wrap(~facet, nrow = 1)
  
  return(list(roc = roc, plot = p))
}
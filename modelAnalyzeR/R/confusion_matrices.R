##################################################
######        Confusion Matrix Library      ######
######           by: Stefanie Molin         ######
####                                          ####
## Functions for calculating the results.       ##
##################################################

#' @title Get True Positives
#' @description Calculate true positives
#' @author Stefanie Molin
#'
#' @param conf_mat A confusion matrix
#'
#' @return The number of true positives
#'
true_positives <- function(conf_mat){
  return(conf_mat$table["TRUE", "TRUE"])
}

#' @title Get False Positives
#' @description Calculate false positives
#' @author Stefanie Molin
#'
#' @param conf_mat A confusion matrix
#'
#' @return The number of false positives
#'
false_positives <- function(conf_mat){
  return(conf_mat$table["TRUE", "FALSE"])
}

#' @title Get True Negatives
#' @description Calculate true negatives
#' @author Stefanie Molin
#'
#' @param conf_mat A confusion matrix
#'
#' @return The number of true negatives
#'
true_negatives <- function(conf_mat){
  return(conf_mat$table["FALSE", "FALSE"])
}

#' @title Get False Negatives
#' @description Calculate false negatives
#' @author Stefanie Molin
#'
#' @param conf_mat A confusion matrix
#'
#' @return The number of false negatives
#'
false_negatives <- function(conf_mat){
  return(conf_mat$table["FALSE", "TRUE"])
}

#' @title Get Precision
#' @description Calculate precision
#' @author Stefanie Molin
#'
#' @param conf_mat A confusion matrix
#'
#' @return The precision metric
#'
precision <- function(conf_mat){
  return(conf_mat$byClass["Precision"])
}

#' @title Get Recall
#' @description Calculate recall
#' @author Stefanie Molin
#'
#' @param conf_mat A confusion matrix
#'
#' @return The recall metric
#'
recall <- function(conf_mat){
  return(conf_mat$byClass["Recall"])
}

#' @title Get F1 Score
#' @description Calculate F1 score
#' @author Stefanie Molin
#'
#' @param conf_mat A confusion matrix
#'
#' @return The F1 Score metric
#'
f1_score <- function(conf_mat){
  return(conf_mat$byClass["F1"])
}

#' @title Get Specificity
#' @description Calculate specificity
#' @author Stefanie Molin
#'
#' @param conf_mat A confusion matrix
#'
#' @return The specificity
#'
specificity <- function(conf_mat){
  return(conf_mat$byClass["Specificity"])
}

#' @title Get Accuracy
#' @description Calculate accuracy
#' @author Stefanie Molin
#'
#' @param conf_mat A confusion matrix
#'
#' @return The accuracy
#'
accuracy <- function(conf_mat){
  return(conf_mat$overall["Accuracy"])
}

#' @title Calculate Precision
#' @description Calculate precision
#' @author Stefanie Molin
#'
#' @param TP Number of true positives
#' @param FP Number of false positives
#'
#' @return The precision
#'
calculate_precision <- function(TP, FP){
  return(TP/(TP + FP))
}

#' @title Calculate Recall
#' @description Calculate recall
#' @author Stefanie Molin
#'
#' @param TP Number of true positives
#' @param FN Number of false negatives
#'
#' @return The recall
#'
calculate_recall <- function(TP, FN){
  return(TP/(TP + FN))
}
#' @title Calculate F1-score
#' @description Calculate F1-score
#' @author Stefanie Molin
#'
#' @param precision The precision
#' @param recall The recall
#'
#' @return The F1-score
#'
calculate_F1_score <- function(precision, recall){
  return(2 * (precision * recall)/(precision + recall))
}

#' @title Calculate Specificity
#' @description Calculate specificity
#' @author Stefanie Molin
#'
#' @param TN Number of true negatives 
#' @param FP Number of false positives
#'
#' @return The specificity
#'
calculate_specificity <- function(TN, FP){
  return(TN/(TN + FP))
}

#' @title Calculate Accuracy
#' @description Calculate accuracy
#' @author Stefanie Molin
#'
#' @param TP Number of true positives
#' @param TN Number of true negatives 
#' @param FP Number of false positives
#' @param FN Number of false negatives
#'
#' @return The accuracy
#'
calculate_accuracy <- function(TP, TN, FP, FN){
  return((TP + TN)/(TP + TN + FP + FN))
}

#' @title AS Period Confusion Matrix
#' @description Calculate AS confusion matrix by date
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import caret
#'
#' @param beginning Dataframe of AS results for beginning of the month
#' @param middle Dataframe of AS results for middle of the month
#' @param end Dataframe of AS results for end of the month
#' @param logs Dataframe of logs from Metis
#'
#' @return A dataframe
#'
#' @export
AS_overall_confusion_matrix <- function(beginning, middle, end, logs){
  # collect average opinion from the logs
  from_metis <- logs %>% select(series, kpi, is_alert, run_date) %>% 
    group_by(series, kpi, run_date) %>% 
    summarize(is_alert = mean(is_alert) >= .5)
  
  beginning <- beginning %>% inner_join(from_metis, by = c("series", "kpi", "run_date"))
  middle <- middle %>% inner_join(from_metis, by = c("series", "kpi", "run_date"))
  end <- end %>% inner_join(from_metis, by = c("series", "kpi", "run_date"))
  
  conf_mat_beginning <- confusionMatrix(factor(beginning$is_alert.x, levels = c(TRUE, FALSE)), 
                                        factor(beginning$is_alert.y, levels = c(TRUE, FALSE)), positive = "TRUE")
  conf_mat_middle <- confusionMatrix(factor(middle$is_alert.x, levels = c(TRUE, FALSE)), 
                                     factor(middle$is_alert.y, levels = c(TRUE, FALSE)), positive = "TRUE")
  conf_mat_end <- confusionMatrix(factor(end$is_alert.x, levels = c(TRUE, FALSE)), 
                                  factor(end$is_alert.y, levels = c(TRUE, FALSE)), positive = "TRUE")
  
  results <- data.frame(Date = c("Beginning", "Middle", "End"),
                        TP = c(true_positives(conf_mat_beginning),
                               true_positives(conf_mat_middle),
                               true_positives(conf_mat_end)),
                        FP = c(false_positives(conf_mat_beginning),
                               false_positives(conf_mat_middle),
                               false_positives(conf_mat_end)),
                        TN = c(true_negatives(conf_mat_beginning),
                               true_negatives(conf_mat_middle),
                               true_negatives(conf_mat_end)),
                        FN = c(false_negatives(conf_mat_beginning),
                               false_negatives(conf_mat_middle),
                               false_negatives(conf_mat_end)),
                        Precision = c(precision(conf_mat_beginning),
                                      precision(conf_mat_middle),
                                      precision(conf_mat_end)),
                        Recall = c(recall(conf_mat_beginning),
                                   recall(conf_mat_middle),
                                   recall(conf_mat_end)),
                        "F1-score" = c(f1_score(conf_mat_beginning),
                                      f1_score(conf_mat_middle),
                                      f1_score(conf_mat_end)),
                        Specificity = c(specificity(conf_mat_beginning),
                                        specificity(conf_mat_middle),
                                        specificity(conf_mat_end)),
                        Accuracy = c(accuracy(conf_mat_beginning),
                                   accuracy(conf_mat_middle),
                                   accuracy(conf_mat_end)),
                        check.names = FALSE)
  
  totals <- results %>% summarize(Date = "**TOTAL**", TP = sum(TP), FP = sum(FP), TN = sum(TN), FN = sum(FN),
                                  Precision = calculate_precision(TP = TP, FP = FP),
                                  Recall = calculate_recall(TP = TP, FN = FN),
                                  "F1-score" = calculate_F1_score(precision = Precision, recall = Recall),
                                  Specificity = calculate_specificity(TN = TN, FP = FP),
                                  Accuracy = calculate_accuracy(TP = TP, TN = TN, FP = FP, FN = FN))
  
  return(rbind(results, totals))
}

#' @title AS Alerts Period Confusion Matrix for Western Electric Rules
#' @description Calculate AS confusion matrix by date
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import caret
#' @import scopeR
#'
#' @param beginning Dataframe of AS results for beginning of the month
#' @param middle Dataframe of AS results for middle of the month
#' @param end Dataframe of AS results for end of the month
#' @param logs Dataframe of logs from Metis
#' @param rule Name(s) of the rule(s) to check for. This must match the output of the check for alerts function, i.e. "Rule 1" or "Stratification rule" 
#' (the "violation" part will be added automatically)
#' @param rules_order Vector of rules from first checked to last
#'
#' @return A dataframe
#'
#' @export
western_electric_AS_confusion_matrix <- function(beginning, middle, end, logs, rule, rules_order){
  # filter rules
  rule <- ifelse(grepl("violation", rule), rule, 
                 ifelse(grepl("[Rr]ule", rule), paste(rule, "violation"), paste(rule, "rule violation")))
  
  remaining_rules <- subsequent_western_electric_rules(rule = gsub(" violation", "", gsub(" rule", "", rule[length(rule)])), 
                                                       rules_order = rules_order, include = FALSE)
  
  beginning <- beginning %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  middle <- middle %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  end <- end %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  # run with function for base cases
  results <- AS_overall_confusion_matrix(beginning, middle, end, logs)
  
  return(results)
}

#' @title AS Metrics Confusion Matrix
#' @description Calculate AS confusion matrix by metric
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import caret
#' @import scopeR
#'
#' @param beginning Dataframe of AS results for beginning of the month
#' @param middle Dataframe of AS results for middle of the month
#' @param end Dataframe of AS results for end of the month
#' @param logs Dataframe of logs from Metis
#'
#' @return A dataframe
#'
#' @export
AS_metrics_confusion_matrix <- function(beginning, middle, end, logs){
  # collect average opinion from the logs
  from_metis <- logs %>% select(series, kpi, is_alert, run_date) %>% 
    group_by(series, kpi, run_date) %>% 
    summarize(is_alert = mean(is_alert) >= .5)
  
  prediction <- rbind(beginning, middle, end) %>% inner_join(from_metis, by = c("series", "kpi", "run_date"))
  
  results <- data.frame()
  
  for(metric in prediction %>% distinct(kpi) %>% unlist %>% as.character){
    # formatting of names
    if(metric %in% c("cr", "cos", "cr", "ctr", "tac")){
      list_entry <- toupper(metric)
    } else if(metric == "client_rext"){
      list_entry <- "RexT Local"
    } else if(metric == "rext_euro"){
      list_entry <- "RexT Euro"
    } else if(grepl("_", metric)){
      list_entry <- scopeR::capitalize_name(sub(pattern = "_", replacement = " ", x = metric))
    } else{
      list_entry <- scopeR::capitalize_name(metric)
    }
    
    metric_predictions <- prediction %>% filter(kpi == metric)
    
    conf_mat <- confusionMatrix(factor((metric_predictions %>% select(is_alert.x))[[1]], levels = c(TRUE, FALSE)), 
                                factor((metric_predictions %>% select(is_alert.y))[[1]], levels = c(TRUE, FALSE)), positive = "TRUE")
    
    results <- rbind(results, data.frame(Metric = list_entry, 
                                         TP = true_positives(conf_mat),
                                         FP = false_positives(conf_mat),
                                         TN = true_negatives(conf_mat),
                                         FN = false_negatives(conf_mat),
                                         Precision = precision(conf_mat),
                                         Recall = recall(conf_mat),
                                         "F1-score" = f1_score(conf_mat),
                                         Specificity = specificity(conf_mat),
                                         Accuracy = accuracy(conf_mat),
                                         check.names = FALSE, row.names = ""))
  }
  
  totals <- results %>% summarize(Metric = "**Total**", TP = sum(TP), FP = sum(FP), TN = sum(TN), FN = sum(FN),
                                  Precision = calculate_precision(TP = TP, FP = FP),
                                  Recall = calculate_recall(TP = TP, FN = FN),
                                  "F1-score" = calculate_F1_score(precision = Precision, recall = Recall),
                                  Specificity = calculate_specificity(TN = TN, FP = FP),
                                  Accuracy = calculate_accuracy(TP = TP, TN = TN, FP = FP, FN = FN))
  
  return(rbind(results, totals))
}

#' @title AS Alerts Period Confusion Matrix for Western Electric Rules
#' @description Calculate AS confusion matrix by date
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import caret
#' @import scopeR
#'
#' @param beginning Dataframe of AS results for beginning of the month
#' @param middle Dataframe of AS results for middle of the month
#' @param end Dataframe of AS results for end of the month
#' @param logs Dataframe of logs from Metis
#' @param rule Name(s) of the rule(s) to check for. This must match the output of the check for alerts function, i.e. "Rule 1" or "Stratification rule" 
#' (the "violation" part will be added automatically)
#' @param rules_order Vector of rules from first checked to last
#'
#' @return A dataframe
#'
#' @export
western_electric_AS_metrics_confusion_matrix <- function(beginning, middle, end, logs, rule, rules_order){
  # filter rules
  rule <- ifelse(grepl("violation", rule), rule, 
                 ifelse(grepl("[Rr]ule", rule), paste(rule, "violation"), paste(rule, "rule violation")))
  
  remaining_rules <- subsequent_western_electric_rules(rule = gsub(" violation", "", gsub(" rule", "", rule[length(rule)])), 
                                                       rules_order = rules_order, include = FALSE)
  
  beginning <- beginning %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  middle <- middle %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  end <- end %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  # run with function for base cases
  results <- AS_metrics_confusion_matrix(beginning, middle, end, logs)
  
  return(results)
}

#' @title TS Period Confusion Matrix
#' @description Calculate TS confusion matrix by date
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import caret
#'
#' @param beginning Dataframe of TS results for beginning of the month
#' @param middle Dataframe of TS results for middle of the month
#' @param end Dataframe of TS results for end of the month
#' @param logs Dataframe of logs from Metis
#'
#' @return A dataframe
#'
#' @export
TS_overall_confusion_matrix <- function(beginning, middle, end, logs){
  # collect average opinion from the logs
  from_metis <- logs %>% select(series, site_type, event_name, is_alert, run_date) %>% 
    group_by(series, site_type, event_name, run_date) %>% 
    summarize(is_alert = mean(is_alert) >= .5)
  
  beginning <- beginning %>% inner_join(from_metis, by = c("series", "site_type", "event_name", "run_date"))
  middle <- middle %>% inner_join(from_metis, by = c("series", "site_type", "event_name", "run_date"))
  end <- end %>% inner_join(from_metis, by = c("series", "site_type", "event_name", "run_date"))
  
  conf_mat_beginning <- confusionMatrix(factor(beginning$is_alert.x, levels = c(TRUE, FALSE)), 
                                        factor(beginning$is_alert.y, levels = c(TRUE, FALSE)), positive = "TRUE")
  conf_mat_middle <- confusionMatrix(factor(middle$is_alert.x, levels = c(TRUE, FALSE)), 
                                     factor(middle$is_alert.y, levels = c(TRUE, FALSE)), positive = "TRUE")
  conf_mat_end <- confusionMatrix(factor(end$is_alert.x, levels = c(TRUE, FALSE)), 
                                  factor(end$is_alert.y, levels = c(TRUE, FALSE)), positive = "TRUE")
  
  results <- data.frame(Date = c("Beginning", "Middle", "End"),
                        TP = c(true_positives(conf_mat_beginning),
                               true_positives(conf_mat_middle),
                               true_positives(conf_mat_end)),
                        FP = c(false_positives(conf_mat_beginning),
                               false_positives(conf_mat_middle),
                               false_positives(conf_mat_end)),
                        TN = c(true_negatives(conf_mat_beginning),
                               true_negatives(conf_mat_middle),
                               true_negatives(conf_mat_end)),
                        FN = c(false_negatives(conf_mat_beginning),
                               false_negatives(conf_mat_middle),
                               false_negatives(conf_mat_end)),
                        Precision = c(precision(conf_mat_beginning),
                                      precision(conf_mat_middle),
                                      precision(conf_mat_end)),
                        Recall = c(recall(conf_mat_beginning),
                                   recall(conf_mat_middle),
                                   recall(conf_mat_end)),
                        "F1-score" = c(f1_score(conf_mat_beginning),
                                       f1_score(conf_mat_middle),
                                       f1_score(conf_mat_end)),
                        Specificity = c(specificity(conf_mat_beginning),
                                        specificity(conf_mat_middle),
                                        specificity(conf_mat_end)),
                        Accuracy = c(accuracy(conf_mat_beginning),
                                     accuracy(conf_mat_middle),
                                     accuracy(conf_mat_end)),
                        check.names = FALSE)
  
  totals <- results %>% summarize(Date = "**TOTAL**", TP = sum(TP), FP = sum(FP), TN = sum(TN), FN = sum(FN),
                                  Precision = calculate_precision(TP = TP, FP = FP),
                                  Recall = calculate_recall(TP = TP, FN = FN),
                                  "F1-score" = calculate_F1_score(precision = Precision, recall = Recall),
                                  Specificity = calculate_specificity(TN = TN, FP = FP),
                                  Accuracy = calculate_accuracy(TP = TP, TN = TN, FP = FP, FN = FN))
  
  return(rbind(results, totals))
}

#' @title TS Alerts Period Confusion Matrix for Western Electric Rules
#' @description Calculate TS confusion matrix by date
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import caret
#' @import scopeR
#'
#' @param beginning Dataframe of TS results for beginning of the month
#' @param middle Dataframe of TS results for middle of the month
#' @param end Dataframe of TS results for end of the month
#' @param logs Dataframe of logs from Metis
#' @param rule Name(s) of the rule(s) to check for. This must match the output of the check for alerts function, i.e. "Rule 1" or "Stratification rule" 
#' (the "violation" part will be added automatically)
#' @param rules_order Vector of rules from first checked to last
#'
#' @return A dataframe
#'
#' @export
western_electric_TS_confusion_matrix <- function(beginning, middle, end, logs, rule, rules_order){
  # filter rules
  rule <- ifelse(grepl("violation", rule), rule, 
                 ifelse(grepl("[Rr]ule", rule), paste(rule, "violation"), paste(rule, "rule violation")))
  
  remaining_rules <- subsequent_western_electric_rules(rule = gsub(" violation", "", gsub(" rule", "", rule[length(rule)])), 
                                                       rules_order = rules_order, include = FALSE)
  
  beginning <- beginning %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  middle <- middle %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  end <- end %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  # run with function for base cases
  results <- TS_overall_confusion_matrix(beginning, middle, end, logs)
  
  return(results)
}

#' @title TS Metrics Confusion Matrix
#' @description Calculate TS confusion matrix by metric
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import caret
#' @import scopeR
#'
#' @param beginning Dataframe of TS results for beginning of the month
#' @param middle Dataframe of TS results for middle of the month
#' @param end Dataframe of TS results for end of the month
#' @param logs Dataframe of logs from Metis
#'
#' @return A dataframe
#'
#' @export
TS_metrics_confusion_matrix <- function(beginning, middle, end, logs){
  # collect average opinion from the logs
  from_metis <- logs %>% filter(event_name != 'N/A' & site_type != 'N/A') %>% 
    select(series, site_type, event_name, is_alert, run_date) %>% 
    group_by(series, site_type, event_name, run_date) %>% 
    summarize(is_alert = mean(is_alert) >= .5)
  
  prediction <- rbind(beginning, middle, end) %>% 
    inner_join(from_metis, by = c("series", "site_type", "event_name", "run_date"))
  
  site_type_results <- data.frame()
  event_name_results <- data.frame()
  
  for(metric in prediction %>% distinct(site_type) %>% unlist %>% as.character){
    # formatting of names
    if(metric == "SITE LEVEL"){
      list_entry <- scopeR::capitalize_name(metric)
    } else{
      list_entry <- toupper(metric)
    }
    
    metric_predictions <- prediction %>% filter(site_type == metric)
    
    conf_mat <- confusionMatrix(factor((metric_predictions %>% select(is_alert.x))[[1]], levels = c(TRUE, FALSE)), 
                                factor((metric_predictions %>% select(is_alert.y))[[1]], levels = c(TRUE, FALSE)), positive = "TRUE")
    
    site_type_results <- rbind(site_type_results, data.frame(Metric = list_entry, 
                                                             TP = true_positives(conf_mat),
                                                             FP = false_positives(conf_mat),
                                                             TN = true_negatives(conf_mat),
                                                             FN = false_negatives(conf_mat),
                                                             Precision = precision(conf_mat),
                                                             Recall = recall(conf_mat),
                                                             "F1-score" = f1_score(conf_mat),
                                                             Specificity = specificity(conf_mat),
                                                             Accuracy = accuracy(conf_mat),
                                                             check.names = FALSE, row.names = ""))
  }
  
  site_type_totals <- site_type_results %>% summarize(Metric = "**Total**", TP = sum(TP), FP = sum(FP), TN = sum(TN), FN = sum(FN),
                                                      Precision = calculate_precision(TP = TP, FP = FP),
                                                      Recall = calculate_recall(TP = TP, FN = FN),
                                                      "F1-score" = calculate_F1_score(precision = Precision, recall = Recall),
                                                      Specificity = calculate_specificity(TN = TN, FP = FP),
                                                      Accuracy = calculate_accuracy(TP = TP, TN = TN, FP = FP, FN = FN))
  
  for(metric in prediction %>% distinct(event_name) %>% unlist %>% as.character){
    # formatting of names
    list_entry <- scopeR::capitalize_name(metric)
    
    metric_predictions <- prediction %>% filter(event_name == metric)
    
    conf_mat <- confusionMatrix(factor((metric_predictions %>% select(is_alert.x))[[1]], levels = c(TRUE, FALSE)), 
                                factor((metric_predictions %>% select(is_alert.y))[[1]], levels = c(TRUE, FALSE)), positive = "TRUE")
    
    event_name_results <- rbind(event_name_results, data.frame(Metric = list_entry, 
                                                               TP = true_positives(conf_mat),
                                                               FP = false_positives(conf_mat),
                                                               TN = true_negatives(conf_mat),
                                                               FN = false_negatives(conf_mat),
                                                               Precision = precision(conf_mat),
                                                               Recall = recall(conf_mat),
                                                               "F1-score" = f1_score(conf_mat),
                                                               Specificity = specificity(conf_mat),
                                                               Accuracy = accuracy(conf_mat),
                                                               check.names = FALSE, row.names = ""))
  }
  
  event_name_totals <- event_name_results %>% summarize(Metric = "**TOTAL**", TP = sum(TP), FP = sum(FP), TN = sum(TN), FN = sum(FN),
                                                        Precision = calculate_precision(TP = TP, FP = FP),
                                                        Recall = calculate_recall(TP = TP, FN = FN),
                                                        "F1-score" = calculate_F1_score(precision = Precision, recall = Recall),
                                                        Specificity = calculate_specificity(TN = TN, FP = FP),
                                                        Accuracy = calculate_accuracy(TP = TP, TN = TN, FP = FP, FN = FN))
  
  return(list(event_name = rbind(event_name_results, event_name_totals), site_type = rbind(site_type_results, site_type_totals)))
}

#' @title TS Alerts Metrics Confusion Matrix for Western Electric Rules
#' @description Calculate TS confusion matrix by metric
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import caret
#' @import scopeR
#'
#' @param beginning Dataframe of TS results for beginning of the month
#' @param middle Dataframe of TS results for middle of the month
#' @param end Dataframe of TS results for end of the month
#' @param logs Dataframe of logs from Metis
#' @param rule Name(s) of the rule(s) to check for. This must match the output of the check for alerts function, i.e. "Rule 1" or "Stratification rule" 
#' (the "violation" part will be added automatically)
#' @param rules_order Vector of rules from first checked to last
#'
#' @return A dataframe
#'
#' @export
western_electric_TS_metrics_confusion_matrix <- function(beginning, middle, end, logs, rule, rules_order){
  # filter rules
  rule <- ifelse(grepl("violation", rule), rule, 
                 ifelse(grepl("[Rr]ule", rule), paste(rule, "violation"), paste(rule, "rule violation")))
  
  remaining_rules <- subsequent_western_electric_rules(rule = gsub(" violation", "", gsub(" rule", "", rule[length(rule)])), 
                                                       rules_order = rules_order, include = FALSE)
  
  beginning <- beginning %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  middle <- middle %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  end <- end %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  # run with function for base cases
  results <- TS_metrics_confusion_matrix(beginning, middle, end, logs)
  
  return(results)
}

#' @title Territory RexT Period Confusion Matrix
#' @description Calculate Territory RexT confusion matrix by date
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import caret
#'
#' @param data Dataframe of territory RexT results
#' @param logs Dataframe of logs from Metis
#' @param beginning_dates Vector of dates for beginning of the month
#' @param middle_dates Vector of dates for middle of the month
#' @param end_dates Vector of dates for end of the month
#'
#' @return A dataframe
#'
#' @export
exec_overall_confusion_matrix <- function(data, logs, beginning_dates, middle_dates, end_dates){
  # collect average opinion from the logs
  from_metis <- logs %>% dplyr::filter(kpi == 'territory_rext') %>% 
    select(country, subregion, region, ranking, run_date, is_alert) %>% 
    group_by(country, subregion, region, ranking, run_date) %>% 
    summarize(is_alert = mean(is_alert) >= .5)
  
  data <- data %>% 
    dplyr::inner_join(from_metis, by = c("country", "subregion", "region", "ranking", "run_date"))
  
  beginning <- data %>% dplyr::filter(run_date %in% as.Date(beginning_dates))
  middle <- data %>% dplyr::filter(run_date %in% as.Date(middle_dates))
  end <- data %>% dplyr::filter(run_date %in% as.Date(end_dates))
  
  conf_mat_beginning <- confusionMatrix(factor(beginning$is_alert.x, levels = c(TRUE, FALSE)), 
                                        factor(beginning$is_alert.y, levels = c(TRUE, FALSE)), positive = "TRUE")
  conf_mat_middle <- confusionMatrix(factor(middle$is_alert.x, levels = c(TRUE, FALSE)), 
                                     factor(middle$is_alert.y, levels = c(TRUE, FALSE)), positive = "TRUE")
  conf_mat_end <- confusionMatrix(factor(end$is_alert.x, levels = c(TRUE, FALSE)), 
                                  factor(end$is_alert.y, levels = c(TRUE, FALSE)), positive = "TRUE")
  
  results <- data.frame(Date = c("Beginning", "Middle", "End"),
                        TP = c(true_positives(conf_mat_beginning),
                               true_positives(conf_mat_middle),
                               true_positives(conf_mat_end)),
                        FP = c(false_positives(conf_mat_beginning),
                               false_positives(conf_mat_middle),
                               false_positives(conf_mat_end)),
                        TN = c(true_negatives(conf_mat_beginning),
                               true_negatives(conf_mat_middle),
                               true_negatives(conf_mat_end)),
                        FN = c(false_negatives(conf_mat_beginning),
                               false_negatives(conf_mat_middle),
                               false_negatives(conf_mat_end)),
                        Precision = c(precision(conf_mat_beginning),
                                      precision(conf_mat_middle),
                                      precision(conf_mat_end)),
                        Recall = c(recall(conf_mat_beginning),
                                   recall(conf_mat_middle),
                                   recall(conf_mat_end)),
                        "F1-score" = c(f1_score(conf_mat_beginning),
                                       f1_score(conf_mat_middle),
                                       f1_score(conf_mat_end)),
                        Specificity = c(specificity(conf_mat_beginning),
                                        specificity(conf_mat_middle),
                                        specificity(conf_mat_end)),
                        Accuracy = c(accuracy(conf_mat_beginning),
                                     accuracy(conf_mat_middle),
                                     accuracy(conf_mat_end)),
                        check.names = FALSE)
  
  totals <- results %>% summarize(Date = "**TOTAL**", TP = sum(TP), FP = sum(FP), TN = sum(TN), FN = sum(FN),
                                  Precision = calculate_precision(TP = TP, FP = FP),
                                  Recall = calculate_recall(TP = TP, FN = FN),
                                  "F1-score" = calculate_F1_score(precision = Precision, recall = Recall),
                                  Specificity = calculate_specificity(TN = TN, FP = FP),
                                  Accuracy = calculate_accuracy(TP = TP, TN = TN, FP = FP, FN = FN))

  return(rbind(results, totals))
}

#' @title Territory RexT Period Confusion Matrix for Western Electric Rules
#' @description Calculate Territory RexT confusion matrix by date
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import caret
#' @import scopeR
#'
#' @param data Dataframe of territory RexT results
#' @param logs Dataframe of logs from Metis
#' @param beginning_dates Vector of dates for beginning of the month
#' @param middle_dates Vector of dates for middle of the month
#' @param end_dates Vector of dates for end of the month
#' @param rule Name(s) of the rule(s) to check for. This must match the output of the check for alerts function, i.e. "Rule 1" or "Stratification rule" 
#' (the "violation" part will be added automatically)
#' @param rules_order Vector of rules from first checked to last
#'
#' @return A dataframe
#'
#' @export
western_electric_exec_confusion_matrix <- function(data, logs, beginning_dates, middle_dates, end_dates, rule, rules_order){
  # filter rules
  rule <- ifelse(grepl("violation", rule), rule, 
                 ifelse(grepl("[Rr]ule", rule), paste(rule, "violation"), paste(rule, "rule violation")))
  
  remaining_rules <- subsequent_western_electric_rules(rule = gsub(" violation", "", gsub(" rule", "", rule[length(rule)])), 
                                                       rules_order = rules_order, include = FALSE)
  
  
  data <- data %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  # run with function for base cases
  results <- exec_overall_confusion_matrix(data, logs, beginning_dates, middle_dates, end_dates)
  
  return(results)
}

#' @title Territory RexT Metrics Confusion Matrix
#' @description Calculate Territory RexT confusion matrix by metric
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import caret
#' @import scopeR
#'
#' @param data Dataframe of territory RexT results
#' @param logs Dataframe of logs from Metis
#'
#' @return A dataframe
#'
#' @export
exec_territories_confusion_matrix <- function(data, logs){
  # collect average opinion from the logs
  from_metis <- logs %>% dplyr::filter(kpi == 'territory_rext') %>% 
    select(country, subregion, region, ranking, run_date, is_alert) %>% 
    group_by(country, subregion, region, ranking, run_date) %>% 
    summarize(is_alert = mean(is_alert) >= .5)
  
  data <- data %>% 
    dplyr::inner_join(from_metis, by = c("country", "subregion", "region", "ranking", "run_date")) %>% 
    dplyr::mutate(global_series = ifelse(as.character(country) == 'N/A', 
                                         ifelse(as.character(subregion) == 'N/A', as.character(region), as.character(subregion)), 
                                         as.character(country)))
  
  df <- data.frame()
  
  for(territory in data %>% distinct(global_series) %>% arrange(global_series) %>% unlist %>% as.character){
    territory_predictions <- data %>% filter(global_series == territory)
    
    conf_mat <- confusionMatrix(factor((territory_predictions %>% select(is_alert.x))[[1]], levels = c(TRUE, FALSE)), 
                                factor((territory_predictions %>% select(is_alert.y))[[1]], levels = c(TRUE, FALSE)), positive = "TRUE")
    
    df <- rbind(df, data.frame(Territory = territory, 
                               TP = true_positives(conf_mat),
                               FP = false_positives(conf_mat),
                               TN = true_negatives(conf_mat),
                               FN = false_negatives(conf_mat),
                               Precision = precision(conf_mat),
                               Recall = recall(conf_mat),
                               "F1-score" = f1_score(conf_mat),
                               Specificity = specificity(conf_mat),
                               Accuracy = accuracy(conf_mat),
                               check.names = FALSE, row.names = ""))
  }
  
  totals <- df %>% summarize(Territory = "**Total**", TP = sum(TP), FP = sum(FP), TN = sum(TN), FN = sum(FN),
                             Precision = calculate_precision(TP = TP, FP = FP),
                             Recall = calculate_recall(TP = TP, FN = FN),
                             "F1-score" = calculate_F1_score(precision = Precision, recall = Recall),
                             Specificity = calculate_specificity(TN = TN, FP = FP),
                             Accuracy = calculate_accuracy(TP = TP, TN = TN, FP = FP, FN = FN))
  
  return(rbind(df, totals))
}

#' @title Territory RexT Metrics Confusion Matrix for Western Electric Rules
#' @description Calculate Territory RexT confusion matrix by metric
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import caret
#' @import scopeR
#'
#' @param data Dataframe of territory RexT results
#' @param logs Dataframe of logs from Metis
#' @param rule Name(s) of the rule(s) to check for. This must match the output of the check for alerts function, i.e. "Rule 1" or "Stratification rule" 
#' (the "violation" part will be added automatically)
#' @param rules_order Vector of rules from first checked to last
#'
#' @return A dataframe
#'
#' @export
western_electric_exec_territories_confusion_matrix <- function(data, logs, rule, rules_order){
  # filter rules
  rule <- ifelse(grepl("violation", rule), rule, 
                 ifelse(grepl("[Rr]ule", rule), paste(rule, "violation"), paste(rule, "rule violation")))
  
  remaining_rules <- subsequent_western_electric_rules(rule = gsub(" violation", "", gsub(" rule", "", rule[length(rule)])), 
                                                       rules_order = rules_order, include = FALSE)
  
  
  data <- data %>% 
    dplyr::mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, "N/A", reason),
                  is_alert = ifelse(reason == "N/A", FALSE, is_alert))
  
  # run with function for base cases
  results <- exec_territories_confusion_matrix(data, logs)
  
  return(results)
}

#' @title Confusion Matrix by Rule for Western Electric Rules
#' @description Calculate confusion matrix for given rule across AS, TS, and RexT
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import caret
#'
#' @param AS_data Dataframe of AS results for beginning of the month
#' @param TS_data Dataframe of AS results for middle of the month
#' @param RexT_data Dataframe of AS results for end of the month
#' @param logs Dataframe of logs from Metis
#' @param rule Name(s) of the rule(s) to check for. This must match the output of the check for alerts function, i.e. "Rule 1" or "Stratification rule" 
#' (the "violation" part will be added automatically)
#' @param rules_order Vector of rules from first checked to last
#'
#' @return A dataframe
#'
#' @export
western_electric_rule_confusion_matrix <- function(AS_data, TS_data, RexT_data, logs, rule, rules_order){
  rule <- ifelse(grepl("violation", rule), rule, 
                 ifelse(grepl("[Rr]ule", rule), paste(rule, "violation"), paste(rule, "rule violation")))
  
  # collect average opinion from the logs
  from_metis_AS <- logs %>% select(series, kpi, is_alert, run_date) %>% 
    group_by(series, kpi, run_date) %>% 
    summarize(is_alert = mean(is_alert) >= .5)
  from_metis_TS <- logs %>% filter(event_name != 'N/A' & site_type != 'N/A') %>% 
    select(series, site_type, event_name, is_alert, run_date) %>% 
    group_by(series, site_type, event_name, run_date) %>% 
    summarize(is_alert = mean(is_alert) >= .5)
  from_metis_RexT <- logs %>% dplyr::filter(kpi == 'territory_rext') %>% 
    select(country, subregion, region, ranking, run_date, is_alert) %>% 
    group_by(country, subregion, region, ranking, run_date) %>% 
    summarize(is_alert = mean(is_alert) >= .5)
  
  # need to mark all alerts triggered after the last alert as not alerts (bc they wouldn't have been caught with the specified cutoff)
  remaining_rules <- subsequent_western_electric_rules(rule = gsub(" violation", "", gsub(" rule", "", rule[length(rule)])), rules_order = rules_order, include = FALSE)

  AS_data <- AS_data %>% inner_join(from_metis_AS, by = c("series", "kpi", "run_date")) %>%
    mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, 
                                "N/A", reason),
           is_alert.x = ifelse(reason == "N/A", FALSE, is_alert.x))
  TS_data <- TS_data %>% inner_join(from_metis_TS, by = c("series", "site_type", "event_name", "run_date"))  %>%
    mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, 
                           "N/A", reason),
           is_alert.x = ifelse(reason == "N/A", FALSE, is_alert.x))
  RexT_data <- RexT_data %>% inner_join(from_metis_RexT, by = c("country", "subregion", "region", "ranking", "run_date"))  %>%
    mutate(reason = ifelse(gsub(" violation", "", gsub(" rule", "", reason)) %in% remaining_rules, 
                           "N/A", reason),
           is_alert.x = ifelse(reason == "N/A", FALSE, is_alert.x))
  
  conf_mat_AS <- confusionMatrix(factor(AS_data$is_alert.x, levels = c(TRUE, FALSE)), 
                                        factor(AS_data$is_alert.y, levels = c(TRUE, FALSE)), positive = "TRUE")
  conf_mat_TS <- confusionMatrix(factor(TS_data$is_alert.x, levels = c(TRUE, FALSE)), 
                                     factor(TS_data$is_alert.y, levels = c(TRUE, FALSE)), positive = "TRUE")
  conf_mat_RexT <- confusionMatrix(factor(RexT_data$is_alert.x, levels = c(TRUE, FALSE)), 
                                  factor(RexT_data$is_alert.y, levels = c(TRUE, FALSE)), positive = "TRUE")
  
  results <- data.frame(Series = c("AS", "TS", "RexT"),
                        TP = c(true_positives(conf_mat_AS),
                               true_positives(conf_mat_TS),
                               true_positives(conf_mat_RexT)),
                        FP = c(false_positives(conf_mat_AS),
                               false_positives(conf_mat_TS),
                               false_positives(conf_mat_RexT)),
                        TN = c(true_negatives(conf_mat_AS),
                               true_negatives(conf_mat_TS),
                               true_negatives(conf_mat_RexT)),
                        FN = c(false_negatives(conf_mat_AS),
                               false_negatives(conf_mat_TS),
                               false_negatives(conf_mat_RexT)),
                        Precision = c(precision(conf_mat_AS),
                                      precision(conf_mat_TS),
                                      precision(conf_mat_RexT)),
                        Recall = c(recall(conf_mat_AS),
                                   recall(conf_mat_TS),
                                   recall(conf_mat_RexT)),
                        "F1-score" = c(f1_score(conf_mat_AS),
                                       f1_score(conf_mat_TS),
                                       f1_score(conf_mat_RexT)),
                        Specificity = c(specificity(conf_mat_AS),
                                        specificity(conf_mat_TS),
                                        specificity(conf_mat_RexT)),
                        Accuracy = c(accuracy(conf_mat_AS),
                                     accuracy(conf_mat_TS),
                                     accuracy(conf_mat_RexT)),
                        check.names = FALSE)
  
  return(results)
}

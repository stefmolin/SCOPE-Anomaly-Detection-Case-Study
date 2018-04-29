#' @title Western Electric Rules
#' @description Determine if yesterday is an anomaly according to the Western Electric Rules.
#' @author Stefanie Molin
#'
#' @param data Vector to test
#'
#' @return A list with "anomaly"--a boolean indicating whether or not yesterday was an alert--and
#' "reason"--name of the rule that was violated if there was a violation, otherwise "N/A"
#'
#' @note More info here: \url{https://en.wikipedia.org/wiki/Western_Electric_rules} and here: 
#' \url{https://en.wikipedia.org/wiki/Nelson_rules}
#' 
#' @export

western_electric_rules <- function(data){
  # determine summary stats
  mean <- mean(data)
  sd <- sd(data)
  
  length <- length(data)
  last_value <- data[length]
  
  # calculate upper and lower control limits
  ucl <- mean + 3 * sd
  lcl <- mean - 3 * sd
  
  # stop checking if something is found to be anamolous 
  done_checking <- FALSE
  while(!done_checking){
    # Rule 1: check if last point is outside of the control limits
    if(last_value < lcl | last_value > ucl){
      anomaly <- TRUE
      reason <- "Rule 1 violation"
      break
    }
    
    # Rule 2: 2 out of 3 of the last points fall beyond 2 sigma on the same side of the centerline
    last_3_points <- data[(length - 2):length]
    beyond_2_sigma <- max(sum(last_3_points > mean + 2 * sd), sum(last_3_points < mean - 2 * sd))
    if(beyond_2_sigma >= 2){
      anomaly <- TRUE
      reason <- "Rule 2 violation"
      break
    }
    
    # Rule 3: 4 out of 5 of the last points fall beyond 1 sigma on the same side of the centerline
    last_5_points <- data[(length - 4):length]
    beyond_1_sigma <- max(sum(last_5_points > mean + sd), sum(last_5_points < mean - sd))
    if(beyond_1_sigma >= 4){
      anomaly <- TRUE
      reason <- "Rule 3 violation"
      break
    }
    
    # Trend Rule: 6 consecutive points going up or down
    last_6_points <- data[(length - 5):length]
    diffs <- diff(last_6_points)
    max_in_same_direction <- max(sum(diffs > 0), sum(diffs < 0))
    if(max_in_same_direction == length(diffs)){
      anomaly <- TRUE
      reason <- "Trend rule violation"
      break
    }
    
    # Mixture Rule: 8 consecutive points outside of +/- 1 SD on either side of the mean
    last_8_points <- data[(length - 7):length]
    mixture_count <- sum(last_8_points > mean + sd) + sum(last_8_points < mean - sd)
    if(mixture_count == 8){
      anomaly <- TRUE
      reason <- "Mixture rule violation"
      break
    }
    
    # Stratification Rule: 15 consecutive datapoints within +/- 1 SD on either side of the mean
    last_15_points <- data[(length - 14):length]
    stratificiation_count <- (last_15_points >= mean - sd & last_15_points <= mean + sd)
    if(sum(stratificiation_count) == 15){
      anomaly <- TRUE
      reason <- "Stratification rule violation"
      break
    }
    
    # Rule 4: 9 consecutive points fall on the same side of the mean
    last_9_points <- data[(length - 8):length]
    same_side_points <- max(sum(last_9_points > mean), sum(last_9_points < mean))
    if(same_side_points == 9){
      anomaly <- TRUE
      reason <- "Rule 4 violation"
      break
    }
    
    # Check for constant oscillation (noise): 
    # Fourteen (or more) points in a row alternate in direction, increasing then decreasing.
    last_14_points <- data[(length - 13):length]
    diffs <- diff(last_14_points)
    indices <- 1:length(diffs)
    even_indices <- diffs[indices %% 2 == 0]
    odd_indices <- diffs[indices %% 2 != 0]
    transformed <- c(even_indices, odd_indices * -1)
    if(max(sum(transformed > 0), sum(transformed < 0)) == length(diffs)){
      anomaly <- TRUE
      reason <- "Noise rule violation"
      break
    }
    
    # nothing found, return false
    anomaly <- FALSE
    reason <- "N/A"
    done_checking <- TRUE
  }
  return(list(is_alert = anomaly, reason = reason))
}

#' @title Run through AS Western Electricc Pipeline for Case Study
#' @description Clean data, impute the median for missing dates, and detect anomalies for all client/campaign/kpi combinations.
#' @author Stefanie Molin
#'
#' @importFrom data.table data.table setkeyv :=
#' @importFrom dplyr %>% filter distinct mutate group_by summarize
#' @importFrom reshape2 melt dcast
#' @importFrom futile.logger flog.info flog.debug
#' @import scopeR
#'
#' @param data Dataframe of daily data to check for anomalies.
#'
#' @note This function relies on the structure of the query that provides "data". 
#' If changing column names, adding or subtracting columns, be sure to update here as well.
#' 
#' @note Client/campaign must have TAC in each of the last 2 days and 25 rows in the database in the last 30 days
#'
#' @return A dataframe with the results
#' 
#' @export
western_electric_pipeline_AS <- function(data){
  tracked_kpis <- c("client_rext", "spend", "cos", "ctr", "cr", "clicks", 
                    "displays", "tac", "conversions", "order_value", "rext_euro", "margin")
  
  # unmelt the data to reuse most of the SCOPE code
  futile.logger::flog.info("Reshaping data...")
  data <- unmelt_AS(data)
  
  # find non-eligible accounts
  futile.logger::flog.info("Disqualifying accounts...")
  disqualified <- disqualify_accounts_for_case_study(data = data, series_column = "series",required_entries = 25)
  
  # clean data
  futile.logger::flog.info("Cleaning data...")
  data <- clean_data_for_case_study(data)
  
  # fill in missing dates
  futile.logger::flog.info("Filling in missing dates...")
  data <- fill_in_missing_dates_for_case_study(data, series_key_column = "series", metric_columns = tracked_kpis)
  
  # status update
  futile.logger::flog.info("Checking for anomalies...")
  
  # add identifying column
  data <- data %>% dplyr::mutate(series_and_date = paste(series, "on", run_date))
  
  # rearrange the data into a long dataframe
  melted <- reshape2::melt(data, measure.vars = tracked_kpis, variable.name = "kpi")
  
  # find combinations to test for anomalies
  results <- melted %>% 
    dplyr::distinct(series, client_name, client_id, global_account_name, RTC_vertical, vertical_name,
                    campaign_name, campaign_scenario, campaign_type_name, campaign_revenue_type, 
                    campaign_funnel_id, run_date, campaign_id, cost_center,
                    ranking, country, subregion, region, kpi, series_and_date) %>% 
    dplyr::mutate(is_alert = FALSE, reason = "N/A")
  
  # mark disqualified accounts as not alerts but keep them for final results
  disqualified <- dplyr::semi_join(results, disqualified, by = c("series", "run_date"))
  
  # don't iterate through disqualified accounts
  results <- dplyr::anti_join(results, disqualified, by = c("series", "run_date"))
  
  # make a data.table
  DT_results <- data.table::data.table(results)
  DT <- data.table::data.table(data)
  
  # set keys
  data.table::setkeyv(DT, "series_and_date")
  data.table::setkeyv(DT_results, c("series_and_date", "kpi"))
  to_test <- unique(DT_results$series_and_date)
  
  # find all anomalies (client/campaign/kpi combinations)
  for(combo in to_test){
    futile.logger::flog.info(paste("Checking", combo, "for anomalies"))
    for(metric in tracked_kpis){
      verdict <- western_electric_rules(unlist(DT[series_and_date == combo,
                                                  c(metric), with = FALSE]))
      DT_results[series_and_date == combo & kpi == metric, 
                 c("is_alert", "reason") := list(verdict$is_alert, verdict$reason)]
    }
  }
  
  return(rbind(as.data.frame(DT_results), disqualified))
}

#' @title Western Electric Rules TS Alerts
#' @description Check for TS anomalies using the Western Electric rules
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import AnomalyDetection
#' @importFrom futile.logger flog.info
#'
#' @param data Dataframe in case study format
#'
#' @return A dataframe
#'
#' @export
western_electric_pipeline_TS <- function(data){
  # metrics to check
  tracked_kpis <- c("site_events", "tag_events")
  
  # find non-eligible accounts
  disqualified <- disqualify_accounts_for_case_study(data = data, series_column = "series", is_AS_data = FALSE, required_entries = 25)
  
  # remove unknown site_type
  if(length(grep("unknown", data$site_type)) > 0){
    data <- data[-grep("unknown", data$site_type),]
  }
  
  # clean data
  futile.logger::flog.info("Cleaning data...")
  data <- clean_data_for_case_study(data)
  
  # add column to identify better series that are the same but the global_account_name is different
  data <- data %>% dplyr::mutate(global_series = paste0(global_account_name, ": ", series))
  
  # fill in missing dates
  futile.logger::flog.info("Filling in values for missing dates...")
  data <- fill_in_missing_dates_for_case_study(data = data, series_key_column = "global_series", metric_columns = "value")
  
  # find combinations to test for anomalies
  results <- data %>% 
    dplyr::distinct(global_series, series, global_account_name, RTC_vertical, vertical_name, partner_id, series_and_date,
                    partner_name, cost_center, ranking, country, subregion, region, site_type, event_name, run_date) %>% 
    dplyr::mutate(is_alert = FALSE, reason = "N/A")
  
  # mark disqualified accounts as not alerts but keep them for final results
  disqualified <- dplyr::semi_join(results, disqualified, by = c("series", "run_date"))
  
  # don't iterate through disqualified accounts
  results <- dplyr::anti_join(results, disqualified, by = c("series", "run_date"))
  
  # status update
  futile.logger::flog.info("Checking for anomalies...")
  
  # make a data.table
  DT_results <- data.table::data.table(results)
  DT <- data.table::data.table(data)
  
  # set keys
  data.table::setkeyv(DT, "series_and_date")
  data.table::setkeyv(DT_results, "series_and_date")
  to_test <- unique(DT_results$series_and_date)
  
  # find all anomalies
  for(combo in to_test){
    futile.logger::flog.info(paste("Checking", combo, "for anomalies"))
    
    series_data_vector <- unlist(DT[series_and_date == combo, c("value"), with = FALSE])
    
    verdict <- western_electric_rules(unlist(series_data_vector))
    
    DT_results[series_and_date == combo, 
               c("is_alert", "reason") := list(verdict$is_alert, verdict$reason)]
    
  }
  
  return(rbind(as.data.frame(DT_results), disqualified))
}

#' @title RexT Western Electric Rules Anomaly Detection
#' @description Check for Territory RexT anomalies using the Western Electric rules
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import AnomalyDetection
#' @importFrom futile.logger flog.info
#'
#' @param data Dataframe in case study format
#'
#' @return A dataframe
#'
#' @export
western_electric_pipeline_RexT <- function(data){
  # clean data
  flog.info("Cleaning data...")
  data <- clean_data_for_case_study(data)
  
  # add column to identify better series that are the same but the global_account_name is different
  data <- data %>% dplyr::mutate(global_series = paste(ifelse(as.character(country) == 'N/A', 
                                                              ifelse(as.character(subregion) == 'N/A', as.character(region), as.character(subregion)), 
                                                              as.character(country)), "in", ranking, "for", run_date))
  
  # fill in missing dates
  flog.info("Filling in values for missing dates...")
  data <- fill_in_missing_dates_for_case_study(data = data, series_key_column = "global_series", metric_columns = "value")
  
  # status update
  futile.logger::flog.info("Checking for anomalies...")
  
  # find combinations to test for anomalies
  results <- data %>% 
    dplyr::distinct(global_series, country, subregion, region, ranking, run_date) %>% 
    dplyr::mutate(is_alert = FALSE, reason = "N/A")
  
  # make a data.table
  DT_results <- data.table::data.table(results)
  DT <- data.table::data.table(data)
  
  # set keys
  data.table::setkeyv(DT, "global_series")
  data.table::setkeyv(DT_results, "global_series")
  to_test <- unique(DT_results$global_series)
  
  
  # find all anomalies
  for(combo in to_test){
    futile.logger::flog.info(paste("Checking", combo, "for anomalies"))
    
    series_data_vector <- unlist(DT[global_series == combo, c("value"), with = FALSE])
    
    verdict <- western_electric_rules(unlist(series_data_vector))
    
    DT_results[global_series == combo, 
               c("is_alert", "reason") := list(verdict$is_alert, verdict$reason)]
  }
  
  return(as.data.frame(DT_results))
}

#' @title Subsequent Western Electric Rules
#' @description Return the current rules and all rules that would follow this one.
#' @author Stefanie Molin
#'
#' @param rule Name of the rule to start with
#' @param rules_order Vector of rules from first checked to last
#' @param include Boolean indicating whether or not to include the rule provided in the list. Defaults to TRUE (include it)
#'
#' @return A vector of rules or "N/A" if there are no rules and the provided one isn't included
#'
#' @export
#' 
subsequent_western_electric_rules <- function(rule, rules_order, include = TRUE){
  if(include){
    results <- rules_order[rule <= factor(rules_order, levels = rules_order, ordered=TRUE)]
  } else{
    results <- rules_order[rule < factor(rules_order, levels = rules_order, ordered=TRUE)]
    if(length(results) == 0){
      results <- "N/A"
    }
  }
  return(results)
}

#' @title Previous Western Electric Rules
#' @description Return the current rules and previous rules that would preceed this one.
#' @author Stefanie Molin
#'
#' @param rule Name of the rule to start with
#' @param rules_order Vector of rules from first checked to last
#' @param include Boolean indicating whether or not to include the rule provided in the list. Defaults to TRUE (include it)
#'
#' @return A vector of rules or "N/A" if there are no rules and the provided one isn't included
#'
#' @export
#' 
previous_western_electric_rules <- function(rule, rules_order, include = TRUE){
  if(include){
    results <- rules_order[rule >= factor(rules_order, levels = rules_order, ordered=TRUE)]
  } else{
    results <- rules_order[rule > factor(rules_order, levels = rules_order, ordered=TRUE)]
    if(length(results) == 0){
      results <- "N/A"
    }
  }
  return(results)
}
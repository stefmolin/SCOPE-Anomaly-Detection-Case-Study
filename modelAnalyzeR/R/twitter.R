#' @title AS Twitter Anomaly Detection
#' @description Test AS data for anomaly
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import scopeR
#' @import data.table
#' @import AnomalyDetection
#' @importFrom futile.logger flog.info
#' @importFrom reshape2 melt
#'
#' @param data Dataframe in case study format
#' @param period Integer defining the length of the period for seasonal decomposition
#'
#' @return A dataframe
#'
#' @export
twitter_anomaly_detection_AS <- function(data, period = NULL){
  futile.logger::flog.info("Prepping data")
  tracked_kpis <- c("client_rext", "spend", "cos", "ctr", "cr", "clicks", 
                    "displays", "tac", "conversions", "order_value", "rext_euro", "margin")
  
  # unmelt the data to reuse most of the SCOPE code
  data <- unmelt_AS(data)
  
  # find non-eligible accounts
  disqualified <- disqualify_accounts_for_case_study(data = data, series_column = "series", required_entries = 25)
  
  # clean data
  futile.logger::flog.info("Cleaning data...")
  data <- clean_data_for_case_study(data)
  
  # fill in missing dates
  futile.logger::flog.info("Filling in missing data...")
  data <- fill_in_missing_dates_for_case_study(data, series_key_column = "series", metric_columns = tracked_kpis)
  
  # rearrange the data into a long dataframe
  melted <- reshape2::melt(data, measure.vars = tracked_kpis, variable.name = "kpi")
  
  # find combinations to test for anomalies
  results <- melted %>% 
    dplyr::distinct(series, client_name, client_id, global_account_name, RTC_vertical, vertical_name,
                    campaign_name, campaign_scenario, campaign_type_name, campaign_revenue_type, 
                    campaign_funnel_id, run_date, campaign_id, cost_center,
                    ranking, country, subregion, region, kpi, series_and_date) %>% 
    dplyr::mutate(is_alert = FALSE)
  
  # mark disqualified accounts as not alerts but keep them for final results
  futile.logger::flog.info("Disqualifying series...")
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
      series_data_vector <- unlist(DT[series_and_date == combo, c(metric), with = FALSE])
      
      # determine period if not provided
      if(is.null(period)){
        period <- estimate_frequency(series_data_vector)
      }
      
      num_of_observations <- length(series_data_vector)
      
      DT_results[series_and_date == combo & kpi == metric, 
                 is_alert := (num_of_observations %in% 
                                AnomalyDetectionVec(series_data_vector, max_anoms = ceiling(1/num_of_observations * 100)/100, 
                                                    direction = 'both', plot = FALSE, alpha = 0.05, period = period)$anoms)]
    }
  }
  
  return(as.data.frame(DT_results))
}

#' @title TS Twitter Anomaly Detection
#' @description Check for TS anomalies using the Twitter algorithm
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import AnomalyDetection
#' @importFrom futile.logger flog.info
#'
#' @param data Dataframe in case study format
#' @param period Integer defining the length of the period for seasonal decomposition
#'
#' @return A dataframe
#'
#' @export
twitter_anomaly_detection_TS <- function(data, period = NULL){
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
    dplyr::mutate(is_alert = FALSE)
  
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
    
    # determine period if not provided
    if(is.null(period)){
      period <- estimate_frequency(series_data_vector)
    }
    
    num_of_observations <- length(series_data_vector)
    
    DT_results[series_and_date == combo, 
               is_alert := (num_of_observations %in% 
                              AnomalyDetectionVec(series_data_vector, max_anoms = ceiling(1/num_of_observations * 100)/100, direction = 'both', 
                                                  plot = FALSE, alpha = 0.05, period = period)$anoms)]
  }
  
  return(rbind(as.data.frame(DT_results), disqualified))
}

#' @title RexT Twitter Anomaly Detection
#' @description Check for Territory RexT anomalies using the Twitter algorithm
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import AnomalyDetection
#' @importFrom futile.logger flog.info
#'
#' @param data Dataframe in case study format
#' @param period Integer defining the length of the period for seasonal decomposition
#'
#' @return A dataframe
#'
#' @export
twitter_anomaly_detection_RexT <- function(data, period = NULL){
  # clean data
  flog.info("Cleaning data...")
  data <- clean_data_for_case_study(data)
  
  # add column to identify better series that are the same but the global_account_name is different
  data <- data %>% dplyr::mutate(global_series = paste(ifelse(as.character(country) == 'N/A', 
                                                              ifelse(as.character(subregion) == 'N/A', as.character(region), as.character(subregion)), 
                                                              as.character(country)), "in", ranking, "for", run_date))
  
  # fill in missing dates
  flog.info("Filling in values for missing dates...")
  data <- scopeR::fill_in_missing_dates(data = data, series_key_column = "global_series", metric_columns = "value")
  
  # status update
  futile.logger::flog.info("Checking for anomalies...")
  
  # find combinations to test for anomalies
  results <- data %>% 
    dplyr::distinct(global_series, country, subregion, region, ranking, run_date) %>% 
    dplyr::mutate(is_alert = FALSE)
  
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
    
    # determine period if not provided
    if(is.null(period)){
      period <- estimate_frequency(series_data_vector)
    }
    num_of_observations <- length(series_data_vector)
    
    DT_results[global_series == combo, 
               is_alert := (num_of_observations %in% 
                              AnomalyDetectionVec(series_data_vector, max_anoms = ceiling(1/num_of_observations * 100)/100, direction = 'both', 
                                                  plot = FALSE, alpha = 0.05, period = period)$anoms)]
    
  }
  
  return(as.data.frame(DT_results))
}
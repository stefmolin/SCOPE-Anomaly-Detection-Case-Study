##################################################
######     Percent Change Models Library    ######
######          by: Stefanie Molin          ######
####                                          ####
## Functions for percent change models.         ##
##################################################

#' @title Calculate Aggregations
#' @description Calculate aggregations be specified function at 7, 30, and 60 day lookbacks
#' @author Stefanie Molin
#'
#' @import dplyr
#' @importFrom reshape2 melt
#'
#' @param data Data to use for aggregations
#' @param FUN Function to use for aggregations
#'
#' @return A dataframe of aggregated data
#'
#' @export
calculate_aggregations <- function(data, FUN){
  # calculate the aggregations at 7, 30, and 60 days
  aggr_data <- data %>% 
    dplyr::select(series, day, run_date, kpi, value) %>% 
    dplyr::group_by(series, run_date, kpi) %>% 
    dplyr::summarize(yesterday = sum(ifelse(day == run_date - 1, value, NA), na.rm = TRUE), 
                     L7D = FUN(ifelse(day >= run_date - 8 & day < run_date - 1, value, NA), na.rm = TRUE),
                     L30D = FUN(ifelse(day >= run_date - 31 & day < run_date - 1, value, NA), na.rm = TRUE),
                     L60D = FUN(ifelse(day >= run_date - 61 & day < run_date - 1, value, NA), na.rm = TRUE))
  
  # reshape the data
  aggr_data <- reshape2::melt(aggr_data, measure.vars = c('L7D', 'L30D', 'L60D'), 
                              variable.name = 'aggregation_level', value.name = 'aggregated_value')
  
  # calculate the percent change from the aggregations
  aggr_data <- aggr_data %>% 
    dplyr::mutate(percent_change = (yesterday - aggregated_value)/aggregated_value) %>% 
    dplyr::select(series, run_date, kpi, aggregation_level, percent_change) %>% 
    dplyr::mutate(percent_change = abs(ifelse(is.na(percent_change), 0, 
                                          ifelse(is.nan(percent_change), 0, 
                                                 ifelse(is.infinite(percent_change), 0, percent_change)))))
  
  return(aggr_data)
}

#' @title Prep AS Data for Percent Change Models
#' @description Clean data and calculate percent changes by specific aggregation function.
#' @author Stefanie Molin
#'
#' @importFrom dplyr %>% filter distinct mutate group_by summarize
#' @importFrom reshape2 melt dcast
#' @importFrom futile.logger flog.info
#'
#' @param data Dataframe of daily data to check for anomalies.
#' @param FUN Function to aggregate by
#'
#' @note This function relies on the structure of the query that provides "data". 
#' If changing column names, adding or subtracting columns, be sure to update here as well.
#' 
#' @note Client/campaign must have TAC in each of the last 2 days and 25 rows in the database in the last 30 days
#'
#' @return Prepped data as dataframe
#'
#' @export
prep_AS_percent_change <- function(data, FUN){
  tracked_kpis <- c("client_rext", "spend", "cos", "ctr", "cr", "clicks", 
                    "displays", "tac", "conversions", "order_value", "rext_euro", "margin")
  
  # unmelt the data to reuse most of the SCOPE code
  futile.logger::flog.info("Unmelting data...")
  data <- unmelt_AS(data)
  
  # find non-eligible accounts
  futile.logger::flog.info("Disqualifying series...")
  disqualified <- disqualify_accounts_for_case_study(data = data, series_column = "series", required_entries = 25)
  
  # clean data
  futile.logger::flog.info("Cleaning data...")
  data <- clean_data_for_case_study(data)
  
  # fill in missing dates
  futile.logger::flog.info("Filling in missing dates...")
  data <- fill_in_missing_dates_for_case_study(data = data, series_key_column = "series", metric_columns = tracked_kpis)
  
  # rearrange the data into a long dataframe
  futile.logger::flog.info("Reshaping data...")
  melted <- reshape2::melt(data, measure.vars = tracked_kpis, variable.name = "kpi")

  # calculate aggregations
  futile.logger::flog.info("Calculating aggregations...")
  df <- calculate_aggregations(melted, FUN)

  # include disqualified metrics at 0% percent change so they won't be flagged
  disqualified <- dplyr::semi_join(df, disqualified, by = c("series", "run_date")) %>%
    dplyr::mutate(percent_change = 0)

  return(rbind(df, disqualified))
}

#' @title Prep TS Data for Percent Change Models
#' @description Clean data and calculate percent changes by specific aggregation function.
#' @author Stefanie Molin
#'
#' @importFrom dplyr %>% filter distinct mutate group_by summarize
#' @importFrom futile.logger flog.info
#'
#' @param data Dataframe of daily data to check for anomalies.
#' @param FUN Function to aggregate by
#'
#' @note This function relies on the structure of the query that provides "data". 
#' If changing column names, adding or subtracting columns, be sure to update here as well.
#' 
#' @return Prepped data as dataframe
#'
#' @export
prep_TS_percent_change <- function(data, FUN){
  # metrics to check
  tracked_kpis <- c("site_events", "tag_events")
  
  # find non-eligible accounts
  futile.logger::flog.info("Disqualifying series...")
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
  
  # calculate aggregations
  futile.logger::flog.info("Calculating aggregations...")
  data <- calculate_aggregations(data, FUN)
  
  # include disqualified metrics at 0% percent change so they won't be flagged
  disqualified <- dplyr::semi_join(data, disqualified, by = c("series", "run_date")) %>% 
    dplyr::mutate(percent_change = 0)
  
  return(rbind(data, disqualified))
}

#' @title Prep RexT Data for Percent Change Models
#' @description Clean data and calculate percent changes by specific aggregation function.
#' @author Stefanie Molin
#'
#' @importFrom dplyr %>% filter distinct mutate group_by summarize
#'
#' @param data Dataframe of daily data to check for anomalies.
#' @param FUN Function to aggregate by
#'
#' @note This function relies on the structure of the query that provides "data". 
#' If changing column names, adding or subtracting columns, be sure to update here as well.
#' 
#' @return Prepped data as dataframe
#'
#' @export
prep_RexT_percent_change <- function(data, FUN){
  # clean data
  data <- clean_data_for_case_study(data)
  
  # create a series column
  data <- data %>% 
    dplyr::mutate(series = ifelse(as.character(country) == 'N/A', 
                                  ifelse(as.character(subregion) == 'N/A', as.character(region), as.character(subregion)), 
                                  as.character(country)))
  
  # calculate aggregations
  data <- calculate_aggregations(data, FUN)
  
  return(data)
  
}

#' @title Flag Percent Change Alerts
#' @description Calculate alerts for percent difference in excess of the given threshold for the chosen lookback window
#' @author Stefanie Molin
#'
#' @import dplyr
#'
#' @param data Dataframe containing the percent change info
#' @param aggregation String specifying aggregation level to use. Can be 'L7D', 'L30D', or 'L60D'
#' @param threshold Percentage threshold for alerts as decimal.
#'
#' @return The provided dataframe with an 'is_alert' column added
#'
#' @export
flag_percent_change_alerts <- function(data, aggregation, threshold){
  data <- data %>% filter(aggregation_level == aggregation) %>% mutate(is_alert = abs(percent_change) >= threshold)
  
  return(data)
}
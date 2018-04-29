#' @title Prep AS Data for Transfer to Python for Modelling
#' @description Clean data and mark disqualifications for transfer to Python for modelling
#' @author Stefanie Molin
#'
#' @importFrom dplyr %>% filter distinct mutate group_by summarize
#' @importFrom reshape2 melt dcast
#' @importFrom futile.logger flog.info
#'
#' @param data Dataframe of daily data to check for anomalies.
#' @param days Number of days to require for model (others will be marked as disqualified)
#'
#' @note This function relies on the structure of the query that provides "data". 
#' If changing column names, adding or subtracting columns, be sure to update here as well.
#'
#' @return Prepped data as dataframe
#'
#' @export
prep_AS_for_python <- function(data, days){
  tracked_kpis <- c("client_rext", "spend", "cos", "ctr", "cr", "clicks", 
                    "displays", "tac", "conversions", "order_value", "rext_euro", "margin")
  
  # unmelt the data to reuse most of the SCOPE code
  futile.logger::flog.info("Unmelting data...")
  data <- unmelt_AS(data)
  
  # clean data
  futile.logger::flog.info("Cleaning data...")
  data <- clean_data_for_case_study(data)
  
  # fill in missing dates
  futile.logger::flog.info("Filling in missing dates...")
  data <- fill_in_missing_dates_for_case_study(data = data, series_key_column = "series", metric_columns = tracked_kpis)
  
  # find non-eligible accounts
  futile.logger::flog.info("Disqualifying series...")
  disqualified <- disqualify_accounts_for_case_study(data = data, series_column = "series", required_entries = days)
  
  # rearrange the data into a long dataframe and only keep entries where we have enough data, everything else will be marked disqualified
  futile.logger::flog.info("Reshaping data...")
  melted <- reshape2::melt(data, measure.vars = tracked_kpis, variable.name = "kpi") %>% 
    dplyr::filter(as.Date(day) >= as.Date(run_date) - days)
  
  # include disqualified metrics at 0% percent change so they won't be flagged
  disqualified <- dplyr::semi_join(melted, disqualified, by = c("series", "run_date")) %>%
    dplyr::mutate(disqualified = TRUE)
  
  # mark as valid
  df <- melted %>% 
    dplyr::anti_join(disqualified, by = c("series", "run_date")) %>% 
    dplyr::mutate(disqualified = FALSE)
  
  results <- rbind(df, disqualified)
  
  return(results)
}

#' @title Prep TS Data for Transfer to Python for Modelling
#' @description Clean data and mark disqualifications for transfer to Python for modelling
#' @author Stefanie Molin
#'
#' @importFrom dplyr %>% filter distinct mutate group_by summarize
#' @importFrom reshape2 melt dcast
#' @importFrom futile.logger flog.info
#'
#' @param data Dataframe of daily data to check for anomalies.
#' @param days Number of days to require for model (others will be marked as disqualified)
#'
#' @note This function relies on the structure of the query that provides "data". 
#' If changing column names, adding or subtracting columns, be sure to update here as well.
#'
#' @return Prepped data as dataframe
#'
#' @export
prep_TS_for_python <- function(data, days){
  # metrics to check
  tracked_kpis <- c("site_events", "tag_events")
  
  # find non-eligible accounts
  futile.logger::flog.info("Disqualifying series...")
  disqualified <- disqualify_accounts_for_case_study(data = data, series_column = "series", is_AS_data = FALSE, required_entries = days) 
  
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
  
  # include disqualified metrics at 0% percent change so they won't be flagged
  disqualified <- dplyr::semi_join(data, disqualified, by = c("series", "run_date")) %>% 
    dplyr::mutate(disqualified = TRUE)
  
  # final adjustment to dates and disqualified column
  data <- data  %>% 
    dplyr::anti_join(disqualified, by = c("series", "run_date"))  %>% 
    dplyr::mutate(disqualified = FALSE) %>% 
    dplyr::filter(as.Date(day) >= as.Date(run_date) - days)
  
  return(rbind(data, disqualified))
}

#' @title Prep RexT Data for Transfer to Python for Modelling
#' @description Clean data and mark disqualifications for transfer to Python for modelling
#' @author Stefanie Molin
#'
#' @importFrom dplyr %>% filter distinct mutate group_by summarize
#' @importFrom reshape2 melt dcast
#' @importFrom futile.logger flog.info
#'
#' @param data Dataframe of daily data to check for anomalies.
#' @param days Number of days to require for model (others will be marked as disqualified)
#'
#' @note This function relies on the structure of the query that provides "data". 
#' If changing column names, adding or subtracting columns, be sure to update here as well.
#'
#' @return Prepped data as dataframe
#'
#' @export
prep_RexT_for_python <- function(data, days){
  # clean data
  data <- clean_data_for_case_study(data)
  
  # create a series column
  data <- data %>% 
    dplyr::mutate(series = ifelse(as.character(country) == 'N/A', 
                                  ifelse(as.character(subregion) == 'N/A', as.character(region), as.character(subregion)), 
                                  as.character(country))) %>% 
    dplyr::filter(as.Date(day) >= as.Date(run_date) - days)
  
  return(data)
  
}
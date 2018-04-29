#' @title Clean Data
#' @description Replace Inf, NaN and NA values with 0
#' @author Stefanie Molin
#'
#' @param df Dataframe to clean
#'
#' @return A dataframe
#'
#' @export
clean_data_for_case_study <- function(df){
  for(column in names(df)){
    if(identical(column, "day") | identical(column, "run_date") | is.factor(column)){
      next
    }
    # remove NA, NaN and Inf values
    df[is.infinite(df[,column]), column] <- 0
    df[is.nan(df[,column]), column] <- 0
    df[is.na(df[,column]), column] <- 0
  }
  
  return(df)
}

#' @title Disqualify Accounts for Case Study
#' @description Return the series that aren't eligible for alerts (TAC of 0 for last 2 days and/or not live for 25 days in the last 30 days)
#' @author Stefanie Molin
#'
#' @import dplyr
#'
#' @param data Dataframe to search. Must have columns: tac and day.
#' @param series_column Column name of the series to check for disqualification
#' @param is_AS_data Boolean for whether or not this is AS data. AS data will have a TAC column for an additional disqualification.
#' @param required_entries Number of days the series must have data for in the last 30 days to be considered live
#'
#' @return A dataframe
#'
#' @export
disqualify_accounts_for_case_study <- function(data, series_column, is_AS_data = TRUE, required_entries = 25){
  # remove accounts that don't have data for yesterday
  has_data_for_yesterday <- data %>% 
    dplyr::filter(day == run_date - 1) %>% 
    dplyr::distinct_(series_column, "run_date")
  no_data_for_yesterday <- data %>% 
    dplyr::anti_join(has_data_for_yesterday, by = c(series_column, "run_date")) %>% 
    dplyr::distinct_(series_column, "run_date")
  
  if(is_AS_data){
    # handle disqualifications (only keep clients/campaigns that have positive tac on at least 1 of the last 2 days)
    data$tac <- round(data$tac, 2)
    
    disqualified_tac <- data %>% 
      dplyr::filter((day == run_date - 1 | day == run_date - 2)) %>% 
      dplyr::group_by_(series_column, "run_date") %>% 
      dplyr::summarize(tac = sum(tac)) %>% 
      dplyr::filter(tac <= 0) %>% 
      dplyr::distinct_(series_column, "run_date")
    
    no_data_for_yesterday <- dplyr::union(no_data_for_yesterday, disqualified_tac)
  }
  
  # require data for x out of the last 30 days
  disqualified_days_live <- data %>% 
    dplyr::filter(day >= run_date - 1 - (required_entries + 4)) %>% 
    dplyr::group_by_(series_column, "run_date") %>% 
    dplyr::summarize(entries = n()) %>% 
    dplyr::filter(entries < required_entries) %>% 
    dplyr::distinct_(series_column, "run_date")
  
  # all disqualifications
  disqualified <- dplyr::union(no_data_for_yesterday, disqualified_days_live)
  
  return(disqualified)
}

#' @title Fill In Missing Dates for Case Study
#' @description For any missing dates between the minimum and maximum dates in the time series, impute the median for the metrics.
#' @author Stefanie Molin
#'
#' @importFrom scopeR fill_in_missing_dates
#'
#' @param data Dataframe to check and fill for missing dates (see note for structure)
#' @param series_key_column The name of the column that defines a unique time series (to separate from the rest in the dataframe)
#' @param metric_columns The names of the columns with metrics to impute
#'
#' @note There must be a column "day" in data of type "Date". This adds a column for unique series and dates "series_and_date"
#'
#' @return A dataframe
#'
#' @export
fill_in_missing_dates_for_case_study <- function(data, series_key_column, metric_columns){
  # add the unique column that combines the series and the run_date
  data$series_and_date <- paste(data[, series_key_column], "on", data$run_date)
  
  all_run_dates <- unique(data$run_date)
  results <- data.frame()
  
  for(unique_run_date in all_run_dates){
    results <- rbind(results, scopeR::fill_in_missing_dates(data = data[data$run_date == unique_run_date,], series_key_column = "series_and_date", metric_columns = metric_columns))
  }
  
  return(results)
}
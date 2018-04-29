#######################################################
####  Chi-Square, Tukey Fence, More Extreme Method ####
####            Author: Stefanie Molin             ####
####                                               ####
## Pipelines to clean/prep data, check all series    ##
## for anomalies.                                    ##
#######################################################

#' @title Run through AS Detect Anomaly Pipeline for Case Study
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
#' @param yesterday Day to check for anomalies.
#'
#' @note This function relies on the structure of the query that provides "data". 
#' If changing column names, adding or subtracting columns, be sure to update here as well.
#' 
#' @note Client/campaign must have TAC in each of the last 2 days and 25 rows in the database in the last 30 days
#'
#' @return A list with 2 items: "results"--a dataframe with everything that was tested and whether or not it was an alert and 
#' "cleaned_data"--a dataframe containing the cleaned and prepped data that was used to check for anomalies.
#'
#' @export
chi_tukey_pipeline_AS <- function(data, yesterday){
  tracked_kpis <- c("client_rext", "spend", "cos", "ctr", "cr", "clicks", 
                    "displays", "tac", "conversions", "order_value", "rext_euro", "margin")
  
  # unmelt the data to reuse most of the SCOPE code
  data <- unmelt_AS(data)
  
  # find non-eligible accounts
  disqualified <- disqualify_accounts_for_case_study(data = data, series_column = "series",required_entries = 25)
  
  # note that by removing these accounts from the data here they won't be there for the dashboard (revisit later)
  # data <- dplyr::anti_join(data, disqualified, by = "series")
  
  # clean data
  futile.logger::flog.info("Cleaning data...")
  data <- clean_data_for_case_study(data)
  
  # fill in missing dates
  futile.logger::flog.info("Filling in missing dates...")
  data <- fill_in_missing_dates_for_case_study(data, series_key_column = "series", metric_columns = tracked_kpis)
  
  # status update
  futile.logger::flog.info("Checking for anomalies...")
  
  # rearrange the data into a long dataframe
  melted <- reshape2::melt(data, measure.vars = tracked_kpis, variable.name = "kpi")
  
  # find combinations to test for anomalies
  results <- melted %>% 
    dplyr::distinct(series, client_name, client_id, global_account_name, RTC_vertical, vertical_name,
                    campaign_name, campaign_scenario, campaign_type_name, campaign_revenue_type, 
                    campaign_funnel_id, run_date, campaign_id, cost_center,
                    ranking, country, subregion, region, kpi) %>% 
    dplyr::mutate(is_alert = FALSE)
  
  # mark disqualified accounts as not alerts but keep them for final results
  disqualified <- dplyr::semi_join(results, disqualified, by = c("series", "run_date"))
  
  # don't iterate through disqualified accounts
  results <- dplyr::anti_join(results, disqualified, by = c("series", "run_date"))
  
  # make a data.table
  DT_results <- data.table::data.table(results)
  DT <- data.table::data.table(data)
  
  # set keys
  data.table::setkeyv(DT, "series")
  data.table::setkeyv(DT_results, c("series", "kpi"))
  to_test <- unique(DT_results$series)
  
  # find all anomalies (client/campaign/kpi combinations)
  for(combo in to_test){
    futile.logger::flog.info(paste("Checking", combo, "for anomalies"))
    for(metric in tracked_kpis){
      DT_results[series == combo & kpi == metric, 
                 is_alert := (detect_anomaly(as.data.frame(DT[series == combo,
                                                              c("day", metric), with = FALSE]),
                                             metric = metric, yesterday = yesterday) & 
                                # make sure value deviates from median by more than 0.0001 for percentages and 1 for values
                                ifelse(length(DT[series == combo & day == yesterday, metric, with = FALSE][[1]]) == 1,
                                       abs(
                                         abs(median(DT[series == combo, metric, with = FALSE][[1]])) - 
                                           abs(DT[series == combo & day == yesterday, metric, with = FALSE])
                                       ) >= ifelse(metric %in% c("cos", "cr", "ctr", "margin"), 0.0001, 1), FALSE)) | 
                   # always flag RexT lower than -1000 euros (to avoid issues with highly inflated currencies like JPY, Colombian Pesos, etc.)
                   ifelse(grepl("rext", metric) & length(DT[series == combo & day == yesterday, rext_euro]) == 1,
                          DT[series == combo & day == yesterday, rext_euro] <= -1000, FALSE)]
    }
  }
  
  return(list(results = rbind(as.data.frame(DT_results), disqualified), cleaned_data = data))
}

#' @title Run through TS Detect Anomaly Pipeline for Case Study
#' @description  Clean data, impute the median for missing dates, and detect anomalies for all partner/kpi combinations.
#' @author Stefanie Molin
#' 
#' @importFrom futile.logger flog.info flog.debug
#' @importFrom data.table data.table setkeyv :=
#' @importFrom dplyr %>% filter distinct mutate group_by summarize
#' 
#' @param data Dataframe of the data to check for anomalies
#' @param yesterday Day to check for anomalies
#' 
#' @return A list with 2 items: "results"--a dataframe with everything that was tested and whether or not it was an alert and 
#' "cleaned_data"--a dataframe containing the cleaned and prepped data that was used to check for anomalies.
#'
#' @export
chi_tukey_pipeline_TS <- function(data, yesterday){
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
  flog.info("Filling in values for missing dates...")
  data <- scopeR::fill_in_missing_dates(data = data, series_key_column = "series", metric_columns = "value")
  
  # status update
  futile.logger::flog.info("Checking for anomalies...")
  
  # find combinations to test for anomalies
  results <- data %>% 
    dplyr::distinct(global_series, series, global_account_name, RTC_vertical, vertical_name, partner_id, 
                    partner_name, cost_center, ranking, country, subregion, region, site_type, event_name, run_date) %>% 
    dplyr::mutate(is_alert = FALSE)
  
  # mark disqualified accounts as not alerts but keep them for final results
  disqualified <- dplyr::semi_join(results, disqualified, by = c("series", "run_date"))
  
  # don't iterate through disqualified accounts
  results <- dplyr::anti_join(results, disqualified, by = c("series", "run_date"))
  
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
    
    DT_results[global_series == combo, 
               is_alert := (detect_anomaly(as.data.frame(DT[global_series == combo,
                                                            c("day", "value"), with = FALSE]),
                                           metric = "events", yesterday = yesterday) & 
                              # make sure value deviates from median by more than 1 for values
                              ifelse(length(DT[global_series == combo & day == yesterday, "value", with = FALSE][[1]]) == 1,
                                     abs(
                                       abs(median(DT[global_series == combo, "value", with = FALSE][[1]])) - 
                                         abs(DT[global_series == combo & day == yesterday, "value", with = FALSE])
                                     ) >= 1, FALSE))]
  }
  
  return(list(results = rbind(as.data.frame(DT_results), disqualified), cleaned_data = data))
}

#' @title Run through Exec Detect Anomaly Pipeline for Case Study
#' @description  Clean data, impute the median for missing dates, and detect anomalies for all territory RexT combinations.
#' @author Stefanie Molin
#' 
#' @importFrom futile.logger flog.info flog.debug
#' @importFrom data.table data.table setkeyv :=
#' @importFrom dplyr %>% filter distinct mutate group_by summarize
#' 
#' @param data Dataframe of the data to check for anomalies
#' 
#' @return A list with 2 items: "results"--a dataframe with everything that was tested and whether or not it was an alert and 
#' "cleaned_data"--a dataframe containing the cleaned and prepped data that was used to check for anomalies.
#'
#' @export
chi_tukey_pipeline_exec <- function(data){
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
    yesterday <- DT[global_series == combo, "run_date", with = FALSE][[1]] - 1
    
    futile.logger::flog.info(paste("Checking", combo, "for anomalies"))
    
    DT_results[global_series == combo, 
               is_alert := (detect_anomaly(as.data.frame(DT[global_series == combo,
                                                            c("day", "value"), with = FALSE]),
                                           metric = "exec", yesterday = yesterday) & 
                              # make sure value deviates from median by more than 1 for values
                              ifelse(length(DT[global_series == combo & day == yesterday, "value", with = FALSE][[1]]) == 1,
                                     abs(
                                       abs(median(DT[global_series == combo, "value", with = FALSE][[1]])) - 
                                         abs(DT[global_series == combo & day == yesterday, "value", with = FALSE])
                                     ) >= 1, FALSE))]
    
  }
  
  return(list(results = as.data.frame(DT_results), cleaned_data = data))
}
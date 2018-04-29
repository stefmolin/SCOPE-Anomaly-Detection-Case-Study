#' @title Unmelt AS Data Pulled from Metis
#' @description Unmelts data pulled from Metis to use with other anomaly detection methods.
#' @author Stefanie Molin
#'
#' @importFrom reshape2 dcast
#'
#' @param data Dataframe of AS data pulled from Metis
#'
#' @note This function relies on the structure of the query that provides "data". 
#' If changing column names, adding or subtracting columns, be sure to update here as well.
#'
#' @return Unmelted AS dataframe
#'
#' @export
unmelt_AS <- function(data){
  # unmelt the data to reuse most of the SCOPE code
  data <- reshape2::dcast(data, formula = series + client_name + client_id + global_account_name + 
                            RTC_vertical + vertical_name + campaign_id + campaign_name + 
                            campaign_scenario + campaign_type_name + campaign_revenue_type + 
                            campaign_funnel_id + cost_center + ranking + country + subregion + 
                            region + day + run_date + day_of_week + day_of_month + month_of_year + 
                            day_of_year + week_of_year ~ kpi, value.var = "value")
  return(data)
}

#' @title Unmelt TS Data Pulled from Metis
#' @description Unmelts data pulled from Metis to use with other anomaly detection methods.
#' @author Stefanie Molin
#'
#' @importFrom reshape2 dcast
#'
#' @param data Dataframe of TS data pulled from Metis
#'
#' @note This function relies on the structure of the query that provides "data". 
#' If changing column names, adding or subtracting columns, be sure to update here as well.
#'
#' @return Unmelted TS dataframe
#'
#' @export
unmelt_TS <- function(data){
  # unmelt the data to reuse most of the SCOPE code
  data <- reshape2::dcast(data, formula = series + global_account_name + RTC_vertical + vertical_name + 
                            partner_id + partner_name + cost_center + ranking + country + subregion + 
                            region + site_type + event_name + day + run_date + day_of_week + day_of_month + month_of_year + 
                            day_of_year + week_of_year ~ kpi, value.var = "value")
  return(data)
}

#' @title Unmelt RexT Data Pulled from Metis
#' @description Unmelts data pulled from Metis to use with other anomaly detection methods.
#' @author Stefanie Molin
#'
#' @importFrom reshape2 dcast
#' @import dplyr
#'
#' @param data Dataframe of RexT data pulled from Metis
#'
#' @note This function relies on the structure of the query that provides "data". 
#' If changing column names, adding or subtracting columns, be sure to update here as well.
#'
#' @return Unmelted RexT dataframe
#'
#' @export
unmelt_RexT <- function(data){
  data <- data %>% dplyr::mutate(series = ifelse(as.character(subregion) == 'N/A', 
                                                 as.character(region), 
                                                 ifelse(as.character(country) == "N/A", as.character(subregion), as.character(country))))
  
  # unmelt the data to reuse most of the SCOPE code
  data <- reshape2::dcast(data, formula = ranking + country + subregion + region + 
                            day + run_date + day_of_week + day_of_month + month_of_year + 
                            day_of_year + week_of_year ~ series + kpi, value.var = "value")
  return(data)
}
##################################################
######       Case Study Results Library     ######
######         by: Stefanie Molin           ######
####                                          ####
## Functions for calculating the results.       ##
##################################################

#' @title AS Alert Counts
#' @description Calculate AS client and campaign alert counts for case study.
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stringr str_extract
#'
#' @param beginning Dataframe of AS results for beginning of the month
#' @param middle Dataframe of AS results for middle of the month
#' @param end Dataframe of AS results for end of the month
#'
#' @return A list containing a dataframe of the counts ("counts"), graphs for counts for client and campaign (graph), and a boxplot of the results
#' 
#' @note The boxplot shows the distribution for series (clients or campaigns) that have been flagged, 
#' i.e. campaign alerts per client is the distribution of campaign alerts for clients that have alerts.
#'
#' @export
AS_alert_counts <- function(beginning, middle, end){
  beginning_client <- beginning %>% dplyr::filter(is_alert & campaign_name == "NOT A CAMPAIGN")
  beginning_campaign <- beginning %>% dplyr::filter(is_alert & campaign_name != "NOT A CAMPAIGN")
  middle_client <- middle %>% dplyr::filter(is_alert & campaign_name == "NOT A CAMPAIGN")
  middle_campaign <- middle %>% dplyr::filter(is_alert & campaign_name != "NOT A CAMPAIGN")
  end_client <- end %>% dplyr::filter(is_alert & campaign_name == "NOT A CAMPAIGN")
  end_campaign <- end %>% dplyr::filter(is_alert & campaign_name != "NOT A CAMPAIGN")
  
  df <- data.frame("Date" = c("Beginning", "Middle", "End"), 
                   "Clients with Alerts" = c(beginning_client %>% dplyr::distinct(client_name) %>% nrow, 
                                                  middle_client %>% dplyr::distinct(client_name) %>% nrow, 
                                                  end_client %>% dplyr::distinct(client_name) %>% nrow), 
                   "Median Alerts per Client" = c(median((beginning_client %>% dplyr::group_by(client_name) %>% 
                                                            dplyr::summarize(count = n()) %>% dplyr::select(count))[[1]]), 
                                                       median((middle_client %>% dplyr::group_by(client_name) %>% 
                                                                 dplyr::summarize(count = n()) %>% dplyr::select(count))[[1]]), 
                                                       median((end_client %>% dplyr::group_by(client_name) %>% 
                                                                 dplyr::summarize(count = n()) %>% dplyr::select(count))[[1]])),
                   "Median Campaign Alerts per Client" = c(median((beginning %>% dplyr::filter(is_alert) %>% dplyr::group_by(client_name) %>% 
                                                                     dplyr::summarize(campaign_alerts = sum(campaign_name != "NOT A CAMPAIGN")) %>% 
                                                                     dplyr::select(campaign_alerts))[[1]]),
                                                           median((middle %>% dplyr::filter(is_alert) %>% dplyr::group_by(client_name) %>% 
                                                                     dplyr::summarize(campaign_alerts = sum(campaign_name != "NOT A CAMPAIGN")) %>% 
                                                                     dplyr::select(campaign_alerts))[[1]]),
                                                           median((end %>% dplyr::filter(is_alert) %>% dplyr::group_by(client_name) %>%
                                                                     dplyr::summarize(campaign_alerts = sum(campaign_name != "NOT A CAMPAIGN")) %>% 
                                                                     dplyr::select(campaign_alerts))[[1]])),
                   "Campaigns with Alerts" = c(beginning_campaign %>% dplyr::distinct(series) %>% nrow, 
                                                  middle_campaign %>% dplyr::distinct(series) %>% nrow, 
                                                  end_campaign %>% dplyr::distinct(series) %>% nrow), 
                   "Median Alerts per Campaign" = c(median((beginning_campaign %>% dplyr::group_by(series) %>% 
                                                              dplyr::summarize(count = n()) %>% dplyr::select(count))[[1]]), 
                                                       median((middle_campaign %>% dplyr::group_by(series) %>% 
                                                                 dplyr::summarize(count = n()) %>% dplyr::select(count))[[1]]), 
                                                       median((end_campaign %>% dplyr::group_by(series) %>% 
                                                                 dplyr::summarize(count = n()) %>% dplyr::select(count))[[1]]))
                   , check.names = FALSE, stringsAsFactors = FALSE)
  
  # generate comparison graph
  graph <- reshape2::melt(df) %>% dplyr::filter(!grepl("[Mm]edian", variable)) %>% dplyr::mutate(variable = stringr::str_extract(variable, "\\w*")) %>% 
    ggplot2::ggplot(aes(x = variable, y = value, fill = factor(Date, levels = time_order_for_plots(reverse = TRUE), ordered = TRUE))) +
    ggplot2::geom_col(position = "dodge") + 
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = "Alert Count", fill = "Date", title = "AS Alert Counts") +
    ggplot2::scale_y_continuous(labels = scales::comma_format(digits = 0)) + 
    case_study_theme() + 
    time_of_month_color_scheme()
  
  # collect boxplot data
  all_client <- rbind(data.frame(Date = "Beginning", beginning_client), data.frame(Date = "Middle", middle_client), data.frame(Date = "End", end_client)) %>% 
    dplyr::filter(is_alert)
  alerts_per_client <- all_client %>% 
    dplyr::group_by(Date, client_name) %>% 
    dplyr::summarize(count = n()) %>% 
    dplyr::mutate(series = "Alerts\nper Client") %>% 
    dplyr::select(Date, series, count)

  all_campaign <- rbind(data.frame(Date = "Beginning", beginning_campaign), data.frame(Date = "Middle", middle_campaign), data.frame(Date = "End", end_campaign)) %>% 
    dplyr::filter(is_alert)
  alerts_per_campaign <- all_campaign %>% 
    dplyr::group_by(Date, series) %>% 
    dplyr::summarize(count = n()) %>% 
    dplyr::mutate(series = "Alerts\nper Campaign") %>% 
    dplyr::select(Date, series, count)
  
  all_data <- rbind(data.frame(Date = "Beginning", beginning), data.frame(Date = "Middle", middle), data.frame(Date = "End", end)) %>% 
    dplyr::filter(is_alert)
  campaign_alerts_per_client <- all_data %>% dplyr::group_by(Date, client_name) %>%
    dplyr::summarize(count = sum(campaign_name != "NOT A CAMPAIGN")) %>% 
    dplyr::mutate(series = "Campaign\nAlerts per Client") %>% 
    dplyr::select(Date, series, count)
  
  boxplot_labels <- c("Alerts\nper Client", "Alerts\nper Campaign", "Campaign\nAlerts per Client")
  
  boxplot_client_vs_campaign <- rbind(alerts_per_client, alerts_per_campaign) %>% 
    ggplot2::ggplot(aes(x = series, y = count, fill = factor(Date, levels = time_order_for_plots(reverse = TRUE), ordered = TRUE))) +
    ggplot2::geom_boxplot(outlier.color = "black", outlier.size = 1, coef = 3) + # coef is the Tukey fence multiplier
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev(boxplot_labels[-3]), drop = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma_format(digits = 0), breaks = seq(from = 0, to = 12, by = 3), limits = c(0, 12)) + # we are only checking for 12 KPIs so that is the limit
    case_study_theme() + 
    time_of_month_color_scheme() +
    ggplot2::labs(x = "", y = "Alert Count", fill = "Date", title = "Distribution of AS Alerts", subtitle = "Client vs. Campaign",
                  caption = "Note: This is only looking at series that have been flagged,\ni.e. alerts per campaign is the distribution of count of KPI alerts for campaigns that have alerts.")
  
  boxplot_campaign_alerts_per_client <- campaign_alerts_per_client %>%
    ggplot2::ggplot(aes(x = series, y = count, fill = factor(Date, levels = time_order_for_plots(reverse = TRUE), ordered = TRUE))) +
    ggplot2::geom_boxplot(outlier.color = "black", outlier.size = 1, coef = 3) + # coef is the Tukey fence multiplier
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev(boxplot_labels[3]), drop = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma_format(digits = 0)) +
    case_study_theme() +
    time_of_month_color_scheme() +
    ggplot2::labs(x = "", y = "Alert Count", fill = "Date", title = "Distribution of Campaign Alerts per Client",
                  caption = "Note: This is only looking at series that have been flagged,\ni.e. campaign alerts per client is the distribution of campaign alerts for clients that have alerts.")

  return(list(counts = df, graph = graph, boxplot_client_vs_campaign = boxplot_client_vs_campaign, boxplot_campaign_alerts_per_client = boxplot_campaign_alerts_per_client))
}

#' @title AS Metric Alert Counts
#' @description Calculate AS alert counts by metric for case study.
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import scopeR
#' @importFrom reshape2 melt
#'
#' @param beginning Dataframe of AS results for beginning of the month
#' @param middle Dataframe of AS results for middle of the month
#' @param end Dataframe of AS results for end of the month
#'
#' @return A list containing dataframe of client alert counts, a dataframe of campaign alert counts, and a graph of the results
#'
#' @export
AS_alert_counts_by_metric <- function(beginning, middle, end){
  beginning_client <- beginning %>% dplyr::filter(is_alert & campaign_name == "NOT A CAMPAIGN")
  beginning_campaign <- beginning %>% dplyr::filter(is_alert & campaign_name != "NOT A CAMPAIGN")
  middle_client <- middle %>% dplyr::filter(is_alert & campaign_name == "NOT A CAMPAIGN")
  middle_campaign <- middle %>% dplyr::filter(is_alert & campaign_name != "NOT A CAMPAIGN")
  end_client <- end %>% dplyr::filter(is_alert & campaign_name == "NOT A CAMPAIGN")
  end_campaign <- end %>% dplyr::filter(is_alert & campaign_name != "NOT A CAMPAIGN")
  
  client_df <- data.frame("Date" = c("Beginning", "Middle", "End"), check.names = FALSE)
  campaign_df <- data.frame("Date" = c("Beginning", "Middle", "End"), check.names = FALSE)
  
  kpi_list <- beginning %>% dplyr::distinct(kpi) %>% unlist %>% as.character
  
  for(metric in kpi_list){
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
    
    client_df[, list_entry] <- c(beginning_client %>% dplyr::filter(kpi == metric) %>% dplyr::distinct(client_name) %>% nrow,
                                 middle_client %>% dplyr::filter(kpi == metric) %>% dplyr::distinct(client_name) %>% nrow,
                                 end_client %>% dplyr::filter(kpi == metric) %>% dplyr::distinct(client_name) %>% nrow)
    campaign_df[, list_entry] <- c(beginning_campaign %>% dplyr::filter(kpi == metric) %>% dplyr::distinct(series) %>% nrow,
                                   middle_campaign %>% dplyr::filter(kpi == metric) %>% dplyr::distinct(series) %>% nrow,
                                   end_campaign %>% dplyr::filter(kpi == metric) %>% dplyr::distinct(series) %>% nrow)
  }
  
  # graphs
  kpi_counts <- rbind(client_df %>% dplyr::mutate(series = "Client"), campaign_df %>% dplyr::mutate(series = "Campaign")) %>% 
    reshape2::melt(id.vars = c("Date", "series")) %>% 
    dplyr::mutate(variable = as.character(variable), series = factor(series, levels = c("Campaign", "Client"), ordered = TRUE)) %>% 
    dplyr::arrange(desc(variable))
  label_order <- unique(kpi_counts$variable)
  graph <- kpi_counts %>% 
    dplyr::mutate(variable = factor(variable, levels = label_order, ordered = TRUE)) %>%  
    ggplot2::ggplot(aes(x = variable, y = value, fill = factor(Date, levels = time_order_for_plots(reverse = TRUE), ordered = TRUE))) +
    ggplot2::geom_col(position = "dodge") + 
    ggplot2::labs(x = "KPI", y = "Alert Count", fill = "Date", title = "AS Alert Counts by Metric") +
    ggplot2::scale_y_continuous(labels = scales::comma_format(digits = 0)) + 
    ggplot2::scale_x_discrete(limits = label_order, drop = FALSE) +
    ggplot2::coord_flip() + 
    case_study_theme() + 
    time_of_month_color_scheme() +
    facet_wrap_theme() +
    ggplot2::facet_wrap(~ series, nrow = 1) 
  
  return(list(client = client_df, campaign = campaign_df, graph = graph))
}



#' @title AS Alert Counts Graphs Arranger
#' @description Places the 4 graphs for the alert counts and distributions in 1 object
#' @author Stefanie Molin
#' 
#' @importFrom ggplot2 theme coord_flip
#' @importFrom cowplot plot_grid
#' 
#' @param overall_results_count List of results from AS_alert_counts()
#' @param metrics_count List with results from AS_alert_counts_by_metric()
#'
#' @return A plot object
#' 
#' @export
#' 

arrange_AS_alert_count_graphs <- function(overall_results_count, metrics_count){
  row_one <- cowplot::plot_grid(overall_results_count$boxplot_client_vs_campaign + ggplot2::theme(legend.position = "none"), 
                                overall_results_count$boxplot_campaign_alerts_per_client + ggplot2::theme(legend.position = "none"),
                                ncol = 2, nrow = 1, rel_widths = c(1, 1))
  row_two <- cowplot::plot_grid(metrics_count$graph + ggplot2::theme(legend.position = "none"), overall_results_count$graph,
                                ncol = 2, nrow = 1, rel_widths = c(1.2, 0.8))
  cowplot::plot_grid(row_one, row_two, ncol = 1, rel_heights = c(0.7, 1))
}

#' @title TS Alert Counts
#' @description Calculate TS client alert counts for case study.
#' @author Stefanie Molin
#'
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom scopeR capitalize_name
#'
#' @param beginning Dataframe of TS results for beginning of the month
#' @param middle Dataframe of TS results for middle of the month
#' @param end Dataframe of TS results for end of the month
#'
#' @return A list containing dataframe of TS alert counts, a graph of the results, and a boxplot of the distribution
#'
#' @export
TS_alert_counts <- function(beginning, middle, end){
  beginning_site <- beginning %>% dplyr::filter(is_alert & event_name == "SITE LEVEL" & site_type == "SITE LEVEL")
  beginning_tag <- beginning %>% dplyr::filter(is_alert & event_name != "SITE LEVEL" & site_type != "SITE LEVEL")
  middle_site <- middle %>% dplyr::filter(is_alert & event_name == "SITE LEVEL" & site_type == "SITE LEVEL")
  middle_tag <- middle %>% dplyr::filter(is_alert & event_name != "SITE LEVEL" & site_type != "SITE LEVEL")
  end_site <- end %>% dplyr::filter(is_alert & event_name == "SITE LEVEL" & site_type == "SITE LEVEL")
  end_tag <- end %>% dplyr::filter(is_alert & event_name != "SITE LEVEL" & site_type != "SITE LEVEL")
  
  df <- data.frame("Date" = c("Beginning", "Middle", "End"), 
                   "Partner Alerts" = c(beginning_site %>% dplyr::distinct(partner_name) %>% nrow, 
                                             middle_site %>% dplyr::distinct(partner_name) %>% nrow, 
                                             end_site %>% dplyr::distinct(partner_name) %>% nrow), 
                   "Median Tag Alerts per Partner" = c(median((beginning_tag %>% dplyr::group_by(partner_name) %>% 
                                                            dplyr::summarize(count = n()) %>% dplyr::select(count))[[1]]),
                                                  median((middle_tag %>% dplyr::group_by(partner_name) %>% 
                                                            dplyr::summarize(count = n()) %>% dplyr::select(count))[[1]]),
                                                  median((end_tag %>% dplyr::group_by(partner_name) %>% 
                                                            dplyr::summarize(count = n()) %>% dplyr::select(count))[[1]])),
                   "Max Tag Alerts per Partner" = c(max((beginning_tag %>% dplyr::group_by(partner_name) %>% 
                                                      dplyr::summarize(count = n()) %>% dplyr::select(count))[[1]]),
                                               max((middle_tag %>% dplyr::group_by(partner_name) %>% 
                                                      dplyr::summarize(count = n()) %>% dplyr::select(count))[[1]]),
                                               max((end_tag %>% dplyr::group_by(partner_name) %>% 
                                                      dplyr::summarize(count = n()) %>% dplyr::select(count))[[1]])),
                   "Tags Alerts" = c(beginning_tag %>% dplyr::distinct(series) %>% nrow, 
                                     middle_tag %>% dplyr::distinct(series) %>% nrow,
                                     end_tag %>% dplyr::distinct(series) %>% nrow)
                   , check.names = FALSE)
  
  # generate comparison graph
  graph <- reshape2::melt(df) %>% dplyr::filter(!grepl("per", variable)) %>% dplyr::mutate(variable = stringr::str_extract(variable, "\\w*")) %>% 
    ggplot2::ggplot(aes(x = factor(variable, levels = rev(c("Partner", "Tags")), ordered = TRUE), y = value, 
                        fill = factor(Date, levels = time_order_for_plots(reverse = TRUE), ordered = TRUE))) +
    ggplot2::geom_col(position = "dodge") + 
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = "Alert Count", fill = "Date", title = "TS Alert Counts") +
    ggplot2::scale_y_continuous(labels = scales::comma_format(digits = 0)) + 
    case_study_theme() + 
    time_of_month_color_scheme()
  
  # collect boxplot data
  all_partner <- rbind(data.frame(Date = "Beginning", beginning_site), data.frame(Date = "Middle", middle_site), data.frame(Date = "End", end_site)) %>% 
    dplyr::filter(is_alert)
  alerts_per_partner <- all_partner %>% 
    dplyr::group_by(Date, partner_name) %>% 
    dplyr::summarize(count = n()) %>% 
    dplyr::mutate(series = "Alerts\nper Partner") %>% 
    dplyr::select(Date, series, count)
  
  all_tag <- rbind(data.frame(Date = "Beginning", beginning_tag), data.frame(Date = "Middle", middle_tag), data.frame(Date = "End", end_tag)) %>% 
    dplyr::filter(is_alert)
  alerts_per_event_name <- all_tag %>% 
    dplyr::group_by(Date, partner_name, event_name) %>% 
    dplyr::summarize(count = n()) %>% 
    dplyr::mutate(series = paste0("Alerts per\n", scopeR::capitalize_name(event_name), " Tag")) %>% 
    dplyr::select(Date, series, count)
  alerts_per_site_type <- all_tag %>% 
    dplyr::group_by(Date, partner_name, site_type) %>% 
    dplyr::summarize(count = n()) %>% 
    dplyr::mutate(series = paste0("Alerts per\n", toupper(site_type), " Tag")) %>% 
    dplyr::select(Date, series, count)
  
  all_data <- rbind(data.frame(Date = "Beginning", beginning), data.frame(Date = "Middle", middle), data.frame(Date = "End", end)) %>% 
    dplyr::filter(is_alert)
  tag_alerts_per_partner <- all_data %>% 
    dplyr::group_by(Date, partner_name) %>%
    dplyr::summarize(count = sum(event_name != "SITE LEVEL" & site_type != "SITE LEVEL")) %>% 
    dplyr::mutate(series = "Tag Alerts\nper Partner") %>% 
    dplyr::select(Date, series, count)
  
  boxplot_labels <- c("Alerts\nper Partner", "Tag Alerts\nper Partner", unique(sort(alerts_per_event_name$series)), unique(sort(alerts_per_site_type$series)))
  
  boxplot <- rbind(alerts_per_partner, alerts_per_event_name, alerts_per_site_type, tag_alerts_per_partner) %>% 
    ggplot2::ggplot(aes(x = series, y = count, fill = factor(Date, levels = time_order_for_plots(reverse = TRUE), ordered = TRUE))) +
    ggplot2::geom_boxplot(outlier.color = "black", outlier.size = 1, coef = 3) + # coef is the Tukey fence multiplier
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev(boxplot_labels), drop = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma_format(digits = 0)) + 
    case_study_theme() + 
    time_of_month_color_scheme() +
    ggplot2::labs(x = "", y = "Alert Count", fill = "Date", title = "Distribution of TS Alerts", 
                  caption = "Note: This is only looking at series that have been flagged,\ni.e. sales tag alerts is the distribution of sales tag alerts for clients that have sales tag alerts (regardless of site type).")
  
  return(list(counts = df, graph = graph, boxplot = boxplot))
}


#' @title TS Metric Alert Counts
#' @description Calculate TS alert counts by metric for case study.
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import scopeR
#'
#' @param beginning Dataframe of TS results for beginning of the month
#' @param middle Dataframe of TS results for middle of the month
#' @param end Dataframe of TS results for end of the month
#'
#' @return A list containing dataframe of event name alert counts, a dataframe of site type alert counts, and a graph of the results
#'
#' @export
TS_alert_counts_by_metric <- function(beginning, middle, end){
  beginning <- beginning %>% dplyr::filter(is_alert)
  middle <- middle %>% dplyr::filter(is_alert)
  end <- end %>% dplyr::filter(is_alert)
  
  site_type_df <- data.frame("Date" = c("Beginning", "Middle", "End"), check.names = FALSE)
  event_name_df <- data.frame("Date" = c("Beginning", "Middle", "End"), check.names = FALSE)
  
  for(metric in beginning %>% dplyr::distinct(site_type) %>% unlist %>% as.character){
    # formatting of names
    if(metric == "SITE LEVEL"){
      list_entry <- scopeR::capitalize_name(metric)
    } else{
      list_entry <- toupper(metric)
    }
    
    site_type_df[, list_entry] <- c(beginning %>% dplyr::filter(site_type == metric) %>% nrow,
                                    middle %>% dplyr::filter(site_type == metric) %>% nrow,
                                    end %>% dplyr::filter(site_type == metric) %>% nrow)
  }
  
  for(metric in beginning %>% dplyr::distinct(event_name) %>% unlist %>% as.character){
    # formatting of names
    list_entry <- scopeR::capitalize_name(metric)
    
    event_name_df[, list_entry] <- c(beginning %>% dplyr::filter(event_name == metric) %>% nrow,
                                     middle %>% dplyr::filter(event_name == metric) %>% nrow,
                                     end %>% dplyr::filter(event_name == metric) %>% nrow)
  }
  
  # graphs
  kpi_counts <- rbind(site_type_df %>% reshape2::melt(id.vars = "Date") %>% dplyr::mutate(series = "Site Type"), 
                      event_name_df %>% reshape2::melt(id.vars = "Date") %>% dplyr::mutate(series = "Event Name")) %>% 
    dplyr::mutate(variable = as.character(variable), series = factor(series, levels = c("Event Name", "Site Type"), ordered = TRUE)) %>% 
    dplyr::arrange(desc(variable))
  
  event_name_graph <- kpi_counts %>% 
    dplyr::filter(series == "Event Name" & variable != "Site Level") %>% 
    dplyr::mutate(variable = factor(variable, levels = sort(unique(variable)), ordered = TRUE)) %>%  
    ggplot2::ggplot(aes(x = variable, y = value, fill = factor(Date, levels = time_order_for_plots(reverse = FALSE), ordered = TRUE))) +
    ggplot2::geom_col(position = "dodge") + 
    ggplot2::labs(x = "Event Name", y = "Alert Count", fill = "Date", title = "TS Alert Counts by Event Name") +
    ggplot2::scale_y_continuous(labels = scales::comma) + 
    case_study_theme() + 
    time_of_month_color_scheme(reverse = TRUE) 
  
  site_type_graph <- kpi_counts %>% 
    dplyr::filter(series == "Site Type" & variable != "Site Level") %>% 
    dplyr::mutate(variable = factor(variable, levels = sort(unique(variable)), ordered = TRUE)) %>%  
    ggplot2::ggplot(aes(x = variable, y = value, fill = factor(Date, levels = time_order_for_plots(reverse = FALSE), ordered = TRUE))) +
    ggplot2::geom_col(position = "dodge") + 
    ggplot2::labs(x = "Site Type", y = "Alert Count", fill = "Date", title = "TS Alert Counts by Site Type") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    case_study_theme() + 
    time_of_month_color_scheme(reverse = TRUE) 
  
  return(list(event_name = event_name_df %>% dplyr::select("Date", "Site Level", everything()),
              site_type = site_type_df %>% dplyr::select("Date", "Site Level", everything()), 
              event_name_graph = event_name_graph, site_type_graph = site_type_graph))
}

#' @title TS Alert Counts Graphs Arranger
#' @description Places the 4 graphs for the alert counts and distributions in 1 object
#' @author Stefanie Molin
#' 
#' @importFrom ggplot2 theme coord_flip
#' @importFrom cowplot plot_grid
#' 
#' @param overall_results_count List of results from TS_alert_counts()
#' @param metrics_count List with results from TS_alert_counts_by_metric()
#'
#' @return A plot object
#' 
#' @export
#' 

arrange_TS_alert_count_graphs <- function(overall_results_count, metrics_count){
  col_one <- cowplot::plot_grid(overall_results_count$boxplot + ggplot2::theme(legend.position = "none"),  
                                ncol = 1, nrow = 1)
  col_two <- cowplot::plot_grid(overall_results_count$graph, metrics_count$event_name_graph + ggplot2::theme(legend.position = "none"), 
                                metrics_count$site_type_graph + ggplot2::theme(legend.position = "none"), ncol = 1, nrow = 3)
  cowplot::plot_grid(col_one, col_two, ncol = 2, nrow = 1)
}

#' @title Exec Alert Counts
#' @description Calculate territory RexT alert counts for case study.
#' @author Stefanie Molin
#'
#' @import dplyr
#'
#' @param data Dataframe of territory RexT results
#' @param beginning_dates Vector of dates for beginning of the month
#' @param middle_dates Vector of dates for middle of the month
#' @param end_dates Vector of dates for end of the month
#'
#' @return A list containing dataframe of TS alert counts, a graph of the results, and a boxplot of the distribution
#'
#' @export
exec_alert_counts <- function(data, beginning_dates, middle_dates, end_dates){
  all_input_data <- data
  data <- data %>% dplyr::filter(is_alert)
  
  beginning <- data %>% dplyr::filter(run_date %in% as.Date(beginning_dates))
  middle <- data %>% dplyr::filter(run_date %in% as.Date(middle_dates))
  end <- data %>% dplyr::filter(run_date %in% as.Date(end_dates))
  
  df <- data.frame("Date" = c("Beginning", "Middle", "End"), 
                   "Total" = c(beginning %>% nrow,
                               middle %>% nrow, 
                               end %>% nrow), 
                   "Country" = c(beginning %>% dplyr::filter(country != 'N/A') %>% nrow,
                                 middle %>% dplyr::filter(country != 'N/A') %>% nrow,
                                 end %>% dplyr::filter(country != 'N/A') %>% nrow),
                   "Subregion" =  c(beginning %>% dplyr::filter(country == 'N/A' & subregion != 'N/A') %>% nrow,
                                    middle %>% dplyr::filter(country == 'N/A' & subregion != 'N/A') %>% nrow,
                                    end %>% dplyr::filter(country == 'N/A' & subregion != 'N/A') %>% nrow),
                   "Region" = c(beginning %>% dplyr::filter(country == 'N/A' & subregion == 'N/A') %>% nrow,
                                middle %>% dplyr::filter(country == 'N/A' & subregion == 'N/A') %>% nrow,
                                end %>% dplyr::filter(country == 'N/A' & subregion == 'N/A') %>% nrow)
                   , check.names = FALSE)
  
  # generate comparison graph
  melted <- reshape2::melt(df)
  graph <- melted %>%  
    ggplot2::ggplot(aes(x = factor(variable, levels = c("Total", "Country", "Subregion", "Region"), ordered = TRUE), y = value, 
                                   fill = factor(Date, levels = time_order_for_plots(reverse = TRUE), ordered = TRUE))) +
    ggplot2::geom_col(position = "dodge") + 
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = "Alert Count", fill = "Date", title = "RexT Alert Counts") +
    ggplot2::scale_y_continuous(labels = scales::comma_format(digits = 0)) + 
    case_study_theme() + 
    time_of_month_color_scheme()
  
  all_data <- all_input_data %>% 
    dplyr::mutate(Date = ifelse(run_date %in% as.Date(beginning_dates), "Beginning", 
                                ifelse(run_date %in% as.Date(middle_dates), "Middle", "End")))
  
  overall <- all_data %>% 
    dplyr::filter(region == "AMERICAS") %>% 
    dplyr::group_by(Date, region, subregion, country) %>% 
    dplyr::summarize(count = sum(is_alert)) %>% 
    dplyr::mutate(series = "Alerts Overall") %>% 
    dplyr::ungroup() %>%
    dplyr::select(series, Date, count)
  
  na_overall <- all_data %>% 
    dplyr::filter(subregion == "NORTH AMERICA") %>% 
    dplyr::group_by(Date, region, subregion, country) %>% 
    dplyr::summarize(count = sum(is_alert)) %>% 
    dplyr::mutate(series = "Alerts in North America") %>%
    dplyr::ungroup() %>%
    dplyr::select(series, Date, count)
  
  latam_overall <- all_data %>% 
    dplyr::filter(subregion == "LATAM") %>% 
    dplyr::group_by(Date, region, subregion, country) %>%  
    dplyr::summarize(count = sum(is_alert)) %>% 
    dplyr::mutate(series = "Alerts in LATAM") %>%
    dplyr::ungroup() %>%
    dplyr::select(series, Date, count)
  
  boxplot_labels <- c("Alerts Overall", "Alerts in North America", "Alerts in LATAM")
  
  boxplot <- rbind(overall, na_overall, latam_overall) %>% 
    ggplot2::ggplot(aes(x = series, y = count, fill = factor(Date, levels = time_order_for_plots(reverse = TRUE), ordered = TRUE))) +
    ggplot2::geom_boxplot(outlier.color = "black", outlier.size = 1, coef = 3) + # coef is the Tukey fence multiplier
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev(boxplot_labels), drop = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma_format(digits = 0), breaks = seq(from = 0, to = 6, by = 2), limits = c(0, 6)) + 
    case_study_theme() + 
    time_of_month_color_scheme() +
    ggplot2::labs(x = "", y = "Alert Count", fill = "Date", title = "Distribution of RexT Alerts")
  
  return(list(counts = df, graph = graph, boxplot = boxplot))
}


#' @title RexT Alerts by Territory
#' @description Calculate RexT alert counts by territory for case study.
#' @author Stefanie Molin
#'
#' @import dplyr
#'
#' @param data Dataframe of territory RexT results
#' @param beginning_dates Vector of dates for beginning of the month
#' @param middle_dates Vector of dates for middle of the month
#' @param end_dates Vector of dates for end of the month
#'
#' @return A list of a dataframe containing the alert counts and a graph.
#'
#' @export
exec_alert_counts_by_territory <- function(data, beginning_dates, middle_dates, end_dates){
  data <- data %>% 
    dplyr::mutate(global_series = ifelse(as.character(country) == 'N/A', 
                                         ifelse(as.character(subregion) == 'N/A', as.character(region), as.character(subregion)), 
                                         as.character(country)))
  # get names for table
  territories <- data %>% distinct(global_series) %>% arrange(global_series) %>% unlist %>% as.character
  
  data <- data %>% dplyr::filter(is_alert)
  
  beginning <- data %>% dplyr::filter(run_date %in% as.Date(beginning_dates))
  middle <- data %>% dplyr::filter(run_date %in% as.Date(middle_dates))
  end <- data %>% dplyr::filter(run_date %in% as.Date(end_dates))
  
  df <- data.frame("Date" = c("Beginning", "Middle", "End"), check.names = FALSE)
  
  for(territory in territories){
    df[, territory] <- c(beginning %>% filter(global_series == territory) %>% nrow,
                          middle %>% filter(global_series == territory) %>% nrow,
                          end %>% filter(global_series == territory) %>% nrow)
  }
  
  # transpose the results
  df <- data.frame(t(df), stringsAsFactors = FALSE)
  colnames(df) <- df[1,]
  df <- df[-1,]
  df$Territory <- rownames(df)
  df <- df %>% select(Territory, Beginning, Middle, End)
  rownames(df) <- 1:nrow(df)
  
  melted <- reshape2::melt(df, id.vars = "Territory") %>%
    dplyr::rename(Date = variable, variable = Territory) %>% 
    dplyr::mutate(value = as.numeric(value))
  # generate comparison graph
  graph <- melted %>% 
    ggplot2::ggplot(aes(x = factor(variable, levels = rev(sort(unique(df$Territory))), ordered = TRUE), y = value, 
                        fill = factor(Date, levels = time_order_for_plots(reverse = TRUE), ordered = TRUE))) +
    ggplot2::geom_col(position = "dodge") + 
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = "Alert Count", fill = "Date", title = "RexT Alert Counts by Territory") +
    ggplot2::scale_y_continuous(labels = scales::comma_format(digits = 0), breaks = seq(from = 0, to = 6, by = 2), limits = c(0, 6)) + 
    case_study_theme() + 
    time_of_month_color_scheme() 
  
  return(list(counts = df, graph = graph))
}

#' @title RexT Alert Counts Graphs Arranger
#' @description Places the 3 graphs for the alert counts and distributions in 1 object
#' @author Stefanie Molin
#' 
#' @importFrom ggplot2 theme coord_flip
#' @importFrom cowplot plot_grid
#' 
#' @param overall_results_count List of results from exec_alert_counts()
#' @param metrics_count List with results from exec_alert_counts_by_metric()
#'
#' @return A plot object
#' 
#' @export
#' 

arrange_RexT_alert_count_graphs <- function(overall_results_count, metrics_count){
  row_one <- cowplot::plot_grid(overall_results_count$graph, ncol = 1, nrow = 1)
  row_two <- cowplot::plot_grid(overall_results_count$boxplot + ggplot2::theme(legend.position = "none"),  
                                metrics_count$graph + ggplot2::theme(legend.position = "none"), ncol = 2, nrow = 1)
  
  cowplot::plot_grid(row_one, row_two, ncol = 1, nrow = 2, rel_heights = c(0.75, 1))  
}


#' @title Western Electric Rules AS Alert Counts
#' @description Calculate AS client and campaign alert counts for Western Electric Rules
#' @author Stefanie Molin
#'
#' @import dplyr
#'
#' @param beginning Dataframe of AS results for beginning of the month
#' @param middle Dataframe of AS results for middle of the month
#' @param end Dataframe of AS results for end of the month
#' @param rule Rule to filter for
#'
#' @return A list containing a dataframe of the counts ("counts"), graphs for counts for client and campaign (graph), and a boxplot of the results
#'
#' @export
western_electric_AS_counts <- function(beginning, middle, end, rule){
  rule <- ifelse(grepl("violation", rule), rule, 
                 ifelse(grepl("[Rr]ule", rule), paste(rule, "violation"), paste(rule, "rule violation")))
  
  beginning <- beginning %>% dplyr::filter(reason %in% c("N/A", rule))
  middle <- middle %>% dplyr::filter(reason %in% c("N/A", rule))
  end <- end %>% dplyr::filter(reason %in% c("N/A", rule))
  
  results <- AS_alert_counts(beginning = beginning, middle = middle, end = end)
  
  return(results)
}

#' @title Western Electric Rules AS Metric Alert Counts
#' @description Calculate AS alert counts by metric for case study using Western Electric Rules.
#' @author Stefanie Molin
#'
#' @import dplyr
#'
#' @param beginning Dataframe of AS results for beginning of the month
#' @param middle Dataframe of AS results for middle of the month
#' @param end Dataframe of AS results for end of the month
#' @param rule Rule to filter for
#'
#' @return A list containing dataframe of client alert counts, a dataframe of campaign alert counts, and a graph of the results
#'
#' @export
western_electric_AS_metric_counts <- function(beginning, middle, end, rule){
  rule <- ifelse(grepl("violation", rule), rule, 
                 ifelse(grepl("[Rr]ule", rule), paste(rule, "violation"), paste(rule, "rule violation")))
  
  beginning <- beginning %>% dplyr::filter(reason %in% c("N/A", rule))
  middle <- middle %>% dplyr::filter(reason %in% c("N/A", rule))
  end <- end %>% dplyr::filter(reason %in% c("N/A", rule))
  
  results <- AS_alert_counts_by_metric(beginning = beginning, middle = middle, end = end)
  
  return(results)
}

#' @title Western Electric Rules TS Alert Counts
#' @description Calculate TS client and campaign alert counts for Western Electric Rules
#' @author Stefanie Molin
#'
#' @import dplyr
#'
#' @param beginning Dataframe of TS results for beginning of the month
#' @param middle Dataframe of TS results for middle of the month
#' @param end Dataframe of TS results for end of the month
#' @param rule Rule to filter for
#'
#' @return A list containing dataframe of TS alert counts, a graph of the results, and a boxplot of the distribution
#'
#' @export
western_electric_TS_counts <- function(beginning, middle, end, rule){
  rule <- ifelse(grepl("violation", rule), rule, 
                 ifelse(grepl("[Rr]ule", rule), paste(rule, "violation"), paste(rule, "rule violation")))
  
  beginning <- beginning %>% dplyr::filter(reason %in% c("N/A", rule))
  middle <- middle %>% dplyr::filter(reason %in% c("N/A", rule))
  end <- end %>% dplyr::filter(reason %in% c("N/A", rule))
  
  results <- TS_alert_counts(beginning = beginning, middle = middle, end = end)
  
  return(results)
}

#' @title Western Electric Rules TS Metric Alert Counts
#' @description Calculate TS alert counts by metric for case study using Western Electric Rules.
#' @author Stefanie Molin
#'
#' @import dplyr
#'
#' @param beginning Dataframe of TS results for beginning of the month
#' @param middle Dataframe of TS results for middle of the month
#' @param end Dataframe of TS results for end of the month
#' @param rule Rule to filter for
#'
#' @return A list containing dataframe of event name alert counts, a dataframe of site type alert counts, and a graph of the results
#'
#' @export
western_electric_TS_metric_counts <- function(beginning, middle, end, rule){
  rule <- ifelse(grepl("violation", rule), rule, 
                 ifelse(grepl("[Rr]ule", rule), paste(rule, "violation"), paste(rule, "rule violation")))
  
  beginning <- beginning %>% dplyr::filter(reason %in% c("N/A", rule))
  middle <- middle %>% dplyr::filter(reason %in% c("N/A", rule))
  end <- end %>% dplyr::filter(reason %in% c("N/A", rule))
  
  results <- TS_alert_counts_by_metric(beginning = beginning, middle = middle, end = end)
  
  return(results)
}

#' @title Western Electric Rules RexT Alert Counts
#' @description Calculate RexT client and campaign alert counts for Western Electric Rules
#' @author Stefanie Molin
#'
#' @import dplyr
#'
#' @param data Dataframe of territory RexT results
#' @param beginning_dates Vector of dates for beginning of the month
#' @param middle_dates Vector of dates for middle of the month
#' @param end_dates Vector of dates for end of the month
#' @param rule Rule to filter for
#'
#' @return A list containing dataframe of RexT alert counts, a graph of the results, and a boxplot of the distribution
#'
#' @export
western_electric_RexT_counts <- function(data, beginning_dates, middle_dates, end_dates, rule){
  rule <- ifelse(grepl("violation", rule), rule, 
                 ifelse(grepl("[Rr]ule", rule), paste(rule, "violation"), paste(rule, "rule violation")))
  
  data <- data %>% dplyr::filter(reason %in% c("N/A", rule))
  
  results <- exec_alert_counts(data = data, beginning_dates = beginning_dates, 
                               middle_dates = middle_dates, end_dates = end_dates)
  
  return(results)
}

#' @title Western Electric Rules RexT Territory Alert Counts
#' @description Calculate RexT alert counts by territory for case study using Western Electric Rules.
#' @author Stefanie Molin
#'
#' @import dplyr
#'
#' @param data Dataframe of territory RexT results
#' @param beginning_dates Vector of dates for beginning of the month
#' @param middle_dates Vector of dates for middle of the month
#' @param end_dates Vector of dates for end of the month
#' @param rule Rule to filter for
#'
#' @return A list containing dataframe of RexT alert counts by territory and a graph of the results
#'
#' @export
western_electric_RexT_territory_counts <- function(data, beginning_dates, middle_dates, end_dates, rule){
  rule <- ifelse(grepl("violation", rule), rule, 
                 ifelse(grepl("[Rr]ule", rule), paste(rule, "violation"), paste(rule, "rule violation")))
  
  data <- data %>% dplyr::filter(reason %in% c("N/A", rule))
  
  results <- exec_alert_counts_by_territory(data = data, beginning_dates = beginning_dates, 
                                            middle_dates = middle_dates, end_dates = end_dates)
  
  return(results)
}
#' @title Run Anomaly Detection
#' @description Determine whether or not yesterday was an anomaly
#' @author Stefanie Molin
#'
#' @importFrom outliers scores
#'
#' @param data Dataframe where first column is "day" and second is the metric you want to check for anomalies
#' @param direction Can be "both", "neg", or "pos". Determines whether you only want to check for positive anomalies, negative, or both.
#' @param stat_sig The statistical significance for the anomaly test.
#' @param yesterday The date to check for alerts.
#'
#' @return A boolean indicating whether or not yesterday was an alert
#'
run_anomaly_detection <- function(data, direction, stat_sig, yesterday){
  minValidData <- 25
  tukey_multiplier <- 3
  direction <- tolower(direction)
  
  # second column contains data to check for anomaly
  data_vector <- data[,2]
  
  if(nrow(data) >= minValidData){
    # handle one-sided
    if(direction %in% c("neg", "pos")){
      
      if(identical(direction, "neg")){
        
        # check if value is more extreme than day before
        isMoreExtreme <- data[as.Date(data$day) == yesterday, 2] <= data[as.Date(data$day) == yesterday - 1, 2] * 0.9
        
        # if the value isn't there it generates numeric(0) and the equation will evaluate to logical(0), so we need to check length below
        isMoreExtreme <- ifelse(length(isMoreExtreme) == 0, FALSE, isMoreExtreme)
        
        # determine if yesterday meets criteria
        results <- outliers::scores(data_vector, "chisq", stat_sig) & data_vector < quantile(data_vector, 0.25) - tukey_multiplier*IQR(data_vector)
      } else{
        
        # check if value is more extreme than day before
        isMoreExtreme <- data[as.Date(data$day) == yesterday, 2] >= data[as.Date(data$day) == yesterday - 1, 2] * 1.1
        
        # if the value isn't there it generates numeric(0) and the equation will evaluate to logical(0), so we need to check length below
        isMoreExtreme <- ifelse(length(isMoreExtreme) == 0, FALSE, isMoreExtreme)
        
        # determine if yesterday meets criteria
        results <- outliers::scores(data_vector, "chisq", stat_sig) & data_vector > quantile(data_vector, 0.75) + tukey_multiplier*IQR(data_vector)
      }
      
      # check if yesterday was an anomaly
      if(!is.na(results[length(results)]) && as.Date(data[length(results), "day"]) == yesterday && isMoreExtreme){
        anomaly <- results[length(results)]
      } else{
        anomaly <- FALSE
      }
      
    } else if(identical(direction, "both")){
      # handle two-sided
      anomaly <- run_anomaly_detection(data, "neg", stat_sig, yesterday) | run_anomaly_detection(data, "pos", stat_sig, yesterday)
    } else{
      stop("The direction argument can be either \"both\", \"pos\", or \"neg\".", call. = FALSE)
    }
    
  } else{
    anomaly <- FALSE
  }
  
  
  return(anomaly)
}

#' @title Detect Anomaly
#' @description Determine whether or not yesterday was an anomaly after cleaning data.
#' @author Stefanie Molin
#'
#' @param data Dataframe where first column is "day" and second is the metric you want to check for anomalies
#' @param metric Can be "events", "COS", "CR", "CTR", "spend", "rext", "exec". Which metric to check for anomaly.
#' @param yesterday The date to check for alerts.
#'
#' @return A boolean indicating whether or not yesterday was an alert
#'
#' @export
detect_anomaly <- function(data, metric, yesterday){
  # stat sig per metric
  statSigCOS <- 0.9995
  statSigCTR <- 0.999
  statSigCR <- 0.999
  statSigSiteEvents <- 0.975
  statSigSpend <- 0.9975
  statSigRexT <- 0.9975
  statSigExec <- 0.975
  
  metric <- tolower(metric)
  direction <- "both"
  
  if(identical(metric, "events")){
    stat_sig <- statSigSiteEvents
  } else if(identical(metric, "cos")){
    stat_sig <- statSigCOS
  } else if(identical(metric, "cr")){
    stat_sig <- statSigCR
  } else if(identical(metric, "ctr")){
    stat_sig <- statSigCTR
  } else if(identical(metric, "spend")){
    stat_sig <- statSigSpend
  } else if(identical(metric, "rext")){
    stat_sig <- statSigRexT
  } else if(identical(metric, "exec")){
    stat_sig <- statSigExec
  } else{
    stat_sig <- .95
  }
  
  anomaly <- run_anomaly_detection(data, direction = direction, stat_sig = stat_sig, yesterday = yesterday)
  return(anomaly)
}
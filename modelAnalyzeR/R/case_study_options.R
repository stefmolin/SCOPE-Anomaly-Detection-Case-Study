#' @title Set Logging Level
#' @description Set logging level for case study.
#' @author Stefanie Molin
#'
#' @importFrom futile.logger flog.threshold
#'
#' @param level_name String such as "INFO" or "DEBUG"
#'
#' @export
case_study_log_level <- function(level_name){
  log_threshold <- futile.logger::flog.threshold(level_name)
} 
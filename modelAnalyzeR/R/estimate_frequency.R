#' @title Estimate Frequency
#' @description Estimate the frequency of vector
#' @author Stefanie Molin
#'
#' @import dplyr
#' @import TSA
#'
#' @param data Vector of values in order.
#'
#' @return A dataframe
#'
#' @export
estimate_frequency <- function(data){
  fourier_transform <- TSA::periodogram(data, plot = FALSE)
  
  df <- data.frame(freq = round(1/fourier_transform$freq, 0), spec = fourier_transform$spec) %>% 
    arrange(desc(spec)) %>% filter(freq < round(length(data)/2, 0))
  
  return(df[1, "freq"])
}
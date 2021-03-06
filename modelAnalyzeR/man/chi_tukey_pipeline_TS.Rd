% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chi_square_tukey_fence_more_extreme.R
\name{chi_tukey_pipeline_TS}
\alias{chi_tukey_pipeline_TS}
\title{Run through TS Detect Anomaly Pipeline for Case Study}
\usage{
chi_tukey_pipeline_TS(data, yesterday)
}
\arguments{
\item{data}{Dataframe of the data to check for anomalies}

\item{yesterday}{Day to check for anomalies}
}
\value{
A list with 2 items: "results"--a dataframe with everything that was tested and whether or not it was an alert and 
"cleaned_data"--a dataframe containing the cleaned and prepped data that was used to check for anomalies.
}
\description{
Clean data, impute the median for missing dates, and detect anomalies for all partner/kpi combinations.
}
\author{
Stefanie Molin
}

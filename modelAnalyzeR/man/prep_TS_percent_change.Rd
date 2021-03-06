% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percent_change_models.R
\name{prep_TS_percent_change}
\alias{prep_TS_percent_change}
\title{Prep TS Data for Percent Change Models}
\usage{
prep_TS_percent_change(data, FUN)
}
\arguments{
\item{data}{Dataframe of daily data to check for anomalies.}

\item{FUN}{Function to aggregate by}
}
\value{
Prepped data as dataframe
}
\description{
Clean data and calculate percent changes by specific aggregation function.
}
\note{
This function relies on the structure of the query that provides "data". 
If changing column names, adding or subtracting columns, be sure to update here as well.
}
\author{
Stefanie Molin
}

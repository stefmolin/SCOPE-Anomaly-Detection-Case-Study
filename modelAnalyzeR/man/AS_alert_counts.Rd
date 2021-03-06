% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/case_study_results.R
\name{AS_alert_counts}
\alias{AS_alert_counts}
\title{AS Alert Counts}
\usage{
AS_alert_counts(beginning, middle, end)
}
\arguments{
\item{beginning}{Dataframe of AS results for beginning of the month}

\item{middle}{Dataframe of AS results for middle of the month}

\item{end}{Dataframe of AS results for end of the month}
}
\value{
A list containing a dataframe of the counts ("counts"), graphs for counts for client and campaign (graph), and a boxplot of the results
}
\description{
Calculate AS client and campaign alert counts for case study.
}
\note{
The boxplot shows the distribution for series (clients or campaigns) that have been flagged, 
i.e. campaign alerts per client is the distribution of campaign alerts for clients that have alerts.
}
\author{
Stefanie Molin
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/case_study_results.R
\name{western_electric_AS_counts}
\alias{western_electric_AS_counts}
\title{Western Electric Rules AS Alert Counts}
\usage{
western_electric_AS_counts(beginning, middle, end, rule)
}
\arguments{
\item{beginning}{Dataframe of AS results for beginning of the month}

\item{middle}{Dataframe of AS results for middle of the month}

\item{end}{Dataframe of AS results for end of the month}

\item{rule}{Rule to filter for}
}
\value{
A list containing a dataframe of the counts ("counts"), graphs for counts for client and campaign (graph), and a boxplot of the results
}
\description{
Calculate AS client and campaign alert counts for Western Electric Rules
}
\author{
Stefanie Molin
}

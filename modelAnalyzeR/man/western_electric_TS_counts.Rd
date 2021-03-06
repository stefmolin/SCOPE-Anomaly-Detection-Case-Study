% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/case_study_results.R
\name{western_electric_TS_counts}
\alias{western_electric_TS_counts}
\title{Western Electric Rules TS Alert Counts}
\usage{
western_electric_TS_counts(beginning, middle, end, rule)
}
\arguments{
\item{beginning}{Dataframe of TS results for beginning of the month}

\item{middle}{Dataframe of TS results for middle of the month}

\item{end}{Dataframe of TS results for end of the month}

\item{rule}{Rule to filter for}
}
\value{
A list containing dataframe of TS alert counts, a graph of the results, and a boxplot of the distribution
}
\description{
Calculate TS client and campaign alert counts for Western Electric Rules
}
\author{
Stefanie Molin
}

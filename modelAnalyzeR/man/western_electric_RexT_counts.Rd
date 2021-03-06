% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/case_study_results.R
\name{western_electric_RexT_counts}
\alias{western_electric_RexT_counts}
\title{Western Electric Rules RexT Alert Counts}
\usage{
western_electric_RexT_counts(data, beginning_dates, middle_dates, end_dates,
  rule)
}
\arguments{
\item{data}{Dataframe of territory RexT results}

\item{beginning_dates}{Vector of dates for beginning of the month}

\item{middle_dates}{Vector of dates for middle of the month}

\item{end_dates}{Vector of dates for end of the month}

\item{rule}{Rule to filter for}
}
\value{
A list containing dataframe of RexT alert counts, a graph of the results, and a boxplot of the distribution
}
\description{
Calculate RexT client and campaign alert counts for Western Electric Rules
}
\author{
Stefanie Molin
}

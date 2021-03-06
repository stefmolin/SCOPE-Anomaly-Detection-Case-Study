% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ROC_curves.R
\name{fpr_threshold}
\alias{fpr_threshold}
\title{Get False Positive Rate using Thresholds}
\usage{
fpr_threshold(data, threshold, prediction_column)
}
\arguments{
\item{data}{Dataframe with a prediction and is_alert column}

\item{threshold}{Threshold for the prediction to be an alert (greater than or equal to)}

\item{prediction_column}{Column name of the prediction values}
}
\value{
FPR
}
\description{
Calculate false positive rate (FPR)
}
\author{
Stefanie Molin
}

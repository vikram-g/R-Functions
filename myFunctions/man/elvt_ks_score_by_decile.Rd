% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_evaluation.R
\name{elvt_ks_score_by_decile}
\alias{elvt_ks_score_by_decile}
\title{KS score by each decile}
\usage{
elvt_ks_score_by_decile(actual, prediction)
}
\arguments{
\item{actual}{List of actual values - 0s and 1s of the target variable}

\item{prediction}{List of predicted probabilities}
}
\value{
List of numeric values with KS at each deciles
}
\description{
Function returns the KS score at each decile of the predicted probability.
}


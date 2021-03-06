% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_preparation.R
\name{elvt_missing_impute}
\alias{elvt_missing_impute}
\title{Missing value handling}
\usage{
elvt_missing_impute(df, method = "median", acrossTrainTest = F,
  trainIndicator = NULL, WoE_bins = 10, WoE_target = NULL)
}
\arguments{
\item{df}{Data frame with only the columns to be imputed and target value(if WoE method is used)}

\item{method}{String value of method used to impute the missing values "median" or "WoE".}

\item{acrossTrainTest}{Logical value. If TRUE, then median only from train data is used to impute both train and test.}

\item{trainIndicator}{List of logical values indicating which row belongs to train. Length should be equal to number of rows of dataframe}

\item{WoE_bins}{Numeric value. If method is "WoE" this parameter is used to indicate number of bins used}

\item{WoE_target}{Name of target variable when method is "WoE"}
}
\value{
The original data frame with all the missing values imputed
}
\description{
Imputes the missing values either using median or WoE. Can also impute the missing value across train
}


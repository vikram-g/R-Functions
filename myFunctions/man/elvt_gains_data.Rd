% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_evaluation.R
\name{elvt_gains_data}
\alias{elvt_gains_data}
\title{Gains chart data}
\usage{
elvt_gains_data(actual, prediction, groups = 10)
}
\arguments{
\item{actual}{List of actual values - 0s and 1s of the target variable}

\item{prediction}{List of predicted probabilities}

\item{groups}{Number of groups the rows should be aggregated. This will be same as number of rows returned by the function. Defaults to 10.}
}
\value{
A dataframe containing following columns \cr         
1. depth -	cumulative percentage of file covered by each row of the gains table (e.g. 10,20,30,...,100).      \cr
2. obs -	number of observations in each row.     \cr
3. cume.obs - cumulative number of observations in each row.       \cr
4. mean.resp -	mean response in each row.     \cr
5. cume.mean.resp -	cumulative mean response in each row.      \cr
6. cume.pct.of.total -	cumulative percent of total response.       \cr
7. lift -	lift index. The lift index is 100 times the mean.resp for the row divided by the cume.mean.resp for the last row.       \cr
8. cume.lift -	cumulative lift index. It is 100 times the cume.mean.resp for the row divided by the cume.mean.resp for the last row.  \cr   
9. mean.prediction -	mean predicted response in each row.       \cr
10. min.prediction -	minimum predicted response in each row. min.prediction and max.prediction can be used to construct decision rules for applying the model.      \cr
11. max.prediction -	maximum predicted response in each row. \cr
}
\description{
This function calculates all the data needed to create a lift and gains chart.
}


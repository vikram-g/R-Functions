% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_checks.R
\name{elvt_column_sums_flat_file}
\alias{elvt_column_sums_flat_file}
\title{Columns sum from flatfile}
\usage{
elvt_column_sums_flat_file(fileName, fieldSeperator, colsToCount = NULL)
}
\arguments{
\item{fileName}{Name of the flat file to be analysed with full path.}

\item{fieldSeperator}{The field seperator used in the file}

\item{colsToCount}{List indicating column numbers for which the sums need to be calculated}
}
\value{
A list with column sum in the order corresponding to the input flat file.
}
\description{
The function calculates the sum of columns from flat file. This sum can later be compared with column sum from the final data after all processing is done. If a back end is registered, the processing is automatically done in parallel. Any character values present such as column names is ignored.
}


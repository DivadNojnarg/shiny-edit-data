% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app-utils.R
\name{find_cols_to_show}
\alias{find_cols_to_show}
\title{Find columns to show}
\usage{
find_cols_to_show(dat)
}
\arguments{
\item{dat}{Initial data without the editor columns.}
}
\value{
A vector of columns.
}
\description{
Used by \link{define_columns_diff}.
Either filtered columns if the config is not empty.
Otherwise return all columns except the app internal ones.
}

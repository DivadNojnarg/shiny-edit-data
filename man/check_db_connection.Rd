% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app-utils.R
\name{check_db_connection}
\alias{check_db_connection}
\title{Check database pool object}
\usage{
check_db_connection(state, loader, session)
}
\arguments{
\item{state}{App state.}

\item{loader}{R6 waiter instance.}

\item{session}{Shiny session object.}
}
\value{
A waiter object. Side effect: update the app
connected state.
}
\description{
Update loader if failure
}

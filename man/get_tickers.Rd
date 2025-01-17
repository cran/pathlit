% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tickers.R
\name{get_tickers}
\alias{get_tickers}
\title{Get Time Series}
\usage{
get_tickers()
}
\value{
Returns a character vector of the available tickers.
}
\description{
Retrieves the character vector of available tickers. The
time series of these tickers can be retrieved via
\link[pathlit]{get_timeseries}.
See \url{https://www.pathlit.io/docs/api/paths/} for more information.
}
\examples{
\dontrun{
get_tickers()
}
}
\author{
Riasat Ali Istiaque, \email{riasat.istiaque@pathlit.io}
}

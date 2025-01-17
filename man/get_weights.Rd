% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_weights.R
\name{get_weights}
\alias{get_weights}
\title{Get Weights}
\usage{
get_weights(tickers)
}
\arguments{
\item{tickers}{A character vector of instrument tickers.}
}
\value{
Returns a table of weights across the strategies for the tickers
provided.
}
\description{
Computes the weights to be allocated for a portfolio, under
different strategies. See \url{https://www.pathlit.io/docs/api/weights/} for
more information.
}
\examples{
\dontrun{
get_weights(c("AAPL", "HOG", "KO"))
}
}
\author{
Riasat Ali Istiaque, \email{riasat.istiaque@pathlit.io}
}

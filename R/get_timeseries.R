#' @title Get Time Series
#' @description Retrieves the time series of the tickers provided. The
#' comprehensive list of the tickers can be retrieved via
#' \link[pathlit]{get_tickers}.
#' See \url{https://www.pathlit.io/docs/api/paths/} for more information.
#' @author Riasat Ali Istiaque, \email{riasat.istiaque@pathlit.io}
#' @import timeSeries
#' @import httr
#' @param tickers A character vector of instrument tickers.
#' @return Returns a time series of the tickers provided.
#' @examples
#' \dontrun{
#' get_timeseries(c("AAPL", "MSFT"))
#' }
#' @export
get_timeseries <- function(tickers) {
  apiKey <- Sys.getenv("PATHLIT_CLIENT")
  if (apiKey == "") {
    stop("An API key is required.")
  }

  resp <- lapply(tickers, function(x) {
    timeSeries::as.timeSeries(unlist(httr::content(httr::GET(
      paste0(PROD_HOST, timeseries_path, x),
      httr::add_headers("x-api-key" = apiKey)
    ), "parsed")))
  })

  data <- do.call(cbind, resp)
  colnames(data) <- tickers

  return(data)
}

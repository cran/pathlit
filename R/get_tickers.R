#' @title Get Time Series
#' @description Retrieves the character vector of available tickers. The
#' time series of these tickers can be retrieved via
#' \link[pathlit]{get_timeseries}.
#' See \url{https://www.pathlit.io/docs/api/paths/} for more information.
#' @author Riasat Ali Istiaque, \email{riasat.istiaque@pathlit.io}
#' @import httr
#' @return Returns a character vector of the available tickers.
#' @examples
#' \dontrun{
#' get_tickers()
#' }
#' @export
get_tickers <- function() {
  apiKey <- Sys.getenv("PATHLIT_CLIENT")
  if (apiKey == "") {
    stop("An API key is required.")
  }

  resp <- httr::content(httr::GET(
    paste0(PROD_HOST, timeseries_path, "info"),
    httr::add_headers("x-api-key" = apiKey)
  ), "parsed")

  data <- do.call(c, resp)

  return(data)
}

#' @title Get Paths
#' @description Computes the dollar returns of a 100,000 USD portfolio
#' according to the weights computed by \link[pathlit]{get_weights}. See
#' \url{https://www.pathlit.io/docs/api/paths/} for more information.
#' @author Riasat Ali Istiaque, \email{riasat.istiaque@pathlit.io}
#' @import httr
#' @import jsonlite
#' @import timeSeries
#' @param tickers A character vector of instrument tickers.
#' @return Returns a time series of dollar movements across the strategies for
#' the tickers provided.
#' @examples
#' \dontrun{
#' get_paths(c("AAPL", "HOG", "KO"))
#' }
#' @export
get_paths <- function(tickers) {
  apiKey <- Sys.getenv("PATHLIT_CLIENT")
  if (apiKey == "") {
    stop("An API key is required.")
  }

  tickers <- list(tickers)
  names(tickers) <- "tickers"

  resp <- httr::content(httr::POST(
    paste0(PROD_HOST, paths_path),
    body = jsonlite::toJSON(tickers),
    httr::add_headers("x-api-key" = apiKey)
  ), "parsed")

  data <- as.data.frame(do.call(cbind, resp))
  rNames <- rownames(data)[-1]
  cNames <- unname(unlist(data[1, ]))
  data <- timeSeries::timeSeries(mapply(as.numeric, data[-1, ]), rNames)
  colnames(data) <- cNames

  return(data)
}

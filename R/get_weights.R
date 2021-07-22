#' @title Get Weights
#' @description Computes the weights to be allocated for a portfolio, under
#' different strategies. See \url{https://www.pathlit.io/docs/api/weights/} for
#' more information.
#' @author Riasat Ali Istiaque, \email{riasat.istiaque@pathlit.io}
#' @import httr
#' @import jsonlite
#' @import timeSeries
#' @param tickers A character vector of instrument tickers.
#' @return Returns a table of weights across the strategies for the tickers
#' provided.
#' @examples
#' \dontrun{
#' get_weights(c("AAPL", "HOG", "KO"))
#' }
#' @export
get_weights <- function(tickers) {
  apiKey <- Sys.getenv("PATHLIT_CLIENT")
  if (apiKey == "") {
    stop("An API key is required.")
  }

  tickers <- list(tickers)
  names(tickers) <- "tickers"

  resp <- httr::content(httr::POST(
    paste0(PROD_HOST, weights_path),
    body = jsonlite::toJSON(tickers),
    httr::add_headers("x-api-key" = apiKey)
  ), "parsed")

  data <- as.data.frame(do.call(rbind, resp))
  rNames <- rownames(data)
  data <- mapply(as.numeric, data)
  rownames(data) <- rNames
  colnames(data) <- tickers[[1]]

  return(data)
}

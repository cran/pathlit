#' @title Simulate
#' @description Computes the dollar returns of a 100,000 USD portfolio
#' according to the weights computed by \link[pathlit]{get_weights}, but across
#' simulated (normally-distributed) market data.
#' See \url{https://www.pathlit.io/docs/api/sims/} for more information.
#' @author Riasat Ali Istiaque, \email{riasat.istiaque@pathlit.io}
#' @import httr
#' @import jsonlite
#' @import parallel
#' @import timeSeries
#' @param tickers A character vector of instrument tickers.
#' @param run_count Defaults to 10 runs. An integer defining the number of
#' simulated market data universes.
#' @return Returns a list of multiple time series' of dollar movements across
#' the strategies for the tickers provided.
#' @examples
#' \dontrun{
#' Simulate(c("AAPL", "HOG", "KO"), 5)
#' }
#' @export
Simulate <- function(tickers, run_count = 10) {
  apiKey <- Sys.getenv("PATHLIT_CLIENT")
  if (apiKey == "") {
    stop("An API key is required.")
  }

  payload <- list(tickers, run_count)
  names(payload) <- c("tickers", "run_count")

  resp <- httr::content(httr::POST(
    paste0(PROD_HOST, sims_path),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    httr::add_headers("x-api-key" = apiKey)
  ), "parsed")

  data <- parallel::mclapply(seq(length(resp)), function(x) {
    temp <- resp[[x]]
    data <- as.data.frame(do.call(cbind, temp))
    rNames <- rownames(data)[-1]
    cNames <- unname(unlist(data[1, ]))
    data <- timeSeries::timeSeries(
      mapply(as.numeric, data[-1, ]), rNames
    )
    colnames(data) <- cNames
    resp[[x]] <- data
  })
  names(data) <- paste0("run_", seq(run_count))

  return(data)
}

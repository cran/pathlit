#' @title Get Information
#' @description Obtains the current version of the PathLit Engine. See
#' \url{https://www.pathlit.io/docs/api/info/} for more information.
#' @author Riasat Ali Istiaque, \email{riasat.istiaque@pathlit.io}
#' @import httr
#' @return Returns a message indicating PathLit Engine version number.
#' @examples
#' \dontrun{
#' get_info()
#' }
#' @export
get_info <- function() {
  apiKey <- Sys.getenv("PATHLIT_CLIENT")
  if (apiKey == "") {
    stop("An API key is required.")
  }

  message(httr::content(httr::GET(
    paste0(PROD_HOST, info_path),
    httr::add_headers("x-api-key" = apiKey)
  ), "parsed")[[1]])
}

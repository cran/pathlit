#' @title Client
#' @description Sets up client access.
#' @author Riasat Ali Istiaque, \email{riasat.istiaque@pathlit.io}
#' @param apiKey A character string containing the API key.
#' @return Returns the status of the local API key registration.
#' @examples
#' \dontrun{
#' Client("56gIvzm4dj1vJpNUlv3RJ2CeMd47JETG3bcf5zLS")
#' }
#' @export
Client <- function(apiKey) {
  Sys.unsetenv("PATHLIT_CLIENT")
  Sys.setenv("PATHLIT_CLIENT" = apiKey)
  if (Sys.getenv("PATHLIT_CLIENT") != "") {
    message("API key local registration successful.")
  } else {
    stop("API key local registration unsuccessful.")
  }
}

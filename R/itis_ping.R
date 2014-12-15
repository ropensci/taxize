#' Ping the ITIS API to see if it's working.
#'
#' @export
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @details See \code{\link{getdescription()}}, which provides number of scientific and
#' common names in a character string.
#' @examples \dontrun{
#' itis_ping()
#' }

itis_ping <- function(...) {
  res <- getdescription(...)
  grepl("this is the itis web service", res, ignore.case = TRUE)
}

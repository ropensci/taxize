#' Parse scientific names using Global Names Parser
#'
#' @export
#' @param names A vector of length 1 or more taxonomic names
#' @param ... Curl options passed on to [crul::verb-GET]
#' @return A data.frame with results, the submitted names, and the
#' parsed names with additional information.
#' @seealso [gbif_parse()], [gni_parse()]
#' @references <http://gni.globalnames.org/>
#' @examples \dontrun{
#' gn_parse("Cyanistes caeruleus")
#' gn_parse("Plantago minor")
#' gn_parse("Plantago minor minor")
#' gn_parse(c("Plantago minor minor","Helianthus annuus texanus"))
#' 
#' # if > 20 names, uses an HTTP POST request
#' x <- names_list("species", size = 30)
#' gn_parse(x)
#'
#' # pass on curl options
#' gn_parse("Cyanistes caeruleus", verbose = TRUE)
#' }
gn_parse <- function(names, ...) {
  assert(names, "character")
  method <- ifelse(length(names) <= 20, "get", "post")
  tmp <- gn_http(method, names, ...)
  tibble::as_tibble(jsonlite::fromJSON(tmp))
}

gn_http <- function(method, names, ...) {
  cli <- crul::HttpClient$new("https://parser.globalnames.org",
    headers = tx_ual, opts = list(...))
  res <- switch(method,
    get = {
      names <- paste0(names, collapse = "|")
      args <- list(q = names)
      cli$get("api", query = args)
    },
    post = {
      cli$headers <- c(cli$headers, list(`Content-Type` = "application/json",
        Accept = "application/json"))
      cli$post("api", body = jsonlite::toJSON(names))
    }
  )
  res$raise_for_status()
  res$parse("UTF-8")
}

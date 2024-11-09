#' Parse scientific names using Global Names Parser
#'
#' @export
#' @param names A vector of length 1 or more taxonomic names
#' @param ... Curl options passed on to [crul::verb-GET]
#' @return A data.frame with results, the submitted names, and the
#' parsed names with additional information.
#' @seealso [gbif_parse()], [gni_parse()]
#' @references http://gni.globalnames.org/
#' @examples \dontrun{
#' gna_parse("Cyanistes caeruleus")
#' gna_parse("Plantago minor")
#' gna_parse("Plantago minor minor")
#' gna_parse(c("Plantago minor minor","Helianthus annuus texanus"))
#' 
#' # if > 20 names, uses an HTTP POST request
#' x <- names_list("species", size = 30)
#' gna_parse(x)
#'
#' # pass on curl options
#' gna_parse("Cyanistes caeruleus", verbose = TRUE)
#' }
gna_parse <- function(names, ...) {
  assert(names, "character")
  method <- ifelse(length(names) <= 20, "get", "post")
  tmp <- gna_http(method, names, ...)
  out <- jsonlite::fromJSON(tmp)
  out[paste0('canonical_', colnames(out$canonical))] <- out$canonical
  out$canonical <- NULL
  tibble::as_tibble(out)
}

gna_http <- function(method, names, ...) {
  cli <- crul::HttpClient$new("https://parser.globalnames.org/api/v1/",
    headers = tx_ual, opts = list(...))
  res <- switch(method,
    get = {
      cli$get(paste0('api/v1/', paste0(names, collapse = "|")))
    },
    post = {
      cli$headers <- c(cli$headers, list(`Content-Type` = "application/json",
        accept = "application/json"))
      cli$post(body = jsonlite::toJSON(list(names = names)))
    }
  )
  res$raise_for_status()
  res$parse("UTF-8")
}


#' Parse scientific names using EOL's name parser.
#'
#' THIS FUNCTION IS DEFUNCT.
#' 
#' @export
#' @keywords internal
gni_parse <- function(names, ...) {
  .Defunct("ncbi_searcher", "traits",
           msg = "This function is defunct. See gna_parse()")
}

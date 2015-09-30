#' iPlant name resolution
#'
#' @export
#' @param query Vector of one or more taxonomic names. (no common names)
#' @param retrieve Specifies whether to retrieve all matches for the names submitted. One of 'best'
#' (retrieves only the single best match for each name submitted) or 'all' (retrieves all matches)
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @return A data.frame
#' @examples \dontrun{
#' iplant_resolve(query=c("Helianthus annuus", "Homo sapiens"))
#' iplant_resolve("Helianthusss")
#' iplant_resolve("Pooa")
#'
#' library("httr")
#' iplant_resolve("Helianthusss", config=verbose())
#' }
iplant_resolve <- function(query, retrieve='all', ...){
  url <- "http://tnrs.iplantc.org/tnrsm-svc/matchNames"
  query <- paste(query, collapse = ",")
  args <- tc(list(names = query, retrieve = retrieve))
  out <- GET(url, query = argsnull(args), ...)
  warn_for_status(out)
  tt <- content(out, as = "text")
  res <- jsonlite::fromJSON(tt, FALSE)$items
  df <- do.call(rbind, lapply(res, data.frame, stringsAsFactors = FALSE))
  if (!is.null(df)) nmslwr(df) else df
}

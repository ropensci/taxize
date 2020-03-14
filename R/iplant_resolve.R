#' iPlant name resolution
#'
#' @export
#' @param query Vector of one or more taxonomic names (no common names)
#' @param retrieve Specifies whether to retrieve all matches for the
#' names submitted. One of 'best' (retrieves only the single best match
#' for each name submitted) or 'all' (retrieves all matches)
#' @param ... Curl options passed on to [crul::verb-GET]
#' @return A data.frame
#' @examples \dontrun{
#' iplant_resolve(query=c("Helianthus annuus", "Homo sapiens"))
#' iplant_resolve("Helianthusss")
#' iplant_resolve("Pooa")
#' iplant_resolve("Helianthusss", verbose = TRUE)
#' }
iplant_resolve <- function(query, retrieve='all', ...){
  url <- "http://tnrs.iplantc.org/tnrsm-svc/matchNames"
  query <- paste(query, collapse = ",")
  args <- tc(list(names = query, retrieve = retrieve))
  cli <- crul::HttpClient$new(url, headers = tx_ual, opts = list(...))
  out <- cli$get(query = argsnull(args))
  out$raise_for_status()
  res <- jsonlite::fromJSON(out$parse("UTF-8"), FALSE)$items
  df <- do.call(rbind, lapply(res, data.frame, stringsAsFactors = FALSE))
  if (!is.null(df)) nmslwr(df) else df
}

#' Search the CANADENSYS Vascan API.
#'
#' @export
#' @param q (character) Can be a scientific name, a vernacular name or a VASCAN
#'    taxon identifier (e.g. 861)
#' @param format (character) One of json (default) or xml.
#' @param raw (logical) If TRUE, raw json or xml returned, if FALSE, parsed
#' data returned.
#' @param ... (list) Further args passed on to [crul::verb-GET]
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return json, xml or a list.
#' @references API docs <https://data.canadensys.net/vascan/api>
#' @keywords names taxonomy
#' @details Note that we lowercase all outputs in data.frame's, but when a
#' list is given back, we don't touch the list names.
#' @examples \dontrun{
#' vascan_search(q = "Helianthus annuus")
#' vascan_search(q = "Helianthus annuus", raw=TRUE)
#' vascan_search(q = c("Helianthus annuus", "Crataegus dodgei"), raw=TRUE)
#'
#' # format type
#' ## json
#' c <- vascan_search(q = "Helianthus annuus", format="json", raw=TRUE)
#' library("jsonlite")
#' fromJSON(c, FALSE)
#'
#' ## xml
#' d <- vascan_search(q = "Helianthus annuus", format="xml", raw=TRUE)
#' library("xml2")
#' xml2::read_xml(d)
#'
#' # lots of names, in this case 50
#' splist <- names_list(rank='species', size=50)
#' vascan_search(q = splist)
#'
#' # Curl options
#' invisible(vascan_search(q = "Helianthus annuus", verbose = TRUE))
#' }
vascan_search <- function(q, format='json', raw=FALSE, ...) {
  url <- sprintf("https://data.canadensys.net/vascan/api/0.1/search.%s",
    format)
  cli <- crul::HttpClient$new(url = url, headers = tx_ual,
    opts = list(...))

  if (!length(q) > 1) {
    temp <- cli$get(query = list(q = q))
    temp$raise_for_status()
    out <- temp$parse("UTF-8")
  } else {
    args <- paste(q, collapse = '\n')
    temp <- cli$post(body = list(q = args), encode = "form", ...)
    temp$raise_for_status()
    out <- temp$parse("UTF-8")
  }
  if (raw) {
    return( out )
  } else {
    tmp <- jsonlite::fromJSON(out, FALSE)
    res <- tmp$results[[1]]
    lapply(tmp$results, vascan_parse)
  }
}

vascan_parse <- function(x){
  parsed <- lapply(x$matches, function(y) {
    taxass <- nmslwr(data.frame(y$taxonomicAssertions,
      stringsAsFactors = FALSE))
    dist <- nmslwr(data.frame(rbindlist(y$distribution)))
    vern <- nmslwr(data.frame(rbindlist(y$vernacularNames)))
    list(taxonomicassertions = taxass, distribution = dist,
      vernacularnames = vern)
  })
  if (length(parsed) == 0) {
    nmslwr(data.frame(searchedterm = x$searchedTerm, nummatches = x$numMatches,
                      matches = NA, stringsAsFactors = FALSE))
  } else {
    list(searchedterm = x$searchedTerm, nummatches = x$numMatches,
      matches = parsed)
  }
}

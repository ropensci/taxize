#' ION - Index to Organism Names
#'
#' @export
#' @param x An LSID number. Required.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @references \url{http://www.organismnames.com/}
#' @return A data.frame
#' @examples \dontrun{
#' ion(155166)
#' ion(298678)
#' ion(4796748) # ursus americanus
#' ion(1280626) # puma concolor
#' }
ion <- function(x, ...) {
  x <- GET(ion_base(), query = list(lsid = x), ...)
  stop_for_status(x)
  xml <- xmlParse(content(x, "text"))
  dc <- sapply(c('identifier', 'Title'), function(x) {
    xpathApply(xml, paste0("//dc:", x),
               namespaces = c(dc = "http://purl.org/dc/elements/1.1/"), xmlValue)
  })
  tdwg <- setNames(xpathApply(xml, "//tdwg_tn:nameComplete",
               namespaces = c(tdwg_tn = "http://rs.tdwg.org/ontology/voc/TaxonName#"),
               xmlValue), "nameComplete")
  df <- data.frame(c(dc, tdwg), stringsAsFactors = FALSE)
  setNames(df, tolower(names(df)))
}

ion_base <- function() 'http://www.organismnames.com/lsidmetadata.htm'

#' ION - Index to Organism Names
#'
#' @export
#' @param x An LSID number. Required.
#' @param ... Curl options passed on to [crul::verb-GET]
#' @references <http://www.organismnames.com>
#' @return A data.frame
#' @examples \dontrun{
#' ion(155166)
#' ion(298678)
#' ion(4796748) # ursus americanus
#' ion(1280626) # puma concolor
#' }
ion <- function(x, ...) {
  cli <- crul::HttpClient$new(ion_base(), headers = tx_ual, opts = list(...))
  res <- cli$get(query = list(lsid = x))
  res$raise_for_status()
  xml <- xml2::read_xml(res$parse("UTF-8"))
  dc <- as.list(sapply(c('identifier', 'Title'), function(z) {
    xml_text(xml_find_all(xml, paste0("//dc:", z), xml_ns(xml)))
  }))
  tdwg <- as.list(
    stats::setNames(
      xml_text(xml_find_all(xml, "//tdwg_tn:nameComplete", xml_ns(xml))),
      "nameComplete")
  )
  df <- data.frame(c(dc, tdwg), stringsAsFactors = FALSE)
  stats::setNames(df, tolower(names(df)))
}

ion_base <- function() 'http://www.organismnames.com/lsidmetadata.htm'

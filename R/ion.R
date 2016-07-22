#' ION - Index to Organism Names
#'
#' @export
#' @param x An LSID number. Required.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @references http://www.organismnames.com
#' @return A data.frame
#' @examples \dontrun{
#' ion(155166)
#' ion(298678)
#' ion(4796748) # ursus americanus
#' ion(1280626) # puma concolor
#' }
ion <- function(x, ...) {
  res <- GET(ion_base(), query = list(lsid = x), ...)
  stop_for_status(res)
  xml <- xml2::read_xml(con_utf8(res))
  dc <- as.list(sapply(c('identifier', 'Title'), function(z) {
    xml_text(xml_find_all(xml, paste0("//dc:", z), xml_ns(xml)))
  }))
  tdwg <- as.list(
    setNames(
      xml_text(xml_find_all(xml, "//tdwg_tn:nameComplete", xml_ns(xml))),
      "nameComplete")
  )
  df <- data.frame(c(dc, tdwg), stringsAsFactors = FALSE)
  setNames(df, tolower(names(df)))
}

ion_base <- function() 'http://www.organismnames.com/lsidmetadata.htm'

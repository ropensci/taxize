#' NatureServe
#'
#' @export
#' @param x (character) A name to query. Required.
#' @param key (character) API key. Required.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @references \url{https://services.natureserve.org/index.jsp}
#' @return A data.frame, with columns:
#' \itemize{
#'  \item sciname - Scientfic name
#'  \item comname - Common name
#'  \item uid - UID - the taxonomic identifier NatureServe uses
#'  \item uri - URL to get to info online for the taxon
#'  \item taxcomment - comments about the taxon, if any
#' }
#' @examples \dontrun{
#' head(ns_search("Ruby*"))
#' ns_search("Helianthus annuus")
#'
#' ns_data(uid = 'ELEMENT_GLOBAL.2.100925')
#' }
ns_search <- function(x, key = "72ddf45a-c751-44c7-9bca-8db3b4513347", ...) {
  x <- GET(paste0(ns_base(), '/v1/globalSpecies/list/nameSearch'),
           query = list(name = x, NSAccessKeyId = key))
  stop_for_status(x)
  xml <- xml2::read_xml(content(x, "text"))
  kids <- xml2::xml_children(xml2::xml_children(xml)[[2]])
  dat <- lapply(kids, function(z) {
    data.frame(sapply(xml_children(z), function(x) {
      as.list(setNames(xml_text(x), xml_name(x)))
    }), stringsAsFactors = FALSE)
  })
  df <- setNames(rbind.fill(dat),
                 c('uid', 'uri', 'sciname', 'comname', 'taxcomment'))
  df <- move_col2(df, "uid")
  df <- move_col2(df, "uri")
  move_col2(df, "taxcomment")
}

#' @export
#' @rdname nature_serve
ns_data <- function(uid, key = "72ddf45a-c751-44c7-9bca-8db3b4513347", ...) {
  x <- GET(paste0(ns_base(), '/v1.1/globalSpecies/comprehensive'),
           query = list(uid = uid, NSAccessKeyId = key))
  stop_for_status(x)
  xml <- xml2::read_xml(content(x, "text"))
  kids <- xml2::xml_children(xml2::xml_children(xml)[[1]])
  dat <- lapply(kids, function(z) {
    data.frame(sapply(xml_children(z), function(x) {
      as.list(setNames(xml_text(x), xml_name(x)))
    }), stringsAsFactors = FALSE)
  })
  df <- setNames(rbind.fill(dat),
                 c('uid', 'uri', 'sciname', 'comname', 'taxcomment'))
  df <- move_col2(df, "uid")
  df <- move_col2(df, "uri")
  move_col2(df, "taxcomment")
}

ns_base <- function() 'https://services.natureserve.org/idd/rest/ns'

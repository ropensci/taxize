#' @title Index Fungorum
#'
#' @description Search for taxonomic names in Index Fungorum
#'
#' @name fungorum
#' @param q (character) Query term
#' @param anywhere (logical) Default: \code{TRUE}
#' @param limit (integer) Number of results to return. max limit
#' value appears to be 6000, not positive about that though
#' @param key (character) A IndexFungorum taxon key
#' @param lsid (character) an LSID, e.,g. "urn:lsid:indexfungorum.org:names:81085"
#' @param date (character) Date, of the form YYYMMDD
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @references \url{http://www.indexfungorum.org/}, API docs:
#' \url{http://www.indexfungorum.org/ixfwebservice/fungus.asmx}
#' @return A \code{data.frame}, or \code{NULL} if no results
#' @examples \dontrun{
#' # NameSearch
#' fg_name_search(q = "Gymnopus", limit = 2)
#' fg_name_search(q = "Gymnopus")
#'
#' # EpithetSearch
#' fg_epithet_search(q = "phalloides")
#'
#' # NameByKey
#' fg_name_by_key(17703)
#'
#' # NameFullByKey
#' fg_name_full_by_lsid("urn:lsid:indexfungorum.org:names:81085")
#'
#' # AllUpdatedNames
#' fg_all_updated_names(date = gsub("-", "", Sys.Date() - 2))
#'
#' # DeprecatedNames
#' fg_deprecated_names(date=20151001)
#'
#' # AuthorSearch
#' fg_author_search(q = "Fayod", limit = 2)
#' }

#' @export
#' @rdname fungorum
fg_name_search <- function(q, anywhere = TRUE, limit = 10, ...) {
  by_name_search("NameSearch", q, anywhere, limit, ...)
}

#' @export
#' @rdname fungorum
fg_author_search <- function(q, anywhere = TRUE, limit = 10, ...) {
  by_name_search("AuthorSearch", q, anywhere, limit, ...)
}

#' @export
#' @rdname fungorum
fg_epithet_search <- function(q, anywhere = TRUE, limit = 10, ...) {
  by_name_search("EpithetSearch", q, anywhere, limit, ...)
}

#' @export
#' @rdname fungorum
fg_name_by_key <- function(key, ...) {
  tmp <- fung_GET("NameByKey", list(NameKey = key), ...)
  fg_df(fung_parse(tmp))
}

#' @export
#' @rdname fungorum
fg_name_full_by_lsid <- function(lsid, ...) {
  tmp <- fung_GET("NameFullByKey", list(NameLsid = lsid), ...)
  xml2::xml_text(xml2::read_xml(tmp))
}

#' @export
#' @rdname fungorum
fg_all_updated_names <- function(date, ...) {
  tmp <- fung_GET("AllUpdatedNames", list(startDate = date), ...)
  xml <- fung_parse(tmp)
  (x <- setDF(rbindlist(lapply(xml, function(z) {
    vapply(xml_children(z), function(w) as.list(xml_text(w)), list(1))
  }))))
}

#' @export
#' @rdname fungorum
fg_deprecated_names <- function(date, ...) {
  tmp <- fung_GET("DeprecatedNames", list(startDate = date), ...)
  xml <- fung_parse(tmp)
  df <- setDF(rbindlist(
    lapply(xml, function(z) {
      vapply(xml_children(z), function(w) as.list(xml_text(w)), list(1))
    })
  ))
  if (NROW(df) > 0) setNames(df, c('fungusnameoldlsid', 'fungusnamenewlsid')) else df
}



# helpers -----------------
fung_base <- function() "http://www.indexfungorum.org/ixfwebservice/fungus.asmx"

fung_GET <- function(path, args, ...) {
  tt <- GET(file.path(fung_base(), path), query = args, ...)
  stop_for_status(tt)
  con_utf8(tt)
}

fung_parse <- function(x) {
  xml <- xml2::read_xml(x)
  xml_find_all(xml, "//IndexFungorum")
}

fg_df <- function(x) {
  (x <- setDF(rbindlist(
    lapply(x, function(z) {
      data.frame(
        lapply(xml_children(z), function(w) as.list(setNames(xml_text(w), gsub("x0020_", "", tolower(xml_name(w)))))),
        stringsAsFactors = FALSE
      )
    }), use.names = TRUE, fill = TRUE
  )))
}

by_name_search <- function(path, q, anywhere, limit, ...) {
  args <- tc(list(SearchText = q, AnywhereInText = as_l(anywhere), MaxNumber = limit))
  tmp <- fung_GET(path, args, ...)
  fg_df(fung_parse(tmp))
}

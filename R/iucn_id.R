#' Get an ID for a IUCN listed taxon
#'
#' @export
#' @param sciname character; Scientific name. Should be cleand and in the
#' format *<Genus> <Species>*. One or more.
#' @param key (character) required. you IUCN Redlist API key. See
#' [`rredlist::rredlist-package`] for help on authenticating with
#' IUCN Redlist
#' @param ... Curl options passed on to [`crul::HttpClient`]
#' @return A named list (names are input taxa names) of one or more IUCN IDs.
#' Taxa that aren't found are silently dropped.
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' iucn_id("Branta canadensis")
#' iucn_id("Branta bernicla")
#' iucn_id("Panthera uncia")
#' iucn_id("Lynx lynx")
#'
#' # many names
#' iucn_id(c("Panthera uncia", "Lynx lynx"))
#'
#' # many names, some not found
#' iucn_id(c("Panthera uncia", "Lynx lynx", "foo bar", "hello world"))
#'
#' # a name not found
#' iucn_id("Foo bar")
#' }
iucn_id <- function(sciname, key = NULL, ...) {
  out <- list()
  for (i in seq_along(sciname)) {
    out[[i]] <- get_iucn_id(sciname[[i]], key = key, ...)
  }
  unlist(out)
}

get_iucn_id <- function(z, key = NULL, ...) {
  tmp <- rredlist::rl_search(z, key = key, ...)
  if (NROW(tmp$result) == 0) {
    NA
  } else if (NROW(tmp$result) > 1) {
    iduniq <- unique(tmp$result$taxonid)
    if (length(iduniq) == 1) {
      iduniq
    } else {
      stop("> 1 result found, alter your query", call. = FALSE)
    }
  } else {
    tmp$result$taxonid
  }
}


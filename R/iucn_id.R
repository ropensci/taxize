#' Get an ID for a IUCN listed taxon
#'
#' @export
#' @param sciname character; Scientific name. Should be cleand and in the
#' format \emph{<Genus> <Species>}. One or more.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
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
iucn_id <- function(sciname, ...) {
  out <- list()
  for (i in seq_along(sciname)) {
    out[[i]] <- get_iucn_id(sciname[[i]], ...)
  }
  unlist(out)
}

get_iucn_id <- function(z, ...) {
  tmp <- rredlist::rl_search(z, ...)
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

# get_iucn_id <- function(z) {
#   x <- gsub(" ", "-", tolower(z))
#   url <- paste("http://api.iucnredlist.org/go/", x, sep = "")
#   e <- tryCatch(suppressWarnings(readLines(url)), error = function(e) e)
#   if (is(e, "error")) {
#     warning(paste0(z, " - not found"), call. = FALSE)
#     return(NA)
#   } else {
#     id <- grep("http://www.iucnredlist.org/apps/redlist/details/", e, value = TRUE)
#     as.numeric(gsub(".*/", "", id))
#   }
# }

## usage based on using gnr_resolve, maybe bring back when correct IUCN IDs given
# iucn_id <- function(sciname) {
#   res <- gnr_resolve(sciname, data_source_ids = 163, fields = "all")
#   as.list(setNames(res$local_id, res$submitted_name))
# }

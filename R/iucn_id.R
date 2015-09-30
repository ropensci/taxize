#' Get an ID for a IUCN listed taxon
#'
#' @export
#' @param sciname character; Scientific name. Should be cleand and in the
#' format \emph{<Genus> <Species>}. One or more.
#' @return A vector of one or more numeric IDs. Warnings are thrown
#' when the taxon is not found
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
iucn_id <- function(sciname) {
  out <- list()
  for (i in seq_along(sciname)) {
    out[[i]] <- get_iucn_id(sciname[[i]])
  }
  unlist(out)
}

get_iucn_id <- function(z) {
  x <- gsub(" ", "-", tolower(z))
  url <- paste("http://api.iucnredlist.org/go/", x, sep = "")
  e <- tryCatch(suppressWarnings(readLines(url)), error = function(e) e)
  if (is(e, "error")) {
    warning(paste0(z, " - not found"), call. = FALSE)
    return(NA)
  } else {
    id <- grep("http://www.iucnredlist.org/apps/redlist/details/", e, value = TRUE)
    as.numeric(gsub(".*/", "", id))
  }
}

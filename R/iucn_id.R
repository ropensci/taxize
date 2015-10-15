#' Get an ID for a IUCN listed taxon
#'
#' @export
#' @param sciname character; Scientific name. Should be cleand and in the
#' format \emph{<Genus> <Species>}. One or more.
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
iucn_id <- function(sciname) {
  res <- gnr_resolve(sciname, data_source_ids = 163, fields = "all")
  as.list(setNames(res$taxon_id, res$submitted_name))
}

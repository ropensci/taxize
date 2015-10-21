#' Get any matching IUCN species names
#'
#' @export
#' @param name character; taxon name
#' @param verbose logical; should messages be printed?
#' @param ... Further arguments passed on to \code{link{iucn_summary}}
#' @seealso \code{\link{iucn_summary}} \code{\link{iucn_status}}
#' @return Character vector of names that matched in IUCN
#' @examples \dontrun{
#' iucn_getname(name = "Cyanistes caeruleus")
#' iucn_getname(name = "Panthera uncia")
#' iucn_getname(name = "Abies")
#'
#' # not found in global names
#' # iucn_getname(name = "Abronia pinsapo")
#'
#' # not found in IUCN search
#' # iucn_getname(name = "Acacia allenii")
#' }
iucn_getname <- function(name, verbose = TRUE, ...) {
  mssg(verbose, "searching Global Names ...")
  all_names <- gni_search(search_term = name, parse_names = TRUE)
  if (NROW(all_names) == 0) {
    stop("No names found matching ", name, call. = FALSE)
  }
  mssg(verbose, "searching IUCN...")
  res <- gnr_resolve(all_names$canonical, data_source_ids = 163, fields = "all")
  unique(res$canonical_form)
  # out <- suppressWarnings(iucn_summary(all_names$canonical, ...))
  # as.character(all_names$canonical[!sapply(out, function(x) x$status) %in% NA])
}

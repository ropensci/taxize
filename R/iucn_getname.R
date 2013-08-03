#' Get any matching IUCN species names
#' 
#' @param name character; taxon name
#' @param verbose logical; should messages be printed?
#' @param ... Further arguments passed on to \code{link{iucn_summary}}
#' @seealso \code{\link{iucn_summary}} \code{\link{iucn_status}}
#' @export
#' @examples \dontrun{
#' iucn_getname(name = "Cyanistes caeruleus")
#' }
iucn_getname <- function(name, verbose = TRUE, ...)
{
  message_verbose <- function(x){ if(verbose) message(x) }
  
  message_verbose("searching EOL's Global Names Index...")
  all_names <- gni_search(search_term = name, parse_names = TRUE)
  message_verbose("searching IUCN...")
  out <- suppressWarnings(iucn_summary(all_names$canonical, ...))
  as.character(all_names$canonical[!sapply(out, function(x) x$status) %in% NA])
}
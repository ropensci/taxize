#' Search for taxonomy data from Plantminer.com
#'
#' @export
#' @param plants (character) Vector of plant species names. Required.
#' @param from (character) One of tpl (for theplantlist.com data), or
#'    flora (for Brazilian Flora Checklist). Required. Default: `tpl`
#' @param messages (logical) informative messages or not. Default: `TRUE`
#' @param ... curl options passed on to [crul::HttpClient]
#' @return data.frame of results.
#' @note you used to need an API key for Plantminer; it's no longer needed
#' @examples \dontrun{
#' # A single taxon
#' plantminer("Ocotea pulchella")
#'
#' # Many taxa
#' plants <- c("Myrcia lingua", "Myrcia bella", "Ocotea pulchella",
#' 		"Miconia", "Coffea arabica var. amarella", "Bleh")
#' plantminer(plants)
#'
#' # By deafult, tpl is used, for Theplantlist data,
#' # toggle the from parameter here
#' plantminer("Ocotea pulchella", from = "flora")
#' }
plantminer <- function(plants, from = "tpl", messages = TRUE, ...) {
  if (!from %in% c("tpl", "flora")) {
    stop("'from' must be one of 'tpl' or 'flora'")
  }
  do.call(rbind, lapply(seq_along(plants), function(i) {
    mssg(messages, paste(plants[i], collapse = " "))
    cli <- crul::HttpClient$new(url = pmbase(), headers = tx_ual,
                                opts = list(...))
    sp <- cli$get(from, query = list(taxon = plants[i]))
    sp$raise_for_status()
    jsonlite::fromJSON(sp$parse("UTF-8"))
  }))
}

pmbase <- function() "http://www.plantminer.com"

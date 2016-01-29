#' Search for taxonomy data from Plantminer.com
#'
#' @export
#' @param plants (character) Vector of plant species names. Required.
#' @param from (character) One of tpl (for theplantlist.com data), or
#'    flora (for Brazilian Flora Checklist). Required. Deafult: \code{tpl}
#' @param key (character) Your api key for the plantminer.com site.  Go to
#' 		http://www.plantminer.com/ to get your api key.  Two options for
#' 		inputting your key.  1) You can input it manually within the function as
#' 		the second argument, or 2) you can put the key in your .Rprofile file,
#' 		which will then be loaded when you start R. See
#' 		http://bit.ly/135eG0b for help on how to put api keys in your .Rprofile file.
#' @param verbose (logical) Verbose or not. Deafult: \code{TRUE}
#' @return data.frame of results.
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
plantminer <- function(plants, from = "tpl", key = NULL, verbose = TRUE) {
  from <- match.arg(from, c("tpl", "flora"))
	key <- getkey(key, "pmApiKey")
  i <- NULL
  foreach(i = 1:length(plants), .combine = rbind) %do% {
    mssg(verbose, paste(plants[i], collapse = " "))
    sp <- GET(paste0(pmbase(), from), query = list(key = key, taxon = plants[i]))
    stop_for_status(sp)
    jsonlite::fromJSON(con_utf8(sp))
  }
}

pmbase <- function() "http://www.plantminer.com/"

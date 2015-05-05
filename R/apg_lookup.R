#' Lookup in the APGIII taxonomy and replace family names
#'
#' @export
#' @param taxa (character) Taxonomic name to lookup a synonym for in APGIII taxonomy.
#' @param rank (character) Taxonomic rank to lookup a synonym for. One of family or order.
#' @return A APGIII family or order name, or the original name if no match.
#' @details Internally in this function, we use the datasets \code{\link{apg_families}}
#' and \code{\link{apg_orders}} - see their descriptions for the data in them.
#' The functions \code{\link{apgOrders}} \code{\link{apgFamilies}} are for scraping
#' current content from the \url{http://www.mobot.org/MOBOT/research/APweb/} website.
#'
#' BEWARE: The datasets used in this function are (I think) from Version 12 of the data
#' on \url{http://www.mobot.org/MOBOT/research/APweb/} - I'll update data asap.
#' @examples
#' # New name found
#' apg_lookup(taxa = "Hyacinthaceae", rank = "family")
#' apg_lookup(taxa = "Poaceae", rank = "family")
#'
#' # Name not found
#' apg_lookup(taxa = "Asteraceae", rank = "family")
apg_lookup <- function(taxa, rank = "family") {
  if (!rank %in% c('family', 'order')) {
    stop("rank must be one of family or order")
  }
  if (rank == "family") {
    temp <- as.character(apg_families[apg_families$this %in% taxa, "that"])
    if (nchar(temp) == 0) {
      message("no match found...")
      out <- taxa
    } else {
      message("new name...")
      out <- temp
    }
  } else {
    if (rank == "order") {
      temp <- as.character(apg_orders[apg_orders$this %in% taxa, "that"])
      if (nchar(temp) == 0) {
        message("no match found...")
        out <- taxa
      } else {
        message("new name...")
        out <- temp
      }
    }
  }
  return(out)
}

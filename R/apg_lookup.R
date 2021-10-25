#' Lookup in the APGIII taxonomy and replace family names
#'
#' @export
#' @param taxa (character) Taxonomic name to lookup a synonym for
#' in APGIII taxonomy.
#' @param rank (character) Taxonomic rank to lookup a synonym for. One of
#' family or order.
#' @return A APGIII family or order name, the original name if the name
#' is the same as APG has, or NA if no match found
#' @details Internally in this function, we use the datasets [apg_families]
#' and [apg_orders] - see their descriptions for the data in them.
#' The functions [get_apg_orders()] [get_apg_families()] are for scraping
#' current content from the http://www.mobot.org/MOBOT/research/APweb/
#' website
#'
#' The datasets used in this function are from the most recent version of
#' APGIII, Version 14 (http://www.mobot.org/MOBOT/research/APweb/)
#' @examples
#' # New name found
#' apg_lookup(taxa = "Hyacinthaceae", rank = "family")
#' # Name is the same
#' apg_lookup(taxa = "Poaceae", rank = "family")
#' apg_lookup(taxa = "Asteraceae", rank = "family")
#' # Name not found
#' apg_lookup(taxa = "Foobar", rank = "family")
#' 
#' # New name found
#' apg_lookup(taxa = "Acerales", rank = "order")
#' # Name is the same
#' apg_lookup(taxa = "Acorales", rank = "order")
#' # Name not found
#' apg_lookup(taxa = "Foobar", rank = "order")
apg_lookup <- function(taxa, rank = "family") {
  if (!rank %in% c('family', 'order')) {
    stop("rank must be one of family or order")
  }
  af <- taxize_ds$apg_families
  if (rank == "family") {
    x <- af[af$family %in% taxa,]$synonym
    if (length(x) == 0) {
      message("no match found...")
      out <- NA_character_
    } else {
      if (is.na(x)) {
        message("name is the same...")
        out <- taxa
      } else {
        message("new name...")
        out <- x
      }
    }
  } else {
    ao <- taxize_ds$apg_orders
    x <- ao[ao$order %in% taxa,]$synonym
    if (length(x) == 0) {
      message("no match found...")
      out <- NA_character_
    } else {
      if (is.na(x)) {
        message("name is the same...")
        out <- taxa
      } else {
        message("new name...")
        out <- x
      }
    }
  }
  return(out)
}

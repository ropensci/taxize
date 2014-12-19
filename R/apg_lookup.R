#' Lookup in the APGII taxonomy and replace family names.
#'
#' @export
#' @param taxa Taxonomic name to lookup a synonym for in APGII taxonomy.
#' @param rank Taxonomic rank to lookup a synonym for. One of family or order.
#' @return A APGII family or order name, or the original name if no match.
#' @examples
#' # New name found in the APGII taxonomy
#' apg_lookup(taxa = "Hyacinthaceae", rank = "family")
#' apg_lookup(taxa = "Poaceae", rank = "family")
#'
#' # Name not found in the APGII taxonomy
#' apg_lookup(taxa = "Asteraceae", rank = "family")
apg_lookup <- function(taxa, rank = "family")
{
  if (!rank %in% c('family', 'order'))
    stop("rank must be one of family or order")
  if (rank == "family"){
    temp <- as.character(apg_families[apg_families$this %in% taxa, "that"])
    if (nchar(temp)==0){
      message("no match found...")
      out <- taxa
    } else {
      message("new name...")
      out <- temp
    }
  } else {
    if(rank == "order"){
      temp <- as.character(apg_orders[apg_orders$this %in% taxa, "that"])
      if(nchar(temp)==0){
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

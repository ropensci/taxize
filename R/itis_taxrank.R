#' Retrieve taxonomic rank name from given TSN.
#' 
#' @export
#' @param query TSN for a taxonomic group (numeric). If query is left as default
#'    (NULL), you get all possible rank names, and their TSN's (using function
#'    \code{\link{getranknames}}. There is slightly different terminology for 
#'    Monera vs. Plantae vs. Fungi vs. Animalia vs. Chromista, so there are separate 
#'    terminologies for each group.
#' @param ... Further arguments passed on to \code{\link{gettaxonomicranknamefromtsn}}
#' @details You can print messages by setting verbose=FALSE.
#' @return Taxonomic rank names or data.frame of all ranks.
#' @examples \dontrun{
#' # All ranks
#' itis_taxrank()
#' 
#' # A single TSN
#' itis_taxrank(query=202385)
#' # without message
#' itis_taxrank(query=202385, verbose=FALSE)
#' 
#' # Many TSN's
#' itis_taxrank(query=c(202385,183833,180543))
#' }

itis_taxrank <- function(query = NULL, ...)
{
  if(is.null(query)){
    getranknames()
  } else
  {
    sapply(query, function(z) as.character(gettaxonomicranknamefromtsn(z, ...)$rankName))
  }
}
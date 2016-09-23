#' Retrieve taxonomic rank name from given TSN.
#'
#' @export
#' @param query TSN for a taxonomic group (numeric). If query is left as default
#' (NULL), you get all possible rank names, and their TSN's (using function
#' \code{\link[ritis]{rank_names}}. There is slightly different terminology for
#' Monera vs. Plantae vs. Fungi vs. Animalia vs. Chromista, so there are
#' separate terminologies for each group.
#' @param ... Further arguments passed on to \code{\link[ritis]{rank_name}}
#' @details You can print messages by setting \code{verbose=FALSE}.
#' @return Taxonomic rank names or data.frame of all ranks.
#' @examples \dontrun{
#' # All ranks
#' itis_taxrank()
#'
#' # A single TSN
#' itis_taxrank(query=202385)
#'
#' # Many TSN's
#' itis_taxrank(query=c(202385,183833,180543))
#' }
itis_taxrank <- function(query = NULL, ...) {
  if (is.null(query)) {
    ritis::rank_names()
  } else {
    sapply(
      query,
      function(z) as.character(ritis::rank_name(z, ...)$rankname)
    )
  }
}

#' Resolve names using Open Tree of Life resolver
#'
#' @export
#' @param names taxon names to be queried (character vector)
#' @param context_name name of the taxonomic context to be searched
#'     (length-one character vector). Must match (case sensitive) one
#'     of the values returned by \code{\link[rotl]{tnrs_contexts}}.
#' @param do_approximate_matching A logical indicating whether or not
#'     to perform approximate string (a.k.a. \dQuote{fuzzy})
#'     matching. Using \code{FALSE} will greatly improve
#'     speed. Default, however, is \code{TRUE}.
#' @param ids An array of ids to use for identifying names. These
#'     will be assigned to each name in the names array. If ids is
#'     provided, then ids and names must be identical in length.
#' @param include_deprecated A boolean indicating whether or not to
#'     include deprecated taxa in the search.
#' @param include_dubious Whether to include so-called 'dubious'
#'     taxa--those which are not accepted by OTT.
#' @return A data frame summarizing the results of the query. The
#'     original query output is appended as an attribute to the
#'     returned object (and can be obtained using \code{attr(object,
#'     "original_response")}).
#' @param ... Curl options passed on to \code{\link[httr]{POST}} within
#' \code{\link[rotl]{tnrs_match_names}}
#' @author Francois Michonneau \email{francois.michonneau@@gmail.com}
#' Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @seealso \code{\link{gnr_resolve}}, \code{\link{tnrs}}
#' @references
#' \url{https://github.com/OpenTreeOfLife/opentree/wiki/Open-Tree-of-Life-APIs#match_names}
#' @keywords resolve names taxonomy
#' @examples \dontrun{
#' tol_resolve(names=c("echinodermata", "xenacoelomorpha", "chordata", "hemichordata"))
#' tol_resolve(c("Hyla", "Salmo", "Diadema", "Nautilus"))
#' tol_resolve(c("Hyla", "Salmo", "Diadema", "Nautilus"), context_name = "Animals")
#'
#' turducken_spp <- c("Meleagris gallopavo", "Anas platyrhynchos", "Gallus gallus")
#' tnrs_match_names(turducken_spp, context="Animals")
#' }
tol_resolve <- function(names = NULL, context_name = NULL, do_approximate_matching = TRUE,
                        ids = NULL, include_deprecated = FALSE, include_dubious = FALSE, ...) {

  rotl::tnrs_match_names(names = names, context_name = context_name,
                         do_approximate_matching = do_approximate_matching,
                         ids = ids, include_deprecated = include_deprecated,
                         include_dubious = include_dubious, ...)
}

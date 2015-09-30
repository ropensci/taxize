#' @title NCBI taxon information from uids
#'
#' @description Downloads summary taxon information from the NCBI taxonomy databases
#' for a set of taxonomy UIDs using eutils esummary.
#'
#' @export
#' @param id (character) NCBI taxonomy uids to retrieve information for.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @return A \code{data.frame} with the following rows:
#'   \describe{
#'     \item{uid}{The uid queried for}
#'     \item{name}{The name of the taxon; a binomial name if the taxon is of rank species}
#'     \item{rank}{The taxonomic rank (e.g. 'Genus')}
#'   }
#' @author Zachary Foster \email{zacharyfoster1989@@Sgmail.com}
#' @examples \dontrun{
#' ncbi_get_taxon_summary(c(1430660, 4751))
#'
#' # use curl options
#' library("httr")
#' ncbi_get_taxon_summary(c(1430660, 4751), config = verbose())
#' }
ncbi_get_taxon_summary <- function(id, ...) {
  # Argument validation ----------------------------------------------------------------------------
  if (is.null(id)) return(NULL)
  if (length(id) <= 1 && is.na(id)) return(NA)
  id <- as.character(id)
  # Make eutils esummary query ---------------------------------------------------------------------
  base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=taxonomy"
  query <- paste0(base_url, "&id=", paste(id, collapse = "+"))
  # Search ncbi taxonomy for uid -------------------------------------------------------------------
  rr <- GET(query, ...)
  stop_for_status(rr)
  raw_results <- content(rr, "text")
  # Parse results ----------------------------------------------------------------------------------
  results <- XML::xmlTreeParse(raw_results, useInternalNodes = TRUE)
  output <- data.frame(stringsAsFactors = FALSE,
                       uid = XML::xpathSApply(results, "/eSummaryResult//DocSum/Id", XML::xmlValue),
                       name = XML::xpathSApply(results, "/eSummaryResult//DocSum/Item[@Name='ScientificName']",
                                               XML::xmlValue),
                       rank = XML::xpathSApply(results, "/eSummaryResult//DocSum/Item[@Name='Rank']", XML::xmlValue)
  )
  output$rank[output$rank == ''] <- "no rank"
  Sys.sleep(0.34) # NCBI limits requests to three per second
  return(output)
}

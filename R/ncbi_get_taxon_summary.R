#' NCBI taxon information from uids
#' 
#' Downloads summary taxon information from the NCBI taxonomy databases for a set of taxonomy uids
#' using eutils esummary.
#' 
#' @param id (character) NCBI taxonomy uids to retrieve information for.
#' @return A \code{data.frame} with the following rows:
#'   \describe{
#'     \item{uid}{The uid queried for}
#'     \item{name}{The name of the taxon; a binomial name if the taxon is of rank species}
#'     \item{rank}{The taxonomic rank (e.g. 'Genus')}
#'   }
#' @examples
#' \dontrun{
#' ncbi_get_taxon_summary(c(1430660, 4751))}
#' @author Zachary Foster \email{zacharyfoster1989@@Sgmail.com}
#' @export
ncbi_get_taxon_summary <- function(id) {
  # Argument validation ----------------------------------------------------------------------------
  if (length(id) <= 1 && is.na(id)) return(NA)
  if (is.null(id)) return(NULL)
  id <- as.character(id)
  # Make eutils esummary query ---------------------------------------------------------------------
  base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=taxonomy"
  query <- paste0(base_url, "&id=", paste(id, collapse = "+"))
  # Search ncbi taxonomy for uid -------------------------------------------------------------------
  raw_results <- RCurl::getURL(query)
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

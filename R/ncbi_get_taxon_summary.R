#' @title NCBI taxon information from uids
#'
#' @description Downloads summary taxon information from the NCBI taxonomy 
#' databases for a set of taxonomy UIDs using eutils esummary.
#'
#' @export
#' @param id (character) NCBI taxonomy uids to retrieve information for. 
#' See Details.
#' @param key (character) NCBI Entrez API key. optional. See Details.
#' @param ... Curl options passed on to \code{\link[crul]{HttpClient}}
#' @return A \code{data.frame} with the following columns:
#'   \describe{
#'     \item{uid}{The uid queried for}
#'     \item{name}{The name of the taxon; a binomial name if the taxon 
#'     is of rank species}
#'     \item{rank}{The taxonomic rank (e.g. 'Genus')}
#'   }
#' @author Zachary Foster \email{zacharyfoster1989@@Sgmail.com}
#' @details If your input vector or list of NCBI IDs is longer than about 
#' 8000 characters (use \code{nchar(paste(ids, collapse = "+"))}), split 
#' the list up into chunks since at about that number of characters you 
#' will run into the HTTP 414 error "Request-URI Too Long".
#' 
#' @section Authentication:
#' See \code{\link{taxize-authentication}} for help on authentication
#' 
#' @examples \dontrun{
#' ncbi_get_taxon_summary(c(1430660, 4751))
#'
#' # use curl options
#' ncbi_get_taxon_summary(c(1430660, 4751), verbose = TRUE)
#' }
ncbi_get_taxon_summary <- function(id, key = NULL, ...) {
  # Argument validation -------------------------------------------------------
  if (is.null(id)) return(NULL)
  if (length(id) <= 1 && is.na(id)) return(NA)
  id <- as.character(id)
  if (nchar(paste(id, collapse = "+")) > 8000L) {
    message("You may want to split your ids up into chunks")
  }
  # Make eutils esummary query ------------------------------------------------
  key <- getkey(key, "ENTREZ_KEY")
  query <- tc(list(db = "taxonomy", id = paste(id, collapse = "+"), api_key = key))
  # Search ncbi taxonomy for uid ----------------------------------------------
  cli <- crul::HttpClient$new(url = ncbi_base(), opts = list(...))
  rr <- cli$get("entrez/eutils/esummary.fcgi", query = query)
  rr$raise_for_status()
  raw_results <- rr$parse("UTF-8")
  # Parse results -------------------------------------------------------------
  results <- xml2::read_xml(raw_results)
  output <- data.frame(stringsAsFactors = FALSE,
    uid = xml_text_all(results, "/eSummaryResult//DocSum/Id"),
    name = xml_text_all(results, 
      "/eSummaryResult//DocSum/Item[@Name='ScientificName']"),
    rank = xml_text_all(results, "/eSummaryResult//DocSum/Item[@Name='Rank']")
  )
  output$rank[output$rank == ''] <- "no rank"
  # NCBI limits requests to three per second
  if (is.null(key)) Sys.sleep(0.33)
  return(output)
}

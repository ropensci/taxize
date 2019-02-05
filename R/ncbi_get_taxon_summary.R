#' @title NCBI taxon information from uids
#'
#' @description Downloads summary taxon information from the NCBI taxonomy 
#' databases for a set of taxonomy UIDs using eutils esummary.
#'
#' @export
#' @param id (character) NCBI taxonomy uids to retrieve information for. 
#' See Details.
#' @param key (character) NCBI Entrez API key. optional. See Details.
#' @param ... Curl options passed on to \code{\link[crul]{verb-GET}}
#' @return A \code{data.frame} with the following columns:
#'   \describe{
#'     \item{uid}{The uid queried for}
#'     \item{name}{The name of the taxon; a binomial name if the taxon 
#'     is of rank species}
#'     \item{rank}{The taxonomic rank (e.g. 'Genus')}
#'   }
#' @author Zachary Foster \email{zacharyfoster1989@@Sgmail.com}
#' @details If your input vector or list of NCBI IDs is longer than about 
#' 2500 characters (use \code{nchar(paste(ids, collapse = "+"))}), split 
#' the list up into chunks since at about that number of characters you 
#' will run into the HTTP 414 error "Request-URI Too Long".
#' 
#' @section Authentication:
#' See \code{\link{taxize-authentication}} for help on authentication. 
#' We strongly recommend getting an API key
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
  toolong <- nchar(paste(id, collapse = "+")) > 2500L
  if (toolong) message(sub("\n", "", "Number of ids long; we're splitting up into 
chunks for multiple HTTP requests"))
  # Make eutils esummary query ------------------------------------------------
  key <- getkey(key, "ENTREZ_KEY")
  # split into chunks if needed
  if (toolong) {
    ids_list <- lapply(split(id, ceiling(seq_along(id) / 250)), paste, collapse = "+")
  } else {
    ids_list <- list(paste(id, collapse = "+"))
  }

  cli <- crul::HttpClient$new(url = ncbi_base(), opts = list(...))
  out <- list()
  for (i in seq_along(ids_list)) {
    query <- tc(list(db = "taxonomy", id = ids_list[[i]], api_key = key))
    # Search ncbi taxonomy for uid ----------------------------------------------
    rr <- cli$get("entrez/eutils/esummary.fcgi", query = query)
    if (!rr$success()) {
      warning("query failed, proceeding to next if there is one")
      out[[i]] <- paste0(rr$status_code, ": ", rr$status_http()$message)
    } else {
      out[[i]] <- parse_ncbi_gts(rr)
    }
    if (is.null(key)) Sys.sleep(0.33)
  }
  # filter to only data.frame's
  out <- Filter(is.data.frame, out)

  df <- dt2df(out)
  df$.id <- NULL
  return(df)
}

parse_ncbi_gts <- function(x) {    
  raw_results <- x$parse("UTF-8")
  results <- xml2::read_xml(raw_results)
  output <- data.frame(stringsAsFactors = FALSE,
    uid = xml_text_all(results, "/eSummaryResult//DocSum/Id"),
    name = xml_text_all(results, 
      "/eSummaryResult//DocSum/Item[@Name='ScientificName']"),
    rank = xml_text_all(results, "/eSummaryResult//DocSum/Item[@Name='Rank']")
  )
  output$rank[output$rank == ''] <- "no rank"
  return(output)
}

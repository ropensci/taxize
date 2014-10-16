#' Search NCBI for children of a taxon
#' 
#' Search the NCBI Taxonomy database for uids of children of taxa. Taxa can be referenced by name
#' or uid. Referencing by name is faster.
#' 
#' In a few cases, different taxa have the same name (e.g. Satyrium; see examples). If one of these
#' are searched for then the children of both taxa will be returned. This can be avoided by
#' using a uid instead of the name or specifying an ancestor. If an ancestor is provided, only 
#' children of both the taxon and its ancestor are returned. This will only fail if there are two
#' taxa with the same name and the same specified ancestor. 
#' 
#' @param name (\code{character}) The string to search for. Only exact matches found the name given
#' will be returned. Not compatible with \code{id}.
#' @param id (\code{character}) The uid to search for. Not compatible with \code{name}.
#' @param start The first record to return. If omitted, the results are returned from the first
#'   record (start=0). 
#' @param max_return (\code{numeric; length=1}) The maximum number of children to return.
#' @param ancestor (\code{character}) The ancestor of the taxon being searched for. This is useful
#'   if there could be more than one taxon with the same name. Has no effect if \code{id} is used.
#' @param out_type (character) Currently either \code{"summary"} or \code{"uid"}:
#'   \describe{
#'     \item{summary}{The output is a list of \code{data.frame} with children uid, name, and rank.}
#'     \item{uid}{A list of character vectors of children uids}
#'   }
#' @param ambiguous \code{logical; length 1} If \code{FALSE}, children taxa with words like 
#'   "unclassified", "unknown", "uncultured", or "sp." are removed from the output.
#'   NOTE: This option only applies when \code{out_type = "summary"}.
#' @return The output type depends on the value of the \code{out_type} parameter. 
#' @seealso \code{\link{ncbi_get_taxon_summary}}, \code{\link[taxize]{children}}
#' @examples
#' \donttest{
#' ncbi_children(name="Satyrium") #Satyrium is the name of two different genera
#' ncbi_children(name="Satyrium", ancestor="Eumaeini") # A genus of butterflies
#' ncbi_children(name="Satyrium", ancestor="Orchidaceae") # A genus of orchids
#' ncbi_children(id="266948") #"266948" is the uid for the butterfly genus
#' ncbi_children(id="62858") #"62858" is the uid for the orchid genus}
#' @author Zachary Foster \email{zacharyfoster1989@@gmail.com}
#' @export
ncbi_children <- function(name = NULL, id = NULL, start = 0, max_return = 1000,
                          ancestor = NULL, out_type = c("summary", "uid"), ambiguous = FALSE) {
  # Constants --------------------------------------------------------------------------------------
  ambiguous_regex <- paste(sep = "|", "unclassified", "environmental", "uncultured", "unknown",
                           "unidentified", "candidate", "sp\\.", "s\\.l\\.", "sensu lato", "clone",
                           "miscellaneous", "candidatus", "affinis", "aff\\.")
  base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=taxonomy"
  # Argument validation ----------------------------------------------------------------------------
  if (sum(c(is.null(name), is.null(id))) != 1) {
    stop("Either name or id must be specified, but not both")
  }
  out_type <- match.arg(out_type)
  # Get name from id -------------------------------------------------------------------------------
  if (is.null(name)) {
    if (class(id) != 'uid') attr(id, 'class') <- 'uid'
    id_taxonomy <- classification(id, db = 'ncbi')
    name <- vapply(id_taxonomy,
                   function(x) ifelse(nrow(x) > 0, x$name[nrow(x)], as.character(NA)),
                   character(1))
    ancestor <- vapply(id_taxonomy,
                       function(x) ifelse(nrow(x) > 1, x$name[nrow(x) - 1], as.character(NA)),
                       character(1)) 
  } else if (is.null(ancestor)) {
    ancestor <- rep(NA, length(name))
  }
  # Function to search for queries one at a time ---------------------------------------------------
  single_search <- function(name, ancestor) {
    if (is.na(name)) return(NA)
    # Make eutils esearch query  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (is.na(ancestor)) {
      ancestor_query <- NULL
    } else {
      ancestor_query <- paste0("+AND+", ancestor, "[subtree]")
    }
    taxon_query <- paste0("term=", name, "[Next+Level]", ancestor_query)
    max_return_query <- paste0("RetMax=", max_return)
    start_query <- paste0("RetStart=", start)
    query <- paste(base_url, taxon_query, max_return_query, start_query, sep="&")
    query <- gsub(" ", "+", query) #spaces must be replaced with '+'
    # Search ncbi for children - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    raw_results <- RCurl::getURL(query)
    # Parse results  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    results <- XML::xmlTreeParse(raw_results, useInternalNodes = TRUE)
    children_uid <- XML::xpathSApply(results, "//eSearchResult/IdList/Id", XML::xmlValue)
    if (length(children_uid) == 0) {
      output <- NULL
    } else {
      if (out_type == "summary") {
        output <- ncbi_get_taxon_summary(children_uid)
        names(output) <- c("childtaxa_id", "childtaxa_name", "childtaxa_rank")
        # Remove ambiguous results
        if (!ambiguous) {
          output <- output[!grepl(ambiguous_regex, output$childtaxa_name, ignore.case = TRUE), ]          
        }
      } else {
        output <- children_uid
      }
      rownames(output) <- NULL # numeric row names can be misleading after filtering
    }
    Sys.sleep(0.34) # NCBI limits requests to three per second
    return(output)
  }
  #Combine the result of multiple searches ----------------------------------------------------------
  output <- Map(single_search, name, ancestor)
  if (is.null(id)) names(output) <- name else names(output) <- id
  return(output)
}

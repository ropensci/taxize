#' @title Search NCBI for children of a taxon
#'
#' @description Search the NCBI Taxonomy database for uids of children of taxa. Taxa can
#' be referenced by name or uid. Referencing by name is faster.
#'
#' In a few cases, different taxa have the same name (e.g. Satyrium; see examples). If one of these
#' are searched for then the children of both taxa will be returned. This can be avoided by
#' using a uid instead of the name or specifying an ancestor. If an ancestor is provided, only
#' children of both the taxon and its ancestor are returned. This will only fail if there are two
#' taxa with the same name and the same specified ancestor.
#'
#' @export
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
#' @param key (character) NCBI Entrez API key. optional. See Details.
#' @param ... Curl options passed on to \code{\link[crul]{HttpClient}}
#' @return The output type depends on the value of the \code{out_type} parameter. Taxa that cannot
#' be found will result in \code{NA}s and a lack of children results in an empty data structure.
#' @seealso \code{\link{ncbi_get_taxon_summary}}, \code{\link[taxize]{children}}
#' @author Zachary Foster \email{zacharyfoster1989@@gmail.com}
#' 
#' @section Authentication:
#' See \code{\link{taxize-authentication}} for help on authentication
#'
#' @examples
#' \dontrun{
#' ncbi_children(name="Satyrium") #Satyrium is the name of two different genera
#' ncbi_children(name="Satyrium", ancestor="Eumaeini") # A genus of butterflies
#' ncbi_children(name="Satyrium", ancestor="Orchidaceae") # A genus of orchids
#' ncbi_children(id="266948") #"266948" is the uid for the butterfly genus
#' ncbi_children(id="62858") #"62858" is the uid for the orchid genus
#'
#' # use curl options
#' ncbi_children(name="Satyrium", ancestor="Eumaeini", verbose = TRUE)
#' }
ncbi_children <- function(name = NULL, id = NULL, start = 0, max_return = 1000,
                          ancestor = NULL, out_type = c("summary", "uid"), ambiguous = FALSE, 
                          key = NULL, ...) {

  key <- getkey(key, "ENTREZ_KEY")

  # Constants --------------------------------------------------------------------------------------
  ambiguous_regex <- paste(sep = "|", "unclassified", "environmental", "uncultured", "unknown",
                           "unidentified", "candidate", "sp\\.", "s\\.l\\.", "sensu lato", "clone",
                           "miscellaneous", "candidatus", "affinis", "aff\\.", "incertae sedis",
                           "mixed", "samples", "libaries")
  # base_url <- paste0(ncbi_base(), "/entrez/eutils/esearch.fcgi?db=taxonomy")
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
  single_search <- function(name, ancestor, ...) {
    if (is.na(name)) return(NA)
    # Make eutils esearch query  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (is.na(ancestor)) {
      ancestor_query <- NULL
    } else {
      ancestor_query <- paste0("+AND+", ancestor, "[subtree]")
    }

    args <- list(
      db = 'taxonomy', 
      term = paste0(name, "[Next+Level]", ancestor_query),
      RetMax = max_return,
      RetStart = start,
      api_key = key
    )
    args$term <- gsub("\\+", " ", args$term)

    # Search ncbi for children - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    cli <- crul::HttpClient$new(ncbi_base(), opts = list(...))
    rr <- cli$get('entrez/eutils/esearch.fcgi', query = args)
    rr$raise_for_status()
    raw_results <- rr$parse("UTF-8")

    # Parse results  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    results <- xml2::read_xml(raw_results)
    children_uid <- xml2::xml_text(xml2::xml_find_all(results, "//eSearchResult/IdList/Id"))
    if (length(children_uid) == 0) {
      if (out_type == "summary") {
        output <- data.frame(
          childtaxa_id     = character(),
          childtaxa_name   = character(),
          childtaxa_rank   = character(),
          stringsAsFactors = FALSE
        )
      } else {
        output <- numeric()
      }
    } else {
      if (out_type == "summary") {
        output <- ncbi_get_taxon_summary(children_uid, ...)
        names(output) <- c("childtaxa_id", "childtaxa_name", "childtaxa_rank")
        # Remove ambiguous results  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
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
  # output <- Map(single_search, name, ancestor)
  output <- list()
  for (i in seq_along(name)) {
    output[[i]] <- single_search(name[[i]], ancestor[[i]], ...)
  }
  if (is.null(id)) names(output) <- name else names(output) <- id
  return(output)
}

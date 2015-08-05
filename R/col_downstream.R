#' Use Catalogue of Life to get downstream taxa to a given taxonomic level.
#'
#' @import XML
#' @param name The string to search for. Only exact matches found the name given
#'     will be returned, unless one or wildcards are included in the search
#'   	string. An * (asterisk) character denotes a wildcard; a % (percentage)
#'    character may also be used. The name must be at least 3 characters long,
#'    not counting wildcard characters.
#' @param id The record ID of the specific record to return (only for scientific
#'   	names of species or infraspecific taxa)
#' @param downto The taxonomic level you want to go down to. See examples below.
#' 		The taxonomic level IS case sensitive, and you do have to spell it
#' 		correctly. See \code{data(rank_ref)} for spelling.
#' @param format The returned format (default = NULL). If NULL xml is used.
#'    Currently only xml is supported.
#' @param start  The first record to return (default = NULL). If NULL, the
#'    results are returned from the first record (start=0). This is useful if
#'    the total number of results is larger than the maximum number of results
#'    returned by a single Web service query (currently the maximum number of
#'    results returned by a single query is 500 for terse queries and 50 for
#'    full queries).
#' @param checklist The year of the checklist to query, if you want a specific
#' 		year's checklist instead of the lastest as default (numeric).
#' @param verbose Print or suppress messages.
#' @param intermediate (logical) If TRUE, return a list of length two with target
#'    taxon rank names, with additional list of data.frame's of intermediate
#'    taxonomic groups. Default: FALSE
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @details Provide only names instead of id's
#' @return A list of data.frame's.
#' @export
#' @examples \dontrun{
#' # Some basic examples
#' col_downstream(name="Apis", downto="Species")
#' col_downstream(name="Bryophyta", downto="Family")
#'
#' # get classes down from the kingdom Animalia
#' col_downstream(name="Animalia", downto="Class")
#' col_downstream(name="Animalia", downto="Class", intermediate=TRUE)
#'
#' # An example that takes a bit longer
#' col_downstream(name=c("Plantae","Animalia"), downto="Class")
#'
#' # Using a checklist from a specific year
#' col_downstream(name="Bryophyta", downto="Family", checklist=2009)
#'
#' # By id
#' col_downstream(id=2346405, downto="Genus", checklist=2012)
#' }

col_downstream <- function(name = NULL, id = NULL, downto, format = NULL, start = NULL,
  checklist = NULL, verbose = TRUE, intermediate = FALSE, ...)
{
  url = "http://www.catalogueoflife.org/col/webservice"
  downto <- taxize_capwords(downto)
  poss_ranks <- unique(do.call(c, sapply(rank_ref$ranks, strsplit, split=",", USE.NAMES = FALSE)))
  downto <- match.arg(downto, choices = poss_ranks)

  searchcol <- function(x=NULL, y=NULL, ...) {
    args <- tc(list(name=x, id=y, format=format, response="full", start=start))
    out_ <- GET(url, query = argsnull(args), ...)
    out_ <- content(out_, "text")
    tt <- xmlParse(out_)

    childtaxa_id <- xpathSApply(tt, "//child_taxa//id", xmlValue)
    childtaxa_name <- xpathSApply(tt, "//child_taxa//name", xmlValue)
    childtaxa_rank <- xpathSApply(tt, "//child_taxa//rank", xmlValue)
    out <- data.frame(childtaxa_id, childtaxa_name, childtaxa_rank, stringsAsFactors = FALSE)
    return(out)
  }

  func <- function(x=NULL, y=NULL, ...) {
    if(!is.null(checklist)) {
      cc <- match.arg(as.character(checklist), choices=c(2012,2011,2010,2009,2008,2007))
      if(cc %in% c(2012,2011,2010)){
        url <- gsub("col", paste("annual-checklist/", cc, sep=""), url)
      } else {
        url <- "http://webservice.catalogueoflife.org/annual-checklist/year/search.php"
        url <- gsub("year", cc, url)
      }
    }

    torank <- sapply(rank_ref[grep(downto, rank_ref$ranks),"ranks"],
                function(x) strsplit(x, ",")[[1]][[1]], USE.NAMES=FALSE)

    toget <- ifelse(is.null(y), x, y)
    stop_ <- "not"
    notout <- data.frame(rankName = "")
    out <- list()
    if(intermediate) intermed <- list()
    iter <- 0
    while(stop_ == "not"){
      iter <- iter + 1
      if(is.null(x)){
        tt <- ldply(toget, function(z) searchcol(y=z, ...))
      } else {
        tt <- ldply(toget, function(z) searchcol(x=z, ...))
      }

      # remove
      if(nrow(tt[tt$childtaxa_rank == downto, ]) > 0)
        out[[iter]] <- tt[tt$childtaxa_rank == downto, ]
      if(nrow(tt[!tt$childtaxa_rank == downto, ]) > 0) {
        notout <- tt[!tt$childtaxa_rank %in% torank, ]
      } else {
        notout <- data.frame(rankName = downto)
      }
      if(all(notout$childtaxa_rank == downto)) {
        stop_ <- "fam"
      } else {
        if(intermediate) intermed[[iter]] <- notout
        x <- NULL
        toget <- as.character(notout$childtaxa_id)
        stop_ <- "not"
      }
    } # end while loop

    if(length(out) == 0){
      ret <-  data.frame(childtaxa_id=NA, childtaxa_name=NA, childtaxa_rank=NA)
    } else {
      res <- tc(out)
      ret <- do.call(rbind.fill, res)
    }
    if(intermediate) list(target=ret, intermediate=intermed) else ret
  } # end fxn func

  safe_func <- plyr::failwith(NULL, func)
  if (is.null(id)) {
    temp <- setNames(lapply(name, safe_func, y=NULL, ...), name)
  } else {
    temp <- setNames(lapply(id, function(z) safe_func(x=NULL, y=id, ...)), id)
  }

  nas <- sapply(temp, function(z) NROW(na.omit( if(intermediate) z$target else z )))
  if (verbose) message(sprintf('These taxa with no data: %s\nTry adjusting input parameters', names(nas[nas==0])))
  return( temp )
}

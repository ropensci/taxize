#' Use Catalogue of Life to get downstream taxa to a given taxonomic level.
#' 
#' @import RCurl XML plyr
#' @param name The string to search for. Only exact matches found the name given 
#'     will be returned, unless one or wildcards are included in the search 
#'   	string. An * (asterisk) character denotes a wildcard; a % (percentage) 
#'    character may also be used. The name must be at least 3 characters long, 
#'    not counting wildcard characters.
#' @param downto The taxonomic level you want to go down to. See examples below.
#' 		The taxonomic level IS case sensitive, and you do have to spell it 
#' 		correctly. See \code{data(rank_ref)} for spelling.
#' @param checklist The year of the checklist to query, if you want a specific 
#' 		year's checklist instead of the lastest as default (numeric).
#' @param format The returned format (default = NULL). If NULL xml is used. 
#'    Currently only xml is supported.
#' @param start  The first record to return (default = NULL). If NULL, the 
#'    results are returned from the first record (start=0). This is useful if 
#'    the total number of results is larger than the maximum number of results 
#'    returned by a single Web service query (currently the maximum number of 
#'    results returned by a single query is 500 for terse queries and 50 for 
#'    full queries).
#' @details Provide only names instead of id's
#' @return A list of data.frame's.
#' @export
#' @examples \dontrun{
#' # Some basic examples
#' col_downstream(name="Apis", downto="Species")
#' col_downstream(name="Bryophyta", downto="Family")
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

col_downstream <- function(name = NULL, id=NULL, downto, format = NULL, start = NULL, checklist = NULL)
{
  url = "http://www.catalogueoflife.org/col/webservice"
  downto <- taxize_capwords(downto)
  poss_ranks <- unique(do.call(c, sapply(rank_ref$ranks, strsplit, split=",", USE.NAMES = FALSE)))
  downto <- match.arg(downto, choices = poss_ranks)
  
  func <- function(x=NULL, y=NULL) {
    if(is.null(checklist)){NULL} else {
      cc <- match.arg(as.character(checklist), choices=c(2012,2011,2010,2009,2008,2007))
      if(cc %in% c(2012,2011,2010)){
        url <- gsub("col", paste("annual-checklist/", cc, sep=""), url)
      } else
      {
        url <- "http://webservice.catalogueoflife.org/annual-checklist/year/search.php"
        url <- gsub("year", cc, url)
      }
    }
    
    torank <- sapply(rank_ref[grep(downto, rank_ref$ranks):nrow(rank_ref),"ranks"], function(x) strsplit(x, ",")[[1]][[1]], USE.NAMES=F)
    
    toget <- ifelse(is.null(y), x, y) 
    stop_ <- "not" 
    notout <- data.frame(rankName = "")
    out <- list()
    iter <- 0
    while(stop_ == "not"){
      iter <- iter + 1
      
      searchcol <- function(x=NULL, y=NULL) {
        args <- compact(list(name=x, id=y, format=format, response="full", start=start))
        out_ <- getForm(url, .params = args)
        tt <- xmlParse(out_)
        
        childtaxa_id <- xpathSApply(tt, "//child_taxa//id", xmlValue)
        childtaxa_name <- xpathSApply(tt, "//child_taxa//name", xmlValue)
        childtaxa_rank <- xpathSApply(tt, "//child_taxa//rank", xmlValue)
        data.frame(childtaxa_id, childtaxa_name, childtaxa_rank, stringsAsFactors = FALSE)
      }
      
      if(is.null(x)){
        tt <- ldply(toget, function(z) searchcol(y=z))
      } else
      {
        tt <- ldply(toget, function(z) searchcol(x=z))
      }
      
      if(nrow(tt[tt$childtaxa_rank == downto, ]) > 0) out[[iter]] <- tt[tt$childtaxa_rank == downto, ]
      if(nrow(tt[!tt$childtaxa_rank == downto, ]) > 0) {
        notout <- tt[!tt$childtaxa_rank %in% torank, ]
      } else
      { notout <- data.frame(rankName = downto) }
      
      if(all(notout$childtaxa_rank == downto)) { 
        stop_ <- "fam"
      } else
      { 
        toget <- as.character(notout$childtaxa_name)
        stop_ <- "not" 
      }
      
    } # end while loop
    return( compact(out)[[1]] )
    
  } # end fxn func
  
  safe_func <- plyr::failwith(NULL, func)
  if(is.null(id)){ 
    temp <- lapply(name, safe_func, y=NULL) 
    names(temp) <- name
    temp
  } else { 
    temp <- lapply(id, function(z) safe_func(x=NULL, y=id)) 
    names(temp) <- id
    temp
  }
  
  #   safe_func <- plyr::failwith(NULL, func)
  #   temp <- llply(name, safe_func)
  #   names(temp) <- name
  #   temp
}
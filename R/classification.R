#' Retrieve the taxonomic hierarchy for a given taxon ID.
#' 
#' @import XML RCurl plyr
#' 
#' @param x character; taxons to query.
#' @param db character; database to query. either \code{ncbi}, \code{itis}, 
#'    \code{eol}, \code{col} or \code{tropicos}.
#' @param id character; identifiers, returned by \code{\link[taxize]{get_tsn}}, 
#'    \code{\link[taxize]{get_uid}}, \code{\link[taxize]{get_eolid}}, 
#'    \code{\link[taxize]{get_colid}}, or \code{\link[taxize]{get_tpsid}}.
#' @param ... Other arguments passed to \code{\link[taxize]{get_tsn}}, 
#'    \code{\link[taxize]{get_uid}}, \code{\link[taxize]{get_eolid}}, 
#'    \code{\link[taxize]{get_colid}}, or \code{\link[taxize]{get_tpsid}}.
#' @param start The first record to return. If omitted, the results are returned 
#' 		from the first record (start=0). This is useful if the total number of 
#' 		results is larger than the maximum number of results returned by a single 
#' 		Web service query (currently the maximum number of results returned by a 
#' 		single query is 500 for terse queries and 50 for full queries).
#' @param checklist character; The year of the checklist to query, if you want a specific 
#' 		year's checklist instead of the lastest as default (numeric).
#' @param key Your API key; loads from .Rprofile.
#' @param callopts Further args passed on to httr::GET.
#' 
#' @return A named list of data.frames with the taxonomic classifcation of 
#'    every supplied taxa.
#' @note If IDs are supplied directly (not from the \code{get_*} functions) you 
#'    must specify the type of ID. There is a timeout of 1/3 seconds between 
#'    querries to NCBI.
#' 
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}}, 
#'    \code{\link[taxize]{get_eolid}}, \code{\link[taxize]{get_colid}}, 
#'    \code{\link[taxize]{get_tpsid}}
#' 
#' @export
#' @examples \dontrun{
#' # Plug in taxon names directly
#' classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'itis')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'eol')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'col')
#' classification(c("Poa annua", "aaa vva"), db = 'tropicos')
#' 
#' # Use methods for get_uid, get_tsn, get_eolid, get_colid, get_tpsid
#' classification(get_uid(c("Chironomus riparius", "Puma concolor")))
#' 
#' classification(get_uid(c("Chironomus riparius", "aaa vva")))
#' classification(get_tsn(c("Chironomus riparius", "aaa vva")))
#' classification(get_eolid(c("Chironomus riparius", "aaa vva")))
#' classification(get_colid(c("Chironomus riparius", "aaa vva")))
#' classification(get_tpsid(c("Poa annua", "aaa vva")))
#' 
#' # Pass many ids from class "ids"
#' out <- get_ids(names="Poa annua", db = c('ncbi','itis','col','eol','tropicos'))
#' classification(out)
#' }
#' 
#' @examples \donttest{
#' # Fails
#' classification(315576)
#' }
classification <- function(...){
  UseMethod("classification")
}

#' @method classification default
#' @export
#' @rdname classification
classification.default <- function(x, db = NULL, ...){
  if (is.null(db))
    stop("Must specify db!")
  if (db == 'itis') {
    id <- get_tsn(x, ...)
    out <- classification(id, ...)
    names(out) <- x
  }
  if (db == 'ncbi') {
    id <- get_uid(x, ...)
    out <- classification(id, ...)
    names(out) <- x
  }
  if (db == 'eol') {
    id <- get_eolid(x, ...)
    out <- classification(id, ...)
    names(out) <- x
  }
  if (db == 'col') {
    id <- get_colid(x, ...)
    out <- classification(id, ...)
    names(out) <- x
  }
  if (db == 'tropicos') {
    id <- get_tpsid(x, ...)
    out <- classification(id, ...)
    names(out) <- x
  }
  return(out)
}

#' @method classification tsn
#' @export
#' @rdname classification
classification.tsn <- function(id, ...) 
{
  fun <- function(x){
    # return NA if NA is supplied
    if (is.na(x)) {
      out <- NA
    } else {
    	out <- getfullhierarchyfromtsn(x, ...)
    	# remove overhang
    	out <- out[1:which(out$tsn == x), c('taxonName', 'rankName')]
      names(out) <- c('name', 'rank')
    	return(out)
    }
  }
  out <- lapply(id, fun)
  attr(out, 'db') <- 'itis'
  return(out)
}


#' @method classification uid
#' @export
#' @rdname classification
classification.uid <- function(id, ...) {
  fun <- function(x){
    # return NA if NA is supplied
    if(is.na(x)){
      out <- NA
    } else {
      baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy"
      ID <- paste("ID=", x, sep = "")
      searchurl <- paste(baseurl, ID, sep = "&")
      tt <- getURL(searchurl)
      ttp <- xmlTreeParse(tt, useInternalNodes = TRUE)
      out <- data.frame(name = xpathSApply(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/ScientificName", xmlValue), 
                        rank = xpathSApply(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/Rank", xmlValue),
                        stringsAsFactors = FALSE)
      out <- rbind(out, c(xpathSApply(ttp, "//TaxaSet/Taxon/ScientificName", xmlValue),
        xpathSApply(ttp, "//TaxaSet/Taxon/Rank", xmlValue), 
        xpathSApply(ttp, "//TaxaSet/Taxon/TaxId", xmlValue)))
      return(out)
    }
    # NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(out)
  }
  out <- lapply(id, fun)
  attr(out, 'db') <- 'ncbi'
  return(out)
}


#' @method classification eolid
#' @export
#' @rdname classification
classification.eolid <- function(id, key = NULL, callopts = list(), ...) {
  fun <- function(x){
    if(is.na(x)){ 
      out <- NA
    } else {
      url = 'http://www.eol.org/api/hierarchy_entries/1.0/'
      key <- getkey(key, "eolApiKey")
      urlget <- paste(url, x, '.json', sep="")
      args <- compact(list(common_names=common_names, synonyms=synonyms))
      tt <- GET(urlget, query=args, callopts)
      stop_for_status(tt)
      res <- content(tt)
      if(length(res$ancestors)==0){
        return(sprintf("No hierarchy information for %s", x))
      } else {
        out <- do.call(rbind.fill, lapply(res$ancestors, data.frame))[,c('scientificName','taxonRank')]
        names(out) <- c('name', 'rank')
        return(out)
      }
    }
  }
  out <- lapply(id, fun)
#   names(out) <- id
  return(out)
}

#' @method classification colid
#' @export
#' @rdname classification
classification.colid <- function(id, start = NULL, checklist = NULL, ...) {
  fun <- function(x){
    # return NA if NA is supplied
    if(is.na(x)){
      out <- NA
    } else {
      url <- "http://www.catalogueoflife.org/col/webservice"
      if(!is.null(checklist)){
        cc <- match.arg(checklist, choices = c(2012, 2011, 2010, 2009, 2008, 2007))
        if (cc %in% c(2012, 2011, 2010)) {
          url <- gsub("col", paste("annual-checklist/", cc, sep = ""), url)
        } else {
          url <- "http://webservice.catalogueoflife.org/annual-checklist/year/search.php"
          url <- gsub("year", cc, url)
        }
      }
        
      args <- compact(list(id = x, response = "full", start = start))
      out <- getForm(url, .params = args)
      tt <- xmlParse(out)
      
      out <- data.frame(name = xpathSApply(tt, "//classification//name", xmlValue),
                        rank = xpathSApply(tt, "//classification//rank", xmlValue),
                        stringsAsFactors = FALSE)
    }
    return(out)
  }
  out <- lapply(id, fun)
  names(out) <- id
  attr(out, 'db') <- 'col'
  return(out)
}


#' @method classification tpsid
#' @export
#' @rdname classification
classification.tpsid <- function(id, key = NULL, callopts = list(), ...) {
  fun <- function(x){
    if(is.na(x)) {
      out <- NA
    } else {
      url <- sprintf('http://services.tropicos.org/Name/%s/HigherTaxa', x)
      key <- getkey(key, "tropicosApiKey")
      args <- compact(list(format='json', apikey=key))
      tt <- GET(url, query = args, callopts)
      stop_for_status(tt)
      out <- content(tt)
      if(names(out[[1]])[[1]] == "Error"){ 
        out <- data.frame(ScientificName=NA, Rank=NA) 
      } else {
        out <- do.call(rbind.fill, lapply(out, data.frame))[,c('ScientificName','Rank')]
      }
      names(out) <- c('name', 'rank')
    }
    return(out)
  }
  out <- lapply(id, fun)
  names(out) <- id
  return(out)
}


#' @method classification ids
#' @export
#' @rdname classification
classification.ids <- function(id, ...) 
{
  fun <- function(x, ...){
    # return NA if NA is supplied
    if (is.na(x)) {
      out <- NA
    } else {
      out <- classification(x, ...)
    }
    return(out)
  }
  out <- lapply(id, fun)
  return(out)
}
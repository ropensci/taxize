#' Retrieve the taxonomic hierarchy for a given taxon ID.
#' 
#' @import XML RCurl plyr
#' 
#' @param x character; taxons to query.
#' @param db character; database to query. either \code{ncbi}, \code{itis}, or \code{eol}.
#' @param id character; identifiers, returned by \code{\link[taxize]{get_tsn}} 
#'    or \code{\link[taxize]{get_uid}}
#' @param ... Other arguments passed to \code{\link[taxize]{get_tsn}},  
#' \code{\link[taxize]{get_uid}}, or \code{\link[taxize]{get_eolid}}.
#' 
#' @return A named list of data.frames with the taxonomic classifcation of 
#'    every supplied taxa.
#' @note If IDs are supplied directly (not from the \code{get_*} functions) you 
#'    must specify the type of ID. There is a timeout of 1/3 seconds between 
#'    querries to NCBI.
#' 
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}}, 
#' \code{\link[taxize]{get_eolid}}
#' 
#' @export
#' @examples \dontrun{
#' # Plug in taxon names directly
#' classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'itis')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'eol')
#' 
#' # Use methods for get_uid, get_tsn and get_eolid
#' classification(get_uid(c("Chironomus riparius", "Puma concolor")))
#' 
#' classification(get_uid(c("Chironomus riparius", "aaa vva")))
#' classification(get_tsn(c("Chironomus riparius", "aaa vva")))
#' classification(get_eolid(c("Chironomus riparius", "aaa vva")))
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
    stop("Must specify Identifier!")
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
    	out <- out[1:which(out$tsn == x), c('rankName', 'taxonName', 'tsn')]
    	return(out)
    }
  }
  out <- lapply(id, fun)
  names(out) <- id
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
      out <- data.frame(ScientificName = xpathSApply(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/ScientificName", xmlValue), 
                        Rank = xpathSApply(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/Rank", xmlValue), 
                        UID = xpathSApply(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/TaxId", xmlValue),
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
  names(out) <- id
  return(out)
}


#' @method classification eolid
#' @export
#' @rdname classification
classification.eolid <- function(id, ...) {
  fun <- function(x){
    # return NA if NA is supplied
    if(is.na(x)){
      tmp <- NA
    } else {
      tmp <- eol_hierarchy(taxonid=x)
    }
    return(tmp)
  }
  out <- lapply(id, fun)
  names(out) <- id
  return(out)
}
#' Retrieve the taxonomic hierarchy for a given taxon ID.
#' 
#' @import XML RCurl plyr
#' 
#' @param x character; IDs as returned by \code{\link[taxize]{get_tsn}} or \code{\link[taxize]{get_uid}}.
#' @param ID character, type of identifier, either 'uid' or 'tsn'.
#' @param ... Other arguments passed to \code{\link[taxize]{getfullhierarchyfromtsn}}.
#' 
#' @return A list of data.frames with the taxonomic classifcation of every supplied taxa.
#' 
#' @note If IDs are supplied directly (not from the \code{get_*} functions) you must 
#' specify the type of ID. There is a timeout of 1/3 seconds between querries to NCBI.
#' 
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}}
#' 
#' @export
#' @examples \dontrun{
#' classification(get_uid(c("Chironomus riparius", "aaa vva")))
#' classification(get_tsn(c("Chironomus riparius", "aaa vva"), "sciname"))
#' 
#' # must specify Identifier, when not used with get_*()
#' classification(315576, ID = "uid")
#' classification(180544, "tsn")
#' }
#' 
#' @examples \donttest{
#' # Fails
#' classification(315576)
#' }
classification <- function(x, ID = NULL, ...){
  UseMethod("classification")
}

#' @S3method classification default
classification.default <- function(x, ID = NULL, ...){
  if(is.null(ID))
    stop("Must specify Identifier!")
  if(ID == 'tsn')
    out <- taxize:::classification.tsn(x)
  if(ID == 'uid')
    out <- taxize:::classification.uid(x)
  return(out)
}

#' @method classification tsn
#' @export
#' @rdname classification
classification.tsn <- function(x, ...) 
{
  fun <- function(x){
    # return NA if NA is supplied
    if(is.na(x)) {
      out <- NA
    } else {
    	out <- getfullhierarchyfromtsn(x, ...)
    	# remove overhang
    	out <- out[1:which(out$tsn == x), ]
    	return(out)
    }
  }
  out <- llply(x, fun)
  return(out)
}


#' @method classification uid
#' @export
#' @rdname classification
classification.uid <- function(x) {
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
  out <- llply(x, fun)
  return(out)
}
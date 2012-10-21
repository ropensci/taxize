#' Get taxonomic hierarchy for a given taxon ID.
#' 
#' @import XML RCurl plyr
#' @param x IDs from \code{get_tsn()} or \code{get_uid()}.
#' @param ID type of identifier, either 'uid' or 'tsn'
#' @param ... Currently not used
#' @return Classification of taxons in a list of data.frames.
#' @note If IDs are supplied directly (not from the get_* functions) 
#' use the methods classification.ncbi() or classification.tsn() directly. 
#' See examples.
#' @export
#' @examples \dontrun{
#' classification(get_uid(c("Chironomus riparius", "aaa vva")))
#' classification(get_tsn(c("Chironomus riparius", "aaa vva"), "sciname"))
#' # Fails
#' classification(315576)
#' # must specify Identifier, when not used with get_*()
#' classification(315576, ID = "uid")
#' classification(180544, "tsn")
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
  out
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
    	getfullhierarchyfromtsn(x)
    }
  }
  out <- llply(x, fun)
  out
}


#' @method classification uid
#' @export
#' @rdname classification
classification.uid <- function(x, ...) {
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
                        UID = xpathSApply(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/TaxId", xmlValue))
    }
    # NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(out)
  }
  out <- llply(x, fun)
  out
}
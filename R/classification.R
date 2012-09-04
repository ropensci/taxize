#' Get taxonomic hierarchy for a given taxon.
#' @import XML RCurl plyr
#' @param x quoted tsn number (taxonomic serial number)
#' @return Classification of the taxon in a data.frame.
#' @export
#' @examples \dontrun{
#' classification(685566)
#' }

classification <- function(x, ...){
  UseMethod("classification")
}

#'@S3method classification default
classification.default <- function(x, ...){
  stop("No default classification defined!\n
       Use classification.tsn() or classification.ncbi()")
}

#'@S3method classification tsn
classification.tsn <- function(x) 
{
  fun <- function(x){
    # return NA if NA is supplied
    if(is.na(x)) {
      out <- NA
    } else {
      doc <- get_itis_xml(searchterm = x, searchtype = "tsnfullhir", by_ = "tsn")
      namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
      nodes <- getNodeSet(doc, "//ax23:rankName", namespaces=namespaces)
      rank <- sapply(nodes, xmlValue)
      nodes <- getNodeSet(doc, "//ax23:taxonName", namespaces=namespaces)
      taxon <- sapply(nodes, xmlValue)
      nodes <- getNodeSet(doc, "//ax23:tsn", namespaces=namespaces)
      tsn <- sapply(nodes, xmlValue) # last one is a repeat
      out <- data.frame(rank, taxon, tsn=tsn[-length(tsn)])
      out
    }
  }
  out <- llply(x, fun)
  out
}

#'@S3method classification ncbi
classification.ncbi <- function(x) {
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
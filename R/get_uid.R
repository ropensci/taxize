#' Get the UID codes from NCBI for species names.
#' 
#' A function to retrieve the UID-Code (Unique Identifier) of a species from NCBI taxonomy browser.
#' 
#' @import plyr RCurl
#' @param sciname scientific name.
#' @return UID for the supplied species names. NA for non-matching names.
#' 
#' @export
#' @author Eduard Szoecs \email{szoe8822@@uni-landau.de}
#' 
#' @examples \dontrun{
#' get_uid(c("Chironomus riparius", "Chaetopteryx"))
#' get_uid(c("Chironomus riparius", "aaa vva"))
#' }
get_uid <- function(sciname){
  fun <- function(sciname) {
    
    sciname <- gsub(" ", "+", sciname)
    searchurl <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=taxonomy&term=", 
                       sciname, sep = "")
    # NCBI limits requests to three per second
    Sys.sleep(0.33)
    xml_result <- xmlParse(getURL(searchurl))    
    id <- xpathSApply(xml_result, "//IdList/Id", xmlValue)
    if (length(id) == 0)
      id <- NA
    id
  }
  out <- laply(sciname, fun)
  class(out) <- "uid"
  out
}
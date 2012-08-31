#' Get taxonomic hierarchy for a given taxon from NCBI taxonomy browser.
#' 
#' @import plyr XML
#' 
#' @param ids Unique identifiers (UID) for the species. \code{get_uid} can be used to retrieve UIDs.
#' @return A list of data.frames with the classification of the taxon.
#' 
#' @export
#' @author Eduard Szöcs \email{szoe8822@@uni-landau.de}
#' 
#' @examples \dontrun{
#' classification_ncbi(get_uid("Chironomus riparius"))
#' classification_ncbi(get_uid(c("Chironomus riparius", "Chaetopteryx")))
#' classification_ncbi(get_uid(c("Chironomus riparius", "aaa vva")))
#' }
classification_ncbi <- function(ids) {
  fun <- function(x){
    baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy"
    ID <- paste("ID=", x, sep = "")
    searchurl <- paste(baseurl, ID, sep = "&")
    tt <- getURL(searchurl)
    ttp <- xmlTreeParse(tt, useInternalNodes = TRUE)
    out <- data.frame(ScientificName = xpathSApply(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/ScientificName", xmlValue), 
                      Rank = xpathSApply(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/Rank", xmlValue), 
                      UID = xpathSApply(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/TaxId", xmlValue))
    # NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(out)
  }
  out <- llply(ids, fun)
  out
}

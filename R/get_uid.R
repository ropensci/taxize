#' Get the UID codes from NCBI for species names.
#' 
#' A function to retrieve the UID-Code (Unique Identifier) of a species from NCBI taxonomy browser.
#' 
#' @import rentrez plyr
#' 
#' @param sciname scientific name.
#' @return UID for the supplied species names. NA for non-matching names.
#' 
#' @export
#' @author Eduard Sz??cs \email{szoe8822@@uni-landau.de}
#' 
#' @examples \dontrun{
#' get_uid(c("Chironomus riparius", "Chaetopteryx"))
#' get_uid(c("Chironomus riparius", "aaa vva"))
#' }
get_uid <- function(sciname){
  fun <- function(sciname) {
    # NCBI limits requests to three per second
    Sys.sleep(0.33)
    id <- entrez_search(db="taxonomy", term=sciname)$ids
    if (length(id) == 0)
      id <- NA
    id
  }
  out <- laply(sciname, fun)
  class(out) <- "uid"
  out
}
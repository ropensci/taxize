#' Get the TSN code for a search term.
#' @import XML RCurl
#' @param searchterm Any common or scientific name.
#' @param searchtype One of 'sciname', 'anymatch', 'comnamebeg', 'comname', 
#'    'comnameend', 'terms', 'itistermscomname', 'itistermssciname', or
#'    'tsnsvernacular', 'tsnfullhir', 'tsnhirdown' .
#' @param by_ One of 'name' (any common or scientific name) or 'tsn' 
#'    (taxonomic serial number).
#' @param curl If using in a loop, call getCurlHandle() first and pass 
#'    the returned value in here (avoids unnecessary footprint)
#' @return A taxonomic serial number (TSN).
#' @export
#' @examples \dontrun{
#' get_tsn("Quercus_douglasii", "sciname", by_="name")
#' }
get_tsn <- 
  
function(searchterm, searchtype, by_, 
      curl=getCurlHandle()) 
{
  base_url <- "http://www.itis.gov/ITISWebService/services/ITISService/"
  skey_ <- "srchKey="
  tkey_ <- "tsn="
  sciname_url <- "searchByScientificName?"
  anymatch_url <- "searchForAnyMatch?"
  comnamebeg_url <- "searchByCommonNameBeginsWith?"
  comname_url <- "searchByCommonName?"
  comnameend_url <- "searchByCommonNameEndsWith?"
  itisterms_url <- "getITISTerms?"
  itistermscomname_url <- "getITISTermsFromCommonName?"
  itistermssciname_url <- "getITISTermsFromScientificName?"
  tsnsvernacular_url <- "getTsnByVernacularLanguage?"
  tsnfullhir_url <- "getFullHierarchyFromTSN?"
  if(searchtype == "sciname") {bykey <- sciname_url} else
  if(searchtype == "anymatch") {bykey <- anymatch_url} else
  if(searchtype == "comnamebeg") {bykey <- comnamebeg_url} else
  if(searchtype == "comname") {bykey <- comname_url} else
  if(searchtype == "comnameend") {bykey <- comnameend_url} else
  if(searchtype == "terms") {bykey <- itisterms_url} else
  if(searchtype == "itistermscomname") {bykey <- itistermscomname_url} else
  if(searchtype == "itistermssciname") {bykey <- itistermssciname_url} else
  if(searchtype == "tsnsvernacular") {bykey <- tsnsvernacular_url} else
  if(searchtype == "tsnfullhir") {bykey <- tsnfullhir_url} else
    end
  if (by_ ==  'name') { searchurl <- paste(base_url, bykey, skey_, searchterm, sep="") } else
    if (by_ == 'tsn' ) { searchurl <- paste(base_url, bykey, tkey_, searchterm, sep="")  } 
      end
  tt <- getURLContent(searchurl, curl=curl)
  page <- xmlParse(tt)
  xmlToList(page)[[1]][[1]]$tsn
}
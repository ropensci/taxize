#' Search individual strings.
#' @import XML RCurl
#' @param searchterm Any common or scientific name.
#' @param searchtype One of 'sciname', 'anymatch', 'comnamebeg', 'comname', 
#'    'comnameend', 'terms', 'itistermscomname', 'itistermssciname', or
#'    'tsnsvernacular', 'tsnfullhir', 'tsnhirdown' .
#' @param by_ One of 'name' (any common or scientific name) or 'tsn' 
#'    (taxonomic serial number).
#' @return xml with taxonomic information.
#' @export
#' @examples \dontrun{
#' xml <- get_itis_xml("Plethodon ")
#' parse_itis(xml)
#' get_itis_xml('202420', 'tsnhirdown', 'tsn')
#' get_itis_xml("36616", "tsnfullhir", "tsn")
#' }
get_itis_xml <- 
  
function(searchterm, 
  searchtype = c("anymatch", "sciname", "comnamebeg", "comname",
  "comnameend", "terms", "itistermscomname", "itistermssciname",
  "tsnsvernacular", "tsnfullhir", 'tsnhirdown'),
  by_ = c("name", "tsn"), 
  parselist = TRUE,
  base_url = "http://www.itis.gov/ITISWebService/services/ITISService/",
  curl = getCurlHandle()) 
{
 
  searchtype <- match.arg(searchtype)
  by_ <- match.arg(by_)
     
  # if spaces exist between search terms %20 replaces the spaces for the search string
  searchterm <- gsub(" ", "%20", searchterm)

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
  tsnhirdown_url <- "getHierarchyDownFromTSN"
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
  if(searchtype == "tsnhirdown") {bykey <- tsnhirdown_url} else
    end
  if (by_ ==  'name') { searchurl <- paste(base_url, bykey, skey_, searchterm, sep="") } else
    if (by_ == 'tsn' ) { searchurl <- paste(base_url, bykey, tkey_, searchterm, sep="")  } 
      end
  tt <- getURLContent(searchurl, curl=curl)
  if(parselist == TRUE) { xmlParse(tt) } else
      { tt }
}

# url <- 'http://www.itis.gov/ITISWebService/services/ITISService/searchByScientificName'
# args <- list(srchKey = 'Plethodon')
# out <- xmlParse(getForm(url, 
#         .params = args,
#         curl = getCurlHandle()))
# parse_itis(out)
# Function to search individual strings
# Args: 
#   searchterm = any common or scientific name, 
#   searchtype = one of 'sciname', 'anymatch', 'comnamebeg', 'comname', 
#     'comnameend', 'terms', 'itistermscomname', 'itistermssciname', or
#     'tsnsvernacular' 
#   by_ = one of 'name' (any common or scientific name) or 'tsn' (taxonomic serial number)
# Output: xml with taxnomic information
# Examples:
#   xml <- get_itis_xml("Plethodon ")
get_itis_xml <- function(searchterm, searchtype = c("anymatch", "sciname", 
                         "comnamebeg", "comname", "comnameend", "terms",
                         "itistermscomname", "itistermssciname",
                         "tsnsvernacular", "tsnfullhir"),
                         by_=c("name", "tsn"), curl=getCurlHandle()) {
 
 searchtype <- match.arg(searchtype)
  by_ <- match.arg(by_)

  searchterm <- gsub(" ", "%20", searchterm)

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
  return(page)
}




# Development for package taxize
# Goal: Search taxonomic information on multiple web data bases
# taxize.R
require(XML); require(stringr); require(RCurl); require(plyr)

# Function to search individual strings
get_itis_xml <- function(searchterm, searchtype, curl=getCurlHandle()) {
  
  base_url <- "http://www.itis.gov/ITISWebService/services/ITISService/"
  skey_ <- "srchKey="
  sciname_url <- "searchByScientificName?"
  anymatch_url <- "searchForAnyMatch?"
  comnamebeg_url <- "searchByCommonNameBeginsWith?"
  comname_url <- "searchByCommonName?"
  comnameend_url <- "searchByCommonNameEndsWith?"
  itisterms_url <- "getITISTerms?"
  itistermscomname_url <- "getITISTermsFromCommonName?"
  itistermssciname_url <- "getITISTermsFromScientificName?"
  tsnsvernacular_url <- "getTsnByVernacularLanguage?"
  if(searchtype == "sciname") {bykey <- sciname_url} else
  if(searchtype == "anymatch") {bykey <- anymatch_url} else
  if(searchtype == "comnamebeg") {bykey <- comnamebeg_url} else
  if(searchtype == "comname") {bykey <- comname_url} else
  if(searchtype == "comnameend") {bykey <- comnameend_url} else
  if(searchtype == "terms") {bykey <- itisterms_url} else
  if(searchtype == "itistermscomname") {bykey <- itistermscomname_url} else
  if(searchtype == "itistermssciname") {bykey <- itistermssciname_url} else
  if(searchtype == "tsnsvernacular") {bykey <- tsnsvernacular_url} else
    end
  searchurl <- paste(base_url, bykey, skey_, searchterm, sep="")
  tt <- getURLContent(searchurl, curl=curl)
  page <- xmlParse(tt)
  return(page)
  
}

# Examples: search by term and search type
itisxml <- get_itis_xml("Helianthus_annuus", "sciname")
itisxml <- get_itis_xml("dolphin", "anymatch")
itisxml <- get_itis_xml("inch", "comnamebeg")
itisxml <- get_itis_xml("ferret-badger", "comname")
itisxml <- get_itis_xml("grizzly%20bear", "comnameend")
itisxml <- get_itis_xml("bear", "terms")
itisxml <- get_itis_xml("buya", "itistermscomname")
itisxml <- get_itis_xml("ursidae", "itistermssciname")
itisxml <- get_itis_xml("french", "tsnsvernacular")

# Function to convert xml to other formats
pagelist <- xmlToList(itisxml)
pagelist[1]

# Function to match names
# terms_: a vector of terms to search
spnames <- c("Oncorhynchus_mykiss","Ailuroedus_buccoides")
terms_ <- spnames
searchtype <- "sciname"
match_itsnames <- function(terms_, searchtype) {
  
  tempxml <- sapply(terms_, function(x) get_itis_xml(x, "sciname") )
  tempxmllist <- lapply(tempxml, xmlToList)
  tsns <- sapply(tempxmllist, )
  
}
match_itsnames(spnames, "sciname")
get_itis_xml("Oncorhynchus mykiss", "sciname")

# E.g.
  # input data set
taxdat <- read.csv()

  #
match_itsnames(spnames, "sciname")









#' Use any API method for the ITIS API.
#' 
#' @import XML RCurl
#' @param query Any common or scientific name (character), or taxonomic serial 
#' 		number (numeric). Can supply a single query or many.
#' @param searchtype Specify one or more of: 
#' "getacceptednamesfromtsn","getanymatchcount","getcommentdetailfromtsn", 
#' "getcommonnamesfromtsn","getcoremetadatafromtsn","getcoveragefromtsn",
#' "getcredibilityratingfromtsn","getcurrencyfromtsn","getdatedatafromtsn",
#' "getexpertsfromtsn","getfullhierarchyfromtsn","getfullrecordfromtsn",
#' "getgeographicdivisionsfromtsn","getglobalspeciescompletenessfromtsn",
#' "gethierarchydownfromtsn","gethierarchyupfromtsn","getitistermsfromcommonname",
#' "getitistermsfromscientificname","getjurisdictionaloriginfromtsn",
#' "getkingdomnamefromtsn","getlsidfromtsn","getothersourcesfromtsn",
#' "getparenttsnfromtsn","getpublicationsfromtsn","getreviewyearfromtsn",
#' "getscientificnamefromtsn","getsynonymnamesfromtsn","gettaxonauthorshipfromtsn",
#' "gettaxonomicranknamefromtsn","gettaxonomicusagefromtsn",
#' "getunacceptabilityreasonfromtsn","searchbycommonname",
#' "searchbycommonnamebeginswith","searchbycommonnameendswith","searchbyscientificname",
#' "searchforanymatch","searchforanymatchpaged".
#' @return A variety of results can be returned depending on the ritis function
#' 		called.  If many queries or functions, or many of both, are provided, 
#' 		results will be returned in a list.
#' @examples \dontrun{
#' # Search by scientific name, can abbreviate
#' itis("Plethodon", "searchbysci") 
#' 
#' # Bet an LSID code from TSN code
#' itis(202420, 'getlsidfromtsn')
#' 
#' # Get the full taxonomic hierarchy for a taxon from the TSN
#' itis(36616, "getfullhierarchyfromtsn")
#' 
#' # Search by scientific name, then use a TSN to get its parent TSN
#' itis("Ursus", "searchbyscientificname") # let's pick one of the TSN's
#' itis(203539, "getparenttsnfromtsn") 
#' itis(203539, "getsynonymnamesfromtsn") # no synonyms in this case
#'
#' # Use multiple queries on one call to the function
#' itis(c(203539, 202420), "getsynonymnamesfromtsn")
#'  
#' # Use multiple ritis functions
#' itis(203539, c("getsynonymnamesfromtsn","getcommonnamesfromtsn"))
#' 
#' # Use multiple ritis functions and multiple queries
#' itis(c(203539, 202420), searchtype=c("getsynonymnamesfromtsn","getcommonnamesfromtsn"))
#' }
#' @export
itis <- function(query, searchtype = NULL) 
{	
	searchtype <- match.arg(searchtype, choices=c(
		"getacceptednamesfromtsn","getanymatchcount","getcommentdetailfromtsn", 
		"getcommonnamesfromtsn","getcoremetadatafromtsn","getcoveragefromtsn",
		"getcredibilityratingfromtsn","getcurrencyfromtsn","getdatedatafromtsn",
		"getexpertsfromtsn","getfullhierarchyfromtsn","getfullrecordfromtsn",
		"getgeographicdivisionsfromtsn","getglobalspeciescompletenessfromtsn",
		"gethierarchydownfromtsn","gethierarchyupfromtsn","getitistermsfromcommonname",
		"getitistermsfromscientificname","getjurisdictionaloriginfromtsn",
		"getkingdomnamefromtsn","getlsidfromtsn","getothersourcesfromtsn",
		"getparenttsnfromtsn","getpublicationsfromtsn","getreviewyearfromtsn",
		"getscientificnamefromtsn","getsynonymnamesfromtsn","gettaxonauthorshipfromtsn",
		"gettaxonomicranknamefromtsn","gettaxonomicusagefromtsn",
		"getunacceptabilityreasonfromtsn","searchbycommonname",
		"searchbycommonnamebeginswith","searchbycommonnameendswith","searchbyscientificname",
		"searchforanymatch","searchforanymatchpaged"
	), several.ok=TRUE)
	
	# if spaces exist between search terms %20 replaces the spaces for the search string
	query <- sapply(query, function(x) gsub(" ", "%20", x))
	
	# do search
	lapply(query, function(x) each(searchtype)(x))
}

getanymatchcount <- function(srchkey = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getAnyMatchCount',
  ..., 
  curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  xmlToList(out)
}

getcommentdetailfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getCommentDetailFromTSN',
  ..., 
  curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:commentDetail", namespaces=namespaces)
  comment <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:commentId", namespaces=namespaces)
  commid <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:commentTimeStamp", namespaces=namespaces)
  commTime <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:commentator", namespaces=namespaces)
  commentator <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:updateDate", namespaces=namespaces)
  updatedate <- sapply(nodes, xmlValue)
  data.frame(comment=comment, commid=commid, commTime=commTime, 
    commentator=commentator, updatedate=updatedate)
}

getcommonnamesfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getCommonNamesFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:commonName", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:language", namespaces=namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(comname=comname, lang=lang, tsn=tsn[-length(tsn)])
}

getcoremetadatafromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getCoreMetadataFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("credRating","rankId","taxonCoverage","taxonCurrency","taxonUsageRating","tsn")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

getcoveragefromtsn <- function(tsn = NA,
   url = 'http://www.itis.gov/ITISWebService/services/ITISService/getCoverageFromTSN',
   ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:rankId", namespaces=namespaces)
  rankid <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:taxonCoverage", namespaces=namespaces)
  taxoncoverage <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(rankid=rankid, taxoncoverage=taxoncoverage, tsn=tsn)
}

getcredibilityratingfromtsn <- function(tsn = NA,
   url = 'http://www.itis.gov/ITISWebService/services/ITISService/getCredibilityRatingFromTSN',
   ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:credRating", namespaces=namespaces)
  credrating <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(credrating=credrating, tsn=tsn)
}

getcredibilityratings <- function(
   url='http://www.itis.gov/ITISWebService/services/ITISService/getCredibilityRatings') 
{
  message(url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax25="http://metadata.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax25:credibilityValues", namespaces=namespaces)
  credibilityValues <- sapply(nodes, xmlValue)
  data.frame(credibilityValues = credibilityValues)
}

getcurrencyfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getCurrencyFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax21="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax21:rankId", namespaces=namespaces)
  rankid <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:taxonCurrency", namespaces=namespaces)
  taxoncurrency <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(rankid=rankid, taxoncurrency=taxoncurrency, tsn=tsn)
}

getdatedatafromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getDateDataFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax21="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax21:initialTimeStamp", namespaces=namespaces)
  initialTimeStamp <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:updateDate", namespaces=namespaces)
  updateDate <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(initialTimeStamp=initialTimeStamp, updateDate=updateDate, tsn=tsn)
}

getdescription <- function(
   url = 'http://www.itis.gov/ITISWebService/services/ITISService/getDescription') 
{
  message(url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax26="http://itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax26:description", namespaces=namespaces)
  sapply(nodes, xmlValue)
}

getexpertsfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getExpertsFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("comment","expert","name","referredTsn","referenceFor","updateDate")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

getfullhierarchyfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getFullHierarchyFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:parentName", namespaces=namespaces)
  parentName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:parentTsn", namespaces=namespaces)
  parentTsn <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:rankName", namespaces=namespaces)
  rankName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:taxonName", namespaces=namespaces)
  taxonName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(parentName=parentName, parentTsn=parentTsn, rankName=rankName,
             taxonName=taxonName, tsn=tsn[-1])
}

getfullrecordfromlsid <- function(lsid = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getFullRecordFromLSID',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(lsid))
    args$lsid <- lsid
  message(paste(url, '?lsid=', lsid, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  xmlParse(tt)
}

getfullrecordfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getFullRecordFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  xmlParse(tt)
}

getgeographicdivisionsfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getGeographicDivisionsFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("geographicValue","updateDate","tsn")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

getgeographicvalues <- function(
   url = 'http://www.itis.gov/ITISWebService/services/ITISService/getGeographicValues') 
{
  message(url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.org/xsd" )
  nodes <- getNodeSet(out, "//ax23:geographicValues", namespaces=namespaces)
  geographicValues <- sapply(nodes, xmlValue)
  data.frame(geographicValues = geographicValues)
}

getglobalspeciescompletenessfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getGlobalSpeciesCompletenessFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("completeness","rankId","tsn")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

gethierarchydownfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getHierarchyDownFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:parentName", namespaces=namespaces)
  parentName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:parentTsn", namespaces=namespaces)
  parentTsn <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:rankName", namespaces=namespaces)
  rankName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:taxonName", namespaces=namespaces)
  taxonName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(parentName=parentName, parentTsn=parentTsn, rankName=rankName,
             taxonName=taxonName, tsn=tsn[-1])
}

gethierarchyupfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getHierarchyUpFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:parentName", namespaces=namespaces)
  parentName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:parentTsn", namespaces=namespaces)
  parentTsn <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:rankName", namespaces=namespaces)
  rankName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:taxonName", namespaces=namespaces)
  taxonName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(parentName=parentName, parentTsn=parentTsn, rankName=rankName,
             taxonName=taxonName, tsn=tsn)
}

getitistermsfromcommonname <- function(srchkey = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getITISTermsFromCommonName',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:commonNames", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:nameUsage", namespaces=namespaces)
  nameusage <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:scientificName", namespaces=namespaces)
  sciname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(comname=comname[-length(comname)], nameusage=nameusage, sciname=sciname, tsn=tsn)
}

getitistermsfromscientificname <- function(srchkey = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getITISTermsFromScientificName',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  message(paste(url, '?srchKey=', srchkey, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:commonNames", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:nameUsage", namespaces=namespaces)
  nameusage <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:scientificName", namespaces=namespaces)
  sciname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(comname=comname, nameusage=nameusage, sciname=sciname, tsn=tsn)
}

getjurisdictionaloriginfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getJurisdictionalOriginFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("jurisdictionValue","origin","updateDate")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

getjurisdictionoriginvalues <- function(
   url = 'http://www.itis.gov/ITISWebService/services/ITISService/getJurisdictionalOriginValues') 
{
  message(url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.org/xsd" )
  nodes <- getNodeSet(out, "//ax23:jurisdiction", namespaces=namespaces)
  jurisdiction <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:origin", namespaces=namespaces)
  origin <- sapply(nodes, xmlValue)
  data.frame(jurisdiction = jurisdiction, origin = origin)
}

getjurisdictionvalues <- function(
   url = 'http://www.itis.gov/ITISWebService/services/ITISService/getJurisdictionValues') 
{
  message(url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.org/xsd" )
  nodes <- getNodeSet(out, "//ax23:jurisdictionValues", namespaces=namespaces)
  jurisdictionValues <- sapply(nodes, xmlValue)
  data.frame(jurisdictionValues = jurisdictionValues)
}

getkingdomnamefromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getKingdomNameFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("kingdomId","kingdomName","tsn")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

getkingdomnames <- function(
   url = 'http://www.itis.gov/ITISWebService/services/ITISService/getKingdomNames') 
{
  message(url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:kingdomId", namespaces=namespaces)
  kingdomId <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:kingdomName", namespaces=namespaces)
  kingdomName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(kingdomId = kingdomId, kingdomName = kingdomName, tsn = tsn)
}

getlastchangedate <- function(
   url = 'http://www.itis.gov/ITISWebService/services/ITISService/getLastChangeDate') 
{
  message(url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:updateDate", namespaces=namespaces)
  sapply(nodes, xmlValue)
}

getlsidfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getLSIDFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  xmlToList(xmlParse(tt))[[1]]
}

getothersourcesfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getOtherSourcesFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("acquisitionDate","name","referredTsn","source",
                "sourceType","updateDate","version")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

getparenttsnfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getParentTSNFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("parentTsn","tsn")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

getpublicationsfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getPublicationsFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("actualPubDate","isbn","issn","listedPubDate","pages",
                "pubComment","pubName","pubPlace","publisher","referenceAuthor",
                "name","refLanguage","referredTsn","title","updateDate")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

getranknames <- function(
   url = 'http://www.itis.gov/ITISWebService/services/ITISService/getRankNames') 
{
  message(url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax25="http://metadata.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax25:kingdomName", namespaces=namespaces)
  kingdomName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax25:rankId", namespaces=namespaces)
  rankId <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax25:rankName", namespaces=namespaces)
  rankName <- sapply(nodes, xmlValue)
  data.frame(kingdomName = kingdomName, rankId = rankId, rankName = rankName)
}

getrecordfromlsid <- function(lsid = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getRecordFromLSID',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(lsid))
    args$lsid <- lsid
  message(paste(url, '?lsid=', lsid, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("authorship","genusPart","infragenericEpithet",
                "infraspecificEpithet","lsid","nameComplete","nomenclaturalCode",
                "rank","rankString","specificEpithet","uninomial","tsn")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

getreviewyearfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getReviewYearFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("rankId","reviewYear","tsn")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

getscientificnamefromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getScientificNameFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("combinedName","unitInd1","unitInd3","unitName1","unitName2",
                "unitName3","tsn")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

getsynonymnamesfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getSynonymNamesFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:name", namespaces=namespaces)
  if( length(sapply(nodes, xmlValue)) == 0){ name <- list("nomatch") } else
    { name <- sapply(nodes, xmlValue) }
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  if( length(sapply(nodes, xmlValue)) == 1){ tsn <- sapply(nodes, xmlValue) } else
    { tsn <- sapply(nodes, xmlValue) 
      tsn <- tsn[-length(tsn)]
    } 
  data.frame(name=name[[1]], tsn=tsn)
}

gettaxonauthorshipfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getTaxonAuthorshipFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("authorship","updateDate","tsn")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

gettaxonomicranknamefromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getTaxonomicRankNameFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
     ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("kingdomId","kingdomName","rankId","rankName","tsn")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

gettaxonomicusagefromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getTaxonomicUsageFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("taxonUsageRating","tsn")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

gettsnbyvernacularlanguage <- function(language = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getTsnByVernacularLanguage',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(language))
    args$language <- language
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:commonName", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:language", namespaces=namespaces)
  language <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(comname=comname, language=language, tsn=tsn)
}

gettsnfromlsid <- function(lsid = NA,
   url = 'http://www.itis.gov/ITISWebService/services/ITISService/getTSNFromLSID',
   ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(lsid))
    args$lsid <- lsid
  message(paste(url, '?lsid=', lsid, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  if( !is.na( suppressWarnings(as.numeric(xmlToList(out)[[1]])) ) )
    { suppressWarnings( as.numeric(xmlToList(out)[[1]]) )} else
      {"invalid TSN"}
}


getunacceptabilityreasonfromtsn <- function(tsn = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/getUnacceptabilityReasonFromTSN',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  toget <- list("tsn","unacceptReason")
  xpathfunc <- function(x) {    
    sapply(getNodeSet(out, paste("//ax23:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

getvernacularlanguages <- function(
   url = 'http://www.itis.gov/ITISWebService/services/ITISService/getVernacularLanguages') 
{
  message(url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:languageNames", namespaces=namespaces)
  languageNames <- sapply(nodes, xmlValue)
  data.frame(languageNames = languageNames)
}

searchbycommonname <- function(srchkey = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/searchByCommonName',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:commonName", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:language", namespaces=namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(comname=comname, lang=lang, tsn=tsn[-1])
}

searchbycommonnamebeginswith <- function(srchkey = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/searchByCommonNameBeginsWith',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:commonName", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:language", namespaces=namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue) # last one is a repeat
  nodes <- getNodeSet(out, "//ax23:sciName", namespaces=namespaces)
  data.frame(comname=comname, lang=lang, tsn=tsn[-length(tsn)])
}

searchbycommonnameendswith <- function(srchkey = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/searchByCommonNameEndsWith',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:commonName", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:language", namespaces=namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue) # last one is a repeat
  data.frame(comname=comname, lang=lang, tsn=tsn[!nchar(tsn) == 0])
}

searchbyscientificname <- function(srchkey = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/searchByScientificName',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:combinedName", namespaces=namespaces)
  combinedname <- sapply(nodes, xmlValue)
#   combinedname <- sapply(combinedname, gsub, pattern = " ", replacement = "")
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue) # last one is a repeat
  data.frame(combinedname=combinedname, tsn=tsn)
}

searchforanymatch <- function(srchkey = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/searchForAnyMatch',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:commonName", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:language", namespaces=namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue) # last one is a repeat
  nodes <- getNodeSet(out, "//ax23:sciName", namespaces=namespaces)
  sciName <- sapply(nodes, xmlValue)
  list(comname=comname, lang=lang, tsn=tsn[-length(tsn)], sciName=sciName)
}

searchforanymatchpaged <- function(srchkey = NA, pagesize = NA, pagenum = NA, ascend = NA,
  url = 'http://www.itis.gov/ITISWebService/services/ITISService/searchForAnyMatchPaged',
  ..., curl = getCurlHandle() ) 
{
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  if(!is.na(pagesize))
    args$pageSize <- pagesize
  if(!is.na(pagenum))
    args$pageNum <- pagenum
  if(!is.na(ascend))
    args$ascend <- ascend
  tt <- getForm(url,
    .params = args,
    ...,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax23:commonName", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:language", namespaces=namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue) # last one is a repeat
  nodes <- getNodeSet(out, "//ax23:sciName", namespaces=namespaces)
  sciName <- sapply(nodes, xmlValue)
  list(comname=comname, lang=lang, tsn=tsn[-length(tsn)], sciName=sciName)
}

getacceptednamesfromtsn <- function(tsn = NA, supmess = TRUE,
  url = "http://www.itis.gov/ITISWebService/services/ITISService/getAcceptedNamesFromTSN",
  ...,
  curl = getCurlHandle())
{ 
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  tt <- getForm(url, 
    .params = args, 
    ...,
    curl = curl)
  tt_ <- xmlParse(tt)
  temp <- xmlToList(tt_)
  if(supmess == FALSE) {
    if(length(temp$return$acceptedNames) == 1) 
      { message("Good name!")
        temp$return$tsn
        } else
      { message("Bad name!")
        c(submittedTsn = temp$return$tsn, temp$return$acceptedNames[1:2])
        }
  } else
      { if(length(temp$return$acceptedNames) == 1) { temp$return$tsn } else
          { c(submittedTsn = temp$return$tsn, temp$return$acceptedNames[1:2]) } }
}
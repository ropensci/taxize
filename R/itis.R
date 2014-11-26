itbase <- function() 'http://www.itis.gov/ITISWebService/services/ITISService/'

itis_GET <- function(endpt, args, ...){
  tt <- GET(paste0(itbase(), endpt), query = args, ...)
  xmlParse(content(tt, "text"))
}

#' Get accepted names from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @param verbose Verbosity or not (default TRUE)
#' @examples \donttest{
#' # TSN accepted - good name
#' library('httr')
#' getacceptednamesfromtsn('208527', config=timeout(1))
#'
#' # TSN not accepted - input TSN is old name
#' getacceptednamesfromtsn('504239', config=timeout(1))
#'
#' # TSN not accepted - input TSN is old name
#' getacceptednamesfromtsn('504239', config=timeout(3))
#' }
#' @export
#' @keywords internal
getacceptednamesfromtsn <- function(x, ...)
{
  tt_ <- itis_GET("getAcceptedNamesFromTSN", list(tsn = x), ...)
	temp <- xmlToList(tt_)
	if(length(temp$return$acceptedNames) == 1) {
    temp$return$tsn
  } else {
		c(submittedTsn = temp$return$tsn, temp$return$acceptedNames[1:2])
	}
}

#' Get any match count.
#'
#' @import XML RCurl
#' @param x text or taxonomic serial number (TSN) (character or numeric)
#' @param ... optional additional curl options (debugging tools mostly)
#' @return An integer containing the number of matches the search will return.
#' @examples \donttest{
#' library('httr')
#' getanymatchcount(202385, config=timeout(3))
#' getanymatchcount("dolphin", config=timeout(3))
#' }
#' @export
#' @keywords internal
getanymatchcount <- function(x, ...)
{
	out <- itis_GET("getAnyMatchCount", list(srchKey = x), ...)
  as.numeric(xmlToList(out)$return)
}

#' Get comment detail from TSN
#'
#' @import XML RCurl
#' @param x TSN for a taxonomic group (numeric)
#' @param ... optional additional curl options (debugging tools mostly)
#' @return A data.frame with results.
#' @examples \donttest{
#' getcommentdetailfromtsn(180543, config=timeout(1))
#' }
#' @export
#' @keywords internal
getcommentdetailfromtsn <- function(x, ...)
{
	out <- itis_GET("getCommentDetailFromTSN", list(tsn = x), ...)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:commentDetail", namespaces=namespaces)
  comment <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:commentId", namespaces=namespaces)
  commid <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:commentTimeStamp", namespaces=namespaces)
  commTime <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:commentator", namespaces=namespaces)
  commentator <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:updateDate", namespaces=namespaces)
  updatedate <- sapply(nodes, xmlValue)
  data.frame(comment=comment, commid=commid, commTime=commTime,
    commentator=commentator, updatedate=updatedate)
}

#' Get common names from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getcommonnamesfromtsn(183833, config=timeout(1))
#' }
#' @export
#' @keywords internal
getcommonnamesfromtsn <- function(x, ...)
{
	out <- itis_GET("getCommonNamesFromTSN", list(tsn = x), ...)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:commonName", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:language", namespaces=namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(comname=comname, lang=lang, tsn=tsn[-length(tsn)])
}


#' Get core metadata from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getcoremetadatafromtsn(28727, config=timeout(3))  # coverage and currrency data
#' getcoremetadatafromtsn(183671, config=timeout(4))  # no coverage or currrency data
#' }
#' @export
#' @keywords internal
getcoremetadatafromtsn <- function(x, ...)
{
	out <- itis_GET("getCoreMetadataFromTSN", list(tsn = x), ...)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("credRating","rankId","taxonCoverage","taxonCurrency","taxonUsageRating","tsn")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  setNames(do.call(cbind, lapply(toget, as.data.frame(xpathfunc))), toget)
}

#' Get coverge from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getcoveragefromtsn(28727, config=timeout(4))  # coverage data
#' getcoveragefromtsn(526852, config=timeout(4))  # no coverage data
#' }
#' @export
#' @keywords internal
getcoveragefromtsn <- function(x, ...)
{
	out <- itis_GET("getCoverageFromTSN", list(tsn = x), ...)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:rankId", namespaces=namespaces)
  rankid <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:taxonCoverage", namespaces=namespaces)
  taxoncoverage <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(rankid=rankid, taxoncoverage=taxoncoverage, tsn=tsn, stringsAsFactors = FALSE)
}

#' Get credibility rating from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getcredibilityratingfromtsn(526852, config=timeout(4))
#' }
#' @export
#' @keywords internal
getcredibilityratingfromtsn <- function(x, ...)
{
	out <- itis_GET("getCredibilityRatingFromTSN", list(tsn = x), ...)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:credRating", namespaces=namespaces)
  credrating <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(credrating=credrating, tsn=tsn, stringsAsFactors = FALSE)
}

#' Get possible credibility ratings
#'
#' @import RCurl XML
#' @examples \donttest{
#' getcredibilityratings(config=timeout(3))
#' }
#' @export
#' @keywords internal
getcredibilityratings <- function(...)
{
	out <- itis_GET("getCredibilityRatings", list(), ...)
  namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax23:credibilityValues", namespaces=namespaces)
  credibilityValues <- sapply(nodes, xmlValue)
  data.frame(credibilityValues = credibilityValues, stringsAsFactors = FALSE)
}

#' Get currency from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getcurrencyfromtsn(28727, config=timeout(3)) # currency data
#' getcurrencyfromtsn(x=526852, config=timeout(3)) # no currency dat
#' }
#' @export
#' @keywords internal
getcurrencyfromtsn <- function(x, ...)
{
	out <- itis_GET("getCurrencyFromTSN", list(tsn = x), ...)
  namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax21:rankId", namespaces=namespaces)
  rankid <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:taxonCurrency", namespaces=namespaces)
  taxoncurrency <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(rankid=rankid, taxoncurrency=taxoncurrency, tsn=tsn, stringsAsFactors = FALSE)
}

#' Get date data from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getdatedatafromtsn(x=180543, config=timeout(3))
#' }
#' @export
#' @keywords internal
getdatedatafromtsn <- function(x, ...)
{
	out <- itis_GET("getDateDataFromTSN", list(tsn = x), ...)
  namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax21:initialTimeStamp", namespaces=namespaces)
  initialTimeStamp <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:updateDate", namespaces=namespaces)
  updateDate <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(initialTimeStamp=initialTimeStamp, updateDate=updateDate, tsn=tsn, stringsAsFactors = FALSE)
}

#' Get description of the ITIS service
#'
#' @examples \donttest{
#' getdescription(config=timeout(1))
#' }
#' @export
#' @keywords internal
getdescription <- function(...){
	getNodeSet(itis_GET("getDescription", list(), ...), "//ns:return", fun=xmlValue)[[1]]
}

#' Get expert information for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getexpertsfromtsn(180544, config=timeout(3))
#' }
#' @export
#' @keywords internal
getexpertsfromtsn <- function(x, ...)
{
	out <- itis_GET("getExpertsFromTSN", list(tsn = x), ...)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("comment","expert","name","referredTsn","referenceFor","updateDate")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  setNames(do.call(cbind, lapply(toget, as.data.frame(xpathfunc))), toget)
}

#' Get full hierarchy from tsn
#'
#' @param tsn TSN for a taxonomic group (numeric)
#' @param ... optional additional curl options (debugging tools mostly)
#' @examples \donttest{
#' getfullhierarchyfromtsn(37906, config=timeout(3))
#' getfullhierarchyfromtsn(100800, config=timeout(3))
#' }
#' @export
#' @keywords internal

getfullhierarchyfromtsn <- function(x, ...)
{
	out <- itis_GET("getFullHierarchyFromTSN", list(tsn = x), ...)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:parentName", namespaces=namespaces)
  parentName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:parentTsn", namespaces=namespaces)
  parentTsn <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:rankName", namespaces=namespaces)
  rankName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:taxonName", namespaces=namespaces)
  taxonName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
	data.frame(parentName=parentName, parentTsn=parentTsn,
	           rankName=rankName[-length(rankName)],
	           taxonName=taxonName, tsn=tsn[-1], stringsAsFactors=FALSE)
}

#' Returns the full ITIS record for the TSN in the LSID, found by comparing the
#' 		TSN in the search key to the TSN field. Returns an empty result set if
#'   	there is no match or the TSN is invalid.
#'
#' @param x lsid for a taxonomic group (character)
#' @param ... optional additional curl options (debugging tools mostly)
#' @examples \donttest{
#' getfullrecordfromlsid("urn:lsid:itis.gov:itis_tsn:180543", config=timeout(3))
#' getfullrecordfromlsid("urn:lsid:itis.gov:itis_tsn:180543", config=timeout(3))
#' }
#' @export
#' @keywords internal
getfullrecordfromlsid <- function(x, ...)
{
	out <- itis_GET("getFullRecordFromLSID", list(lsid = x), ...)
	namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd")
	toget <- c("acceptedNameList","commentList","commonNameList","completenessRating",
	           "coreMetadata","credibilityRating","currencyRating","dateData","expertList",
	           "geographicDivisionList","hierarchyUp","jurisdictionalOriginList",
	           "kingdom","otherSourceList","parentTSN","publicationList","scientificName",
	           "synonymList","taxRank","taxonAuthor","unacceptReason","usage")
	parsedat <- function(x){
	  tmp <- getNodeSet(out, sprintf("//ax21:%s",x), namespaces=namespaces, xmlToList)[[1]]
	  tmp[!names(tmp) %in% ".attrs"]
	}
	temp <- lapply(toget, parsedat)
	names(temp) <- toget
	temp
}

#' Get full record from TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @param verbose Verbosity or not (default TRUE)
#' @examples \donttest{
#' getfullrecordfromtsn(504239, config=timeout(3))
#' getfullrecordfromtsn(202385, config=timeout(3))
#' getfullrecordfromtsn(183833, config=timeout(3))
#' getfullrecordfromtsn(183833, config=timeout(3))
#' }
#' @export
#' @keywords internal
getfullrecordfromtsn <- function(x, ...)
{
	out <- itis_GET("getFullRecordFromTSN", list(tsn = x), ...)
	namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd")
  toget <- c("acceptedNameList","commentList","commonNameList","completenessRating",
             "coreMetadata","credibilityRating","currencyRating","dateData","expertList",
             "geographicDivisionList","hierarchyUp","jurisdictionalOriginList",
             "kingdom","otherSourceList","parentTSN","publicationList","scientificName",
             "synonymList","taxRank","taxonAuthor","unacceptReason","usage")
	parsedat <- function(x){
	  tmp <- getNodeSet(out, sprintf("//ax21:%s",x), namespaces=namespaces, xmlToList)[[1]]
    tmp[!names(tmp) %in% ".attrs"]
	}
  setNames(lapply(toget, parsedat), toget)
}

#' Get geographic divisions from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getgeographicdivisionsfromtsn(180543, config=timeout(3))
#' }
#' @export
#' @keywords internal
getgeographicdivisionsfromtsn <- function(x, ...)
{
	out <- itis_GET("getGeographicDivisionsFromTSN", list(tsn = x), ...)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("geographicValue","updateDate","tsn")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  setNames(do.call(cbind, lapply(toget, as.data.frame(xpathfunc))), toget)
}

#' Get all possible geographic values
#'
#' @import RCurl XML
#' @examples \donttest{
#' getgeographicvalues(config=timeout(3))
#' }
#' @export
#' @keywords internal
getgeographicvalues <- function(...)
{
	out <- itis_GET("getGeographicValues", list(), ...)
  namespaces <- c(ax21="http://metadata.itis_service.itis.usgs.gov/xsd" )
  nodes <- getNodeSet(out, "//ax21:geographicValues", namespaces=namespaces)
  geographicValues <- sapply(nodes, xmlValue)
  data.frame(geographicValues = geographicValues, stringsAsFactors=FALSE)
}

#' Get global species completeness from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getglobalspeciescompletenessfromtsn(180541, config=timeout(3))
#' }
#' @export
#' @keywords internal
getglobalspeciescompletenessfromtsn <- function(x, ...)
{
	out <- itis_GET("getGlobalSpeciesCompletenessFromTSN", list(tsn = x), ...)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("completeness","rankId","tsn")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  setNames(do.call(cbind, lapply(toget, as.data.frame(xpathfunc))), toget)
}

#' Get hierarchy down from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' gethierarchydownfromtsn(161030, config=timeout(3))
#' }
#' @export
#' @keywords internal
gethierarchydownfromtsn <- function(x, ...)
{
	out <- itis_GET("getHierarchyDownFromTSN", list(tsn = x), ...)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:hierarchyList/ax21:parentName", namespaces=namespaces)
  parentName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:hierarchyList/ax21:parentTsn", namespaces=namespaces)
  parentTsn <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:hierarchyList/ax21:rankName", namespaces=namespaces)
  rankName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:hierarchyList/ax21:taxonName", namespaces=namespaces)
  taxonName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:hierarchyList/ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(parentName=parentName, parentTsn=parentTsn, rankName=rankName,
             taxonName=taxonName, tsn=tsn, stringsAsFactors = FALSE)
}

#' Get hierarchy up from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' gethierarchyupfromtsn(36485, config=timeout(3))
#' }
#' @export
#' @keywords internal
gethierarchyupfromtsn <- function(x, ...)
{
	out <- itis_GET("getHierarchyUpFromTSN", list(tsn = x), ...)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:parentName", namespaces=namespaces)
  parentName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:parentTsn", namespaces=namespaces)
  parentTsn <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:rankName", namespaces=namespaces)
  rankName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:taxonName", namespaces=namespaces)
  taxonName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(parentName=parentName, parentTsn=parentTsn, rankName=rankName,
             taxonName=taxonName, tsn=tsn, stringsAsFactors = FALSE)
}

#' Get itis terms from common names
#'
#' @inheritParams getanymatchcount
#' @examples \donttest{
#' getitistermsfromcommonname("buya")
#'
#' library('httr')
#' getitistermsfromcommonname("buya", config=timeout(1))
#' }
#' @export
#' @keywords internal
getitistermsfromcommonname <- function(x, ...)
{
  out <- itis_GET("getITISTermsFromCommonName", list(srchKey = x), ...)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
	gg <- getNodeSet(out, "//ax21:itisTerms", namespaces=namespaces, xmlToList)
	tmp <- do.call(rbind.fill, lapply(gg, function(x) data.frame(x,stringsAsFactors=FALSE)))
  names(tmp) <- tolower(names(tmp))
	row.names(tmp) <- NULL
	if(nrow(tmp)==1 && names(tmp)=="x"){
	  NA
	} else {
	  tmp$commonnames <- gsub("true", NA, as.character(tmp$commonnames))
	  tmp$.attrs <- as.character(tmp$.attrs)
    tmp
	}
}

#' Get itis terms from common names
#'
#' @inheritParams getanymatchcount
#' @examples \donttest{
#' getitisterms("bear")
#' }
#' @export
#' @keywords internal
getitisterms <- function(x, ...)
{
  out <- itis_GET("getITISTerms", list(srchKey = x), ...)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  gg <- getNodeSet(out, "//ax21:itisTerms", namespaces=namespaces, xmlToList)
  tmp <- do.call(rbind.fill, lapply(gg, function(x) data.frame(x,stringsAsFactors=FALSE)))
  names(tmp) <- tolower(names(tmp))
  row.names(tmp) <- NULL
  if(nrow(tmp)==1 && names(tmp)=="x"){
    NA
  } else {
    tmp$commonnames <- gsub("true", NA, as.character(tmp$commonnames))
    tmp$.attrs <- as.character(tmp$.attrs)
    tmp
  }
}

#' Get itis terms from scientific names
#'
#' @inheritParams getanymatchcount
#' @examples \donttest{
#' getitistermsfromscientificname("ursidae")
#' getitistermsfromscientificname("Ursus")
#' }
#' @export
#' @keywords internal
getitistermsfromscientificname <- function(x, ...)
{
  out <- itis_GET("getITISTermsFromScientificName", list(srchKey = x), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  gg <- getNodeSet(out, "//ax21:itisTerms", namespaces = namespaces,
                   xmlToList)
  tmp <- do.call(rbind.fill, lapply(gg, function(x) data.frame(x,
                                                               stringsAsFactors = FALSE)))
  names(tmp) <- tolower(names(tmp))
  row.names(tmp) <- NULL
  if (nrow(tmp) == 1 && names(tmp) == "x") {
    NA
  }
  else {
    tmp$commonnames <- gsub("true", NA, as.character(tmp$commonnames))
    tmp$.attrs <- as.character(tmp$.attrs)
    tmp
  }
}

#' Get jurisdictional origin from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @param verbose Verbosity or not (default TRUE)
#' @examples \donttest{
#' getjurisdictionaloriginfromtsn(tsn = 2180543)
#' }
#' @export
#' @keywords internal
getjurisdictionaloriginfromtsn <- function(tsn = NA, curlopts=list(), curl = getCurlHandle(),
                                           verbose=TRUE)
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getJurisdictionalOriginFromTSN'
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  mssg(verbose, paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("jurisdictionValue","origin","updateDate")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  if(nrow(df) == 0){
    data.frame(jurisdictionvalue=NA,origin=NA,updatedate=NA)
  } else
  {
    names(df) <- tolower(toget)
    df
  }
}

#' Get jurisdiction origin values
#'
#' @import RCurl XML
#' @examples \donttest{
#' getjurisdictionoriginvalues()
#' }
#' @export
#' @keywords internal
getjurisdictionoriginvalues <- function(verbose=TRUE)
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getJurisdictionalOriginValues'
	mssg(verbose, url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax23:jurisdiction", namespaces=namespaces)
  jurisdiction <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:origin", namespaces=namespaces)
  origin <- sapply(nodes, xmlValue)
  data.frame(jurisdiction = jurisdiction, origin = origin)
}

#' Get possible jurisdiction values
#'
#' @import RCurl XML
#' @examples \donttest{
#' getjurisdictionvalues()
#' }
#' @export
#' @keywords internal
getjurisdictionvalues <- function(verbose=TRUE)
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getJurisdictionValues'
	mssg(verbose, url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax23:jurisdictionValues", namespaces=namespaces)
  jurisdictionValues <- sapply(nodes, xmlValue)
  data.frame(jurisdictionValues = jurisdictionValues)
}

#' Get kingdom names from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @param verbose Verbosity or not (default TRUE)
#' @examples \donttest{
#' getkingdomnamefromtsn(tsn = 202385)
#' }
#' @export
#' @keywords internal
getkingdomnamefromtsn <- function(tsn = NA, curlopts=list(), curl = getCurlHandle(), verbose=TRUE)
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getKingdomNameFromTSN'
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  mssg(verbose, paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("kingdomId","kingdomName","tsn")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

#' Get all possible kingdom names
#'
#' @import RCurl XML
#' @examples \donttest{
#' getkingdomnames()
#' }
#' @export
#' @keywords internal
getkingdomnames <- function()
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getKingdomNames'
  message(url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax21="http://metadata.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax21:kingdomId", namespaces=namespaces)
  kingdomId <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:kingdomName", namespaces=namespaces)
  kingdomName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(kingdomId = kingdomId, kingdomName = kingdomName, tsn = tsn)
}

#' Provides the date the ITIS database was last updated.
#'
#' @import RCurl XML
#' @examples \donttest{
#' getlastchangedate()
#' }
#' @export
#' @keywords internal
getlastchangedate <- function()
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getLastChangeDate'
  message(url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax21="http://metadata.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax21:updateDate", namespaces=namespaces)
  sapply(nodes, xmlValue)
}

#' Gets the unique LSID for the TSN, or an empty result if there is no match.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getlsidfromtsn(tsn = 155166)
#' }
#' @export
#' @keywords internal
getlsidfromtsn <- function(tsn = NA, curlopts=list(), curl = getCurlHandle() )
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getLSIDFromTSN'
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
    .opts=curlopts,
    curl = curl)
  xmlToList(xmlParse(tt))[[1]]
}

#' Returns a list of the other sources used for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getothersourcesfromtsn(tsn = 182662)
#' }
#' @export
#' @keywords internal
getothersourcesfromtsn <- function(tsn = NA, curlopts=list(), curl = getCurlHandle() )
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getOtherSourcesFromTSN'
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("acquisitionDate","name","referredTsn","source",
                "sourceType","updateDate","version")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

#' Returns the parent TSN for the entered TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getparenttsnfromtsn(tsn = 202385)
#' }
#' @export
#' @keywords internal
getparenttsnfromtsn <- function(tsn = NA, curlopts=list(), curl = getCurlHandle() )
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getParentTSNFromTSN'
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("parentTsn","tsn")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

#' Returns a list of the pulications used for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @param verbose Verbosity or not (default TRUE)
#' @examples \donttest{
#' getpublicationsfromtsn(tsn = 70340)
#' }
#' @export
#' @keywords internal
getpublicationsfromtsn <- function(tsn = NA, curlopts=list(), curl = getCurlHandle(), verbose=TRUE)
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getPublicationsFromTSN'
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  mssg(verbose, paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("actualPubDate","isbn","issn","listedPubDate","pages",
                "pubComment","pubName","pubPlace","publisher","referenceAuthor",
                "name","refLanguage","referredTsn","title","updateDate")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  if(NROW(df) > 0) names(df) <- tolower(toget)
  df
}

#' Provides a list of all the unique rank names contained in the database and
#'  their kingdom and rank ID values.
#'
#' @import RCurl XML
#' @examples \donttest{
#' getranknames()
#' }
#' @export
#' @keywords internal
getranknames <- function()
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getRankNames'
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax23="http://metadata.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax23:kingdomName", namespaces=namespaces)
  kingdomName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:rankId", namespaces=namespaces)
  rankId <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax23:rankName", namespaces=namespaces)
  rankName <- sapply(nodes, xmlValue)
  data.frame(kingdomName = kingdomName, rankId = rankId, rankName = rankName)
}

#' Gets the partial ITIS record for the TSN in the LSID, found by comparing the
#'  TSN in the search key to the TSN field. Returns an empty result set if
#'  there is no match or the TSN is invalid.
#'
#' @import RCurl XML
#' @param lsid lsid for a taxonomic group (numeric)
#' @param curlopts optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @param verbose (logical) Print messages or not. Default to TRUE.
#' @examples \donttest{
#' getrecordfromlsid(lsid = "urn:lsid:itis.gov:itis_tsn:180543")
#' }
#' @export
#' @keywords internal
getrecordfromlsid <- function(lsid = NA, curlopts=list(), verbose=TRUE)
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getRecordFromLSID'
  args <- list()
  if(!is.na(lsid))
    args$lsid <- lsid
	mssg(verbose, paste(url, '?lsid=', lsid, sep=''))
  tt <- GET(url, query = args, curlopts)
  stop_for_status(tt)
  res <- content(tt, as = "text")
  out <- xmlParse(res)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("authorship","genusPart","infragenericEpithet",
                "infraspecificEpithet","lsid","nameComplete","nomenclaturalCode",
                "rank","rankString","specificEpithet","uninomial","tsn")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- tolower(toget)
  df
}

#' Returns the review year for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getreviewyearfromtsn(tsn = 180541)
#' }
#' @export
#' @keywords internal
getreviewyearfromtsn <- function(tsn = NA, curlopts=list(), curl = getCurlHandle() )
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getReviewYearFromTSN'
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("rankId","reviewYear","tsn")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

#' Returns the scientific name for the TSN. Also returns the component parts
#'    (names and indicators) of the scientific name.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getscientificnamefromtsn(tsn = 531894)
#' }
#' @export
#' @keywords internal
getscientificnamefromtsn <- function(tsn = NA, curlopts=list(), curl = getCurlHandle() )
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getScientificNameFromTSN'
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  # message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("combinedName","unitInd1","unitInd3","unitName1","unitName2",
                "unitName3","tsn")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

#' Returns a list of the synonyms (if any) for the TSN.
#'
#' @param tsn TSN for a taxonomic group (numeric)
#' @param curlopts optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @param verbose (logical) Print messages or not. Default to TRUE.
#' @examples \donttest{
#' getsynonymnamesfromtsn(tsn = 183671) # tsn not accepted
#' getsynonymnamesfromtsn(tsn = 526852) # tsn accepted
#' }
#' @export
#' @keywords internal
getsynonymnamesfromtsn <- function(tsn = NA, curlopts=list(), curl = getCurlHandle(), verbose=TRUE)
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getSynonymNamesFromTSN'
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
	mssg(verbose, paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,.params = args,.opts=curlopts,curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax21:sciName", namespaces=namespaces)
  if( length(sapply(nodes, xmlValue)) == 0){ name <- list("nomatch") } else
    { name <- sapply(nodes, xmlValue) }
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  if( length(sapply(nodes, xmlValue)) == 1){ tsn <- sapply(nodes, xmlValue) } else
    {
      tsn <- sapply(nodes, xmlValue)
      tsn <- tsn[-1]
    }
  data.frame(name=name, tsn=tsn)
}

#' Returns the author information for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' gettaxonauthorshipfromtsn(tsn = 183671)
#' }
#' @export
#' @keywords internal
gettaxonauthorshipfromtsn <- function(tsn = NA, curlopts=list(), curl = getCurlHandle() )
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getTaxonAuthorshipFromTSN'
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("authorship","updateDate","tsn")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

#' Returns the kingdom and rank information for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @param verbose Verbosity or not (default TRUE)
#' @examples \donttest{
#' gettaxonomicranknamefromtsn(tsn = 202385)
#' }
#' @export
#' @keywords internal
gettaxonomicranknamefromtsn <- function(tsn = NA, curlopts=list(), curl = getCurlHandle(), verbose = TRUE)
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getTaxonomicRankNameFromTSN'
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  mssg(verbose, paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url,
    .params = args,
     .opts=curlopts,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("kingdomId","kingdomName","rankId","rankName","tsn")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

#' Returns the usage information for the TSN.
#'
#' @param tsn TSN for a taxonomic group (numeric)
#' @param curlopts optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @param verbose Verbosity or not (default TRUE)
#' @examples \donttest{
#' gettaxonomicusagefromtsn(tsn = 526852)
#' }
#' @export
#' @keywords internal
gettaxonomicusagefromtsn <- function(tsn = NA, curlopts=list(), curl = getCurlHandle(), verbose = TRUE)
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getTaxonomicUsageFromTSN'
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  mssg(verbose, paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("taxonUsageRating","tsn")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

#' Retrieve accepted TSN (with accepted name)
#'
#' @import RCurl XML
#' @param language A string containing the language. This is a language string,
#'    not the international language code (character)
#' @param curlopts optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @examples \donttest{
#' gettsnbyvernacularlanguage("french")
#' }
#' @export
#' @keywords internal
gettsnbyvernacularlanguage <- function(language = NA, curlopts=list(),
                                       curl = getCurlHandle() )
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getTsnByVernacularLanguage'
  args <- list()
  if(!is.na(language))
    args$language <- language
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:commonName", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:language", namespaces=namespaces)
  language <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(comname=comname, language=language, tsn=tsn)
}

#' Gets the TSN corresponding to the LSID, or an empty result if there is no match.
#'
#' @param lsid lsid for a taxonomic group (numeric)
#' @param curlopts optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @param verbose (logical) Print messages or not. Default to TRUE.
#' @examples \donttest{
#' gettsnfromlsid(lsid = "urn:lsid:itis.gov:itis_tsn:28726")
#' gettsnfromlsid(lsid = "urn:lsid:itis.gov:itis_tsn:0")
#' }
#' @export
#' @keywords internal
gettsnfromlsid <- function(lsid = NA, curlopts=list(), curl = getCurlHandle(), verbose=TRUE)
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getTSNFromLSID'
  args <- list()
  if(!is.na(lsid))
    args$lsid <- lsid
	mssg(verbose, paste(url, '?lsid=', lsid, sep=''))
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  if( !is.na( suppressWarnings(as.numeric(xmlToList(out)[[1]])) ) )
    { suppressWarnings( as.numeric(xmlToList(out)[[1]]) )} else
      {"invalid TSN"}
}

#' Returns the unacceptability reason, if any, for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \donttest{
#' getunacceptabilityreasonfromtsn(tsn = 183671)
#' }
#' @export
#' @keywords internal
getunacceptabilityreasonfromtsn <- function(tsn = NA, curlopts=list(),
                                            curl = getCurlHandle() )
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getUnacceptabilityReasonFromTSN'
  args <- list()
  if(!is.na(tsn))
    args$tsn <- tsn
  message(paste(url, '?tsn=', tsn, sep=''))
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("tsn","unacceptReason")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep=''), namespaces=namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  names(df) <- toget
  df
}

#' Provides a list of the unique languages used in the vernacular table.
#'
#' @import RCurl XML
#' @examples \donttest{
#' getvernacularlanguages()
#' }
#' @export
#' @keywords internal
getvernacularlanguages <- function()
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/getVernacularLanguages'
  message(url)
  tt <- getURL(url)
  out <- xmlParse(tt)
  namespaces <- c(ax21="http://metadata.itis_service.itis.usgs.org/xsd")
  nodes <- getNodeSet(out, "//ax21:languageNames", namespaces=namespaces)
  languageNames <- sapply(nodes, xmlValue)
  data.frame(languageNames = languageNames)
}

#' Search for tsn by common name
#'
#' @inheritParams getanymatchcount
#' @examples \donttest{
#' searchbycommonname("american bullfrog")
#' searchbycommonname("ferret-badger")
#' searchbycommonname(srchkey="polar bear")
#' }
#' @export
#' @keywords internal
searchbycommonname <- function(srchkey = NA, curlopts=list(), curl = getCurlHandle() )
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/searchByCommonName'
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:commonName", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:language", namespaces=namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(comname=comname, lang=lang, tsn=tsn[-1])
}


#' Searches common name and acts as thin wrapper around \code{searchbycommonnamebeginswith} and \code{searchbycommonnameendswith}
#'
#' @param srchkey search terms
#' @param  curlopts Additional curl options.
#' @param  curl  curl handle
#' @param  from default is to search from beginning. Use \code{end} to serch from end.
#' @seealso searchbycommonnamebeginswith searchbycommonnameendswith
#' @return \code{data.frame}
#' @examples \donttest{
#' searchCommon(srchkey="inch")
#' searchCommon(srchkey="inch", from = "end")
#'}
itis_searchcommon <- function(srchkey = NA, curlopts=list(), curl = getCurlHandle(), from = "begin") {
switch(from,
   begin = searchbycommonnamebeginswith(srchkey = srchkey, curlopts),
   end = searchbycommonnameendswith(srchkey = srchkey, curlopts),
   )
}



#' Search for tsn by common name beginning with
#'
#' @inheritParams getanymatchcount
#' @examples \donttest{
#' searchbycommonnamebeginswith("inch")
#' }
#' @export
#' @keywords internal
searchbycommonnamebeginswith <- function(srchkey = NA, curlopts=list(), curl = getCurlHandle() )
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/searchByCommonNameBeginsWith'
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:commonName", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:language", namespaces=namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue) # last one is a repeat
  nodes <- getNodeSet(out, "//ax21:sciName", namespaces=namespaces)
  data.frame(comname=comname, lang=lang, tsn=tsn[-length(tsn)], stringsAsFactors = FALSE)
}

#' Search for tsn by common name ending with
#'
#' @inheritParams getanymatchcount
#' @examples \donttest{
#' searchbycommonnameendswith(srchkey="snake")
#' }
#' @export
#' @keywords internal
searchbycommonnameendswith <- function(srchkey = NA, curlopts=list(), curl=getCurlHandle())
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/searchByCommonNameEndsWith'
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:commonName", namespaces=namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:language", namespaces=namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue) # last one is a repeat
  data.frame(comname=comname, lang=lang, tsn=tsn[!nchar(tsn) == 0], stringsAsFactors = FALSE)
}

#' Search by scientific name
#'
#' @inheritParams getanymatchcount
#' @examples \donttest{
#' searchbyscientificname("Tardigrada")
#' searchbyscientificname("Quercus douglasii")
#' }
#' @export
#' @keywords internal
searchbyscientificname <- function(srchkey = NA, curlopts=list(), curl = getCurlHandle(), verbose=TRUE)
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/searchByScientificName'
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  tt <- getForm(url,
    .params = args,
    .opts=curlopts,
    curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax21:combinedName", namespaces=namespaces)
  combinedname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces=namespaces)
  tsn <- sapply(nodes, xmlValue) # last one is a repeat
  data.frame(combinedname=combinedname, tsn=tsn, stringsAsFactors = FALSE)
}

#' Search for any match
#'
#' @inheritParams getanymatchcount
#' @examples \donttest{
#' searchforanymatch(srchkey = 202385)
#' searchforanymatch(srchkey = "dolphin")
#' }
#' @export
#' @keywords internal
searchforanymatch <- function(srchkey = NA,  curlopts=list(), curl = getCurlHandle() )
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/searchForAnyMatch'
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd")

	if(is.character(srchkey)){
	  me <- getNodeSet(out, "//ax21:anyMatchList", namespaces=namespaces)
	  comname <- sapply(me, function(x) xmlValue(x[["commonNameList"]][["commonNames"]][["commonName"]]))
	  comname_lang <- sapply(me, function(x) xmlValue(x[["commonNameList"]][["commonNames"]][["language"]]))
	  sciname <- sapply(me, function(x) xmlValue(x[["sciName"]]))
	  tsn <- sapply(me, function(x) xmlValue(x[["tsn"]]))
	  data.frame(tsn=tsn, sciname=sciname, comname=comname, comname_lang=comname_lang, stringsAsFactors = FALSE)
	} else
	{
	  me <- getNodeSet(out, "//ax21:commonNames", namespaces=namespaces)
	  comname <- sapply(me, function(x) xmlValue(x[["commonName"]]))
	  comname_tsn <- sapply(me, function(x) xmlValue(x[["tsn"]]))
	  comname_lang <- sapply(me, function(x) xmlValue(x[["language"]]))
	  data.frame(tsn=comname_tsn, comname=comname, comname_lang=comname_lang, stringsAsFactors = FALSE)
	}
}

#' Search for any matched page
#'
#' @inheritParams getanymatchcount
#' @param pagesize An integer containing the page size (numeric)
#' @param pagenum An integer containing the page number (numeric)
#' @param ascend A boolean containing true for ascending sort order or false
#'    for descending (logical)
#' @examples \donttest{
#' searchforanymatchpaged(srchkey=202385, pagesize=100, pagenum=1, ascend=FALSE)
#' searchforanymatchpaged(srchkey="Zy", pagesize=100, pagenum=1, ascend=FALSE)
#' }
#' @export
#' @keywords internal
searchforanymatchpaged <- function(srchkey = NA, pagesize = NA, pagenum = NA,
                                   ascend = NA, curlopts=list(), curl = getCurlHandle() )
{
	url = 'http://www.itis.gov/ITISWebService/services/ITISService/searchForAnyMatchPaged'
  args <- list()
  if(!is.na(srchkey))
    args$srchKey <- srchkey
  if(!is.na(pagesize))
    args$pageSize <- pagesize
  if(!is.na(pagenum))
    args$pageNum <- pagenum
  if(!is.na(ascend))
    args$ascend <- ascend
  tt <- getForm(url, .params = args, .opts=curlopts, curl = curl)
  out <- xmlParse(tt)
  namespaces <- c(namespaces <- c(ax21="http://data.itis_service.itis.usgs.gov/xsd"))

	if(is.character(srchkey)){
	  me <- getNodeSet(out, "//ax21:anyMatchList", namespaces=namespaces)
	  comname <- sapply(me, function(x) xmlValue(x[["commonNameList"]][["commonNames"]][["commonName"]]))
	  comname_lang <- sapply(me, function(x) xmlValue(x[["commonNameList"]][["commonNames"]][["language"]]))
	  sciname <- sapply(me, function(x) xmlValue(x[["sciName"]]))
	  tsn <- sapply(me, function(x) xmlValue(x[["tsn"]]))
	  data.frame(tsn=tsn, sciname=sciname, comname=comname, comname_lang=comname_lang)
	} else
	{
	  me <- getNodeSet(out, "//ax21:commonNames", namespaces=namespaces)
	  comname <- sapply(me, function(x) xmlValue(x[["commonName"]]))
	  comname_tsn <- sapply(me, function(x) xmlValue(x[["tsn"]]))
	  comname_lang <- sapply(me, function(x) xmlValue(x[["language"]]))
	  data.frame(tsn=comname_tsn, comname=comname, comname_lang=comname_lang)
	}
}

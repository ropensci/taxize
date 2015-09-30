itbase <- function() 'http://www.itis.gov/ITISWebService/services/ITISService/'

itis_GET <- function(endpt, args, ...){
  args <- argsnull(args)
  tt <- GET(paste0(itbase(), endpt), query = args, ...)
  xmlParse(content(tt, "text"), encoding = "UTF-8")
}

# itis_parse <- function(a, b, d){
#   xpathfunc <- function(x, y, nsp) {
#     sapply(getNodeSet(y, paste("//ax21:", x, sep = ''), namespaces = nsp), xmlValue)
#   }
#   df <- setNames(data.frame(t(sapply(a, xpathfunc, y = b, nsp = d))), a)
#   colClasses(df, "character")
# }

itis_parse <- function(a, b, d) {
  xpathfunc <- function(x, y, nsp) {
    sapply(getNodeSet(y, paste("//ax21:", x, sep = ''), namespaces = nsp), xmlValue)
  }
  df <- setNames(data.frame(lapply(a, xpathfunc, y = b, nsp = d)), a)
  colClasses(nmslwr(df), "character")
}

itisdf <- function(a, b, matches, colnames, pastens="ax21"){
  matches <- paste0(sprintf('//%s:', pastens), matches)
  output <- c()
  for (i in seq_along(matches)) {
    nodes <- getNodeSet(a, matches[[i]], namespaces = b)
    output[[i]] <- sapply(nodes, xmlValue)
  }
  if (length(unique(sapply(output, length))) == 1 && unique(sapply(output, length)) == 0) {
    data.frame(NULL, stringsAsFactors = FALSE)
  } else if (all(sapply(output, length) == 1)) {
    setNames(data.frame(t(output), stringsAsFactors = FALSE), colnames)
  } else {
    setNames(data.frame(output, stringsAsFactors = FALSE), colnames)
  }
}

#' Get accepted names from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
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
getacceptednamesfromtsn <- function(tsn, ...) {
  tt_ <- itis_GET("getAcceptedNamesFromTSN", list(tsn = tsn), ...)
	temp <- xmlToList(tt_)
	if (length(temp$return$acceptedNames) == 1) {
    temp$return$tsn
  } else {
		nmslwr(c(submittedtsn = temp$return$tsn, temp$return$acceptedNames[1:2]))
	}
}

#' Get any match count.
#'
#' @param x text or taxonomic serial number (TSN) (character or numeric)
#' @param ... optional additional curl options (debugging tools mostly)
#' @return An integer containing the number of matches the search will return.
#' @examples \dontrun{
#' library('httr')
#' getanymatchcount(202385, config=timeout(3))
#' getanymatchcount("dolphin", config=timeout(3))
#' }
#' @export
#' @keywords internal
getanymatchcount <- function(x, ...) {
	out <- itis_GET("getAnyMatchCount", list(srchKey = x), ...)
  as.numeric(xmlToList(out)$return)
}

#' Get comment detail from TSN
#'
#' @param tsn TSN for a taxonomic group (numeric)
#' @param ... optional additional curl options (debugging tools mostly)
#' @return A data.frame with results.
#' @examples \dontrun{
#' getcommentdetailfromtsn(tsn=180543, config=timeout(4))
#' }
#' @export
#' @keywords internal
getcommentdetailfromtsn <- function(tsn, ...) {
	out <- itis_GET("getCommentDetailFromTSN", list(tsn = tsn), config = timeout(1))
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  matches <- c("commentDetail", "commentId", "commentTimeStamp", "commentator","updateDate")
  colnames <- c('comment','commid','commtime','commentator','updatedate')
	nmslwr(itisdf(a = out, b = namespaces, matches = matches, colnames = colnames))
}

#' Get common names from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcommonnamesfromtsn(183833, config=timeout(1))
#' }
#' @export
#' @keywords internal
getcommonnamesfromtsn <- function(tsn, ...) {
	out <- itis_GET("getCommonNamesFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:commonName", namespaces = namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:language", namespaces = namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(comname = comname, lang = lang, tsn = tsn[-length(tsn)], stringsAsFactors = FALSE)
}

#' Get core metadata from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcoremetadatafromtsn(28727, config=timeout(3))  # coverage and currrency data
#' getcoremetadatafromtsn(183671, config=timeout(4))  # no coverage or currrency data
#' }
#' @export
#' @keywords internal
getcoremetadatafromtsn <- function(tsn, ...) {
	out <- itis_GET("getCoreMetadataFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("credRating","rankId","taxonCoverage","taxonCurrency","taxonUsageRating","tsn")
	itis_parse(toget, out, namespaces)
}

#' Get coverge from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcoveragefromtsn(tsn=28727, config=timeout(4))  # coverage data
#' getcoveragefromtsn(526852, config=timeout(4))  # no coverage data
#' }
#' @export
#' @keywords internal
getcoveragefromtsn <- function(tsn, ...) {
	out <- itis_GET("getCoverageFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
	matches <- c("rankId", "taxonCoverage", "tsn")
	itisdf(a = out, b = namespaces, matches, colnames = tolower(matches))
}

#' Get credibility rating from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcredibilityratingfromtsn(526852, config=timeout(4))
#' }
#' @export
#' @keywords internal
getcredibilityratingfromtsn <- function(tsn, ...) {
	out <- itis_GET("getCredibilityRatingFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
	matches <- c("credRating", "tsn")
	itisdf(out, namespaces, matches, tolower(matches))
}

#' Get possible credibility ratings
#'
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getcredibilityratings(config=timeout(3))
#' }
#' @export
#' @keywords internal
getcredibilityratings <- function(...) {
	out <- itis_GET("getCredibilityRatings", list(), ...)
  namespaces <- c(ax23 = "http://metadata.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax23:credibilityValues", namespaces = namespaces)
  credibilityValues <- sapply(nodes, xmlValue)
  data.frame(credibilityvalues = credibilityValues, stringsAsFactors = FALSE)
}

#' Get currency from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcurrencyfromtsn(28727, config=timeout(3)) # currency data
#' getcurrencyfromtsn(526852, config=timeout(3)) # no currency dat
#' }
#' @export
#' @keywords internal
getcurrencyfromtsn <- function(tsn, ...) {
	out <- itis_GET("getCurrencyFromTSN", list(tsn = tsn), ...)
  namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd")
	matches <- c("rankId","taxonCurrency","tsn")
	nmslwr(itisdf(out, namespaces, matches, tolower(matches)))
}

#' Get date data from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getdatedatafromtsn(180543, config=timeout(3))
#' }
#' @export
#' @keywords internal
getdatedatafromtsn <- function(tsn, ...) {
	out <- itis_GET("getDateDataFromTSN", list(tsn = tsn), ...)
  namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd")
	matches <- c("initialTimeStamp","updateDate","tsn")
	nmslwr(itisdf(out, namespaces, matches, tolower(matches)))
}

#' Get description of the ITIS service
#'
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getdescription(config=timeout(1))
#' }
#' @export
#' @keywords internal
getdescription <- function(...){
	getNodeSet(itis_GET("getDescription", list(), ...), "//ns:return", fun = xmlValue)[[1]]
}

#' Get expert information for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getexpertsfromtsn(180544, config=timeout(3))
#' }
#' @export
#' @keywords internal
getexpertsfromtsn <- function(tsn, ...) {
	out <- itis_GET("getExpertsFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("comment","expert","name","referredTsn","referenceFor","updateDate")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep = ''), namespaces = namespaces),xmlValue)
  }
  nmslwr(setNames(do.call(cbind, lapply(toget, as.data.frame(xpathfunc))), toget))
}

#' Get full hierarchy from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getfullhierarchyfromtsn(37906, config=timeout(3))
#' getfullhierarchyfromtsn(100800, config=timeout(3))
#' }
#' @export
#' @keywords internal

getfullhierarchyfromtsn <- function(tsn, ...) {
	out <- itis_GET("getFullHierarchyFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:parentName", namespaces = namespaces)
  parentName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:parentTsn", namespaces = namespaces)
  parentTsn <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:rankName", namespaces = namespaces)
  rankName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:taxonName", namespaces = namespaces)
  taxonName <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
  tsn <- sapply(nodes, xmlValue)
	nmslwr(data.frame(parentName = parentName, parentTsn = parentTsn,
	           rankName = rankName[-length(rankName)],
	           taxonName = taxonName, tsn = tsn[-1], stringsAsFactors = FALSE))
}

#' Returns the full ITIS record for the TSN in the LSID, found by comparing the
#' 		TSN in the search key to the TSN field. Returns an empty result set if
#'   	there is no match or the TSN is invalid.
#'
#' @param lsid lsid for a taxonomic group (character)
#' @param ... optional additional curl options (debugging tools mostly)
#' @examples \dontrun{
#' getfullrecordfromlsid("urn:lsid:itis.gov:itis_tsn:180543", config=timeout(3))
#' getfullrecordfromlsid("urn:lsid:itis.gov:itis_tsn:180543", config=timeout(3))
#' }
#' @export
#' @keywords internal
getfullrecordfromlsid <- function(lsid, ...) {
	out <- itis_GET("getFullRecordFromLSID", list(lsid = lsid), ...)
	namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd")
	toget <- c("acceptedNameList","commentList","commonNameList","completenessRating",
	           "coreMetadata","credibilityRating","currencyRating","dateData","expertList",
	           "geographicDivisionList","hierarchyUp","jurisdictionalOriginList",
	           "kingdom","otherSourceList","parentTSN","publicationList","scientificName",
	           "synonymList","taxRank","taxonAuthor","unacceptReason","usage")
	parsedat <- function(x){
	  tmp <- getNodeSet(out, sprintf("//ax21:%s",x), namespaces = namespaces, xmlToList)[[1]]
	  tmp <- tmp[!names(tmp) %in% ".attrs"]
	  if (!is.null(tmp)) nmslwr(tmp) else tmp
	}
	nmslwr(setNames(lapply(toget, parsedat), toget))
}

#' Get full record from TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getfullrecordfromtsn(504239, config=timeout(3))
#' getfullrecordfromtsn(202385, config=timeout(3))
#' getfullrecordfromtsn(183833, config=timeout(3))
#' }
#' @export
#' @keywords internal
getfullrecordfromtsn <- function(tsn, ...) {
	out <- itis_GET("getFullRecordFromTSN", list(tsn = tsn), ...)
	namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd")
  toget <- c("acceptedNameList","commentList","commonNameList","completenessRating",
             "coreMetadata","credibilityRating","currencyRating","dateData","expertList",
             "geographicDivisionList","hierarchyUp","jurisdictionalOriginList",
             "kingdom","otherSourceList","parentTSN","publicationList","scientificName",
             "synonymList","taxRank","taxonAuthor","unacceptReason","usage")
	parsedat <- function(x){
	  tmp <- getNodeSet(out, sprintf("//ax21:%s",x), namespaces = namespaces, xmlToList)[[1]]
    tmp <- tmp[!names(tmp) %in% ".attrs"]
    if (!is.null(tmp)) nmslwr(tmp) else tmp
	}
  nmslwr(setNames(lapply(toget, parsedat), toget))
}

#' Get geographic divisions from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getgeographicdivisionsfromtsn(180543, config=timeout(3))
#' }
#' @export
#' @keywords internal
getgeographicdivisionsfromtsn <- function(tsn, ...) {
	out <- itis_GET("getGeographicDivisionsFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("geographicValue","updateDate")
	itis_parse(toget, out, namespaces)
}

#' Get all possible geographic values
#'
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getgeographicvalues(config=timeout(3))
#' }
#' @export
#' @keywords internal
getgeographicvalues <- function(...) {
	out <- itis_GET("getGeographicValues", list(), ...)
  namespaces <- c(ax21 = "http://metadata.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax21:geographicValues", namespaces = namespaces)
  geographicValues <- sapply(nodes, xmlValue)
  data.frame(geographicvalues = geographicValues, stringsAsFactors=FALSE)
}

#' Get global species completeness from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getglobalspeciescompletenessfromtsn(180541, config=timeout(3))
#' }
#' @export
#' @keywords internal
getglobalspeciescompletenessfromtsn <- function(tsn, ...) {
	out <- itis_GET("getGlobalSpeciesCompletenessFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("completeness","rankId","tsn")
	itis_parse(toget, out, namespaces)
}

#' Get hierarchy down from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gethierarchydownfromtsn(161030, config=timeout(3))
#' }
#' @export
#' @keywords internal
gethierarchydownfromtsn <- function(tsn, ...) {
	out <- itis_GET("getHierarchyDownFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
	matches <- paste0("hierarchyList/ax21:",
                    c("parentName","parentTsn","rankName","taxonName","tsn"))
	itisdf(out, namespaces, matches, tolower(c("parentName","parentTsn","rankName","taxonName","tsn")))
}

#' Get hierarchy up from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gethierarchyupfromtsn(36485, config=timeout(3))
#' }
#' @export
#' @keywords internal
gethierarchyupfromtsn <- function(tsn, ...) {
	out <- itis_GET("getHierarchyUpFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
	matches <- c("parentName","parentTsn","rankName","taxonName","tsn")
	itisdf(out, namespaces, matches, tolower(matches))
}

#' Get itis terms from common names
#'
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' getitistermsfromcommonname("buya")
#'
#' library('httr')
#' getitistermsfromcommonname("buya", config=timeout(1))
#' }
#' @export
#' @keywords internal
getitistermsfromcommonname <- function(x, ...) {
  out <- itis_GET("getITISTermsFromCommonName", list(srchKey = x), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
	gg <- getNodeSet(out, "//ax21:itisTerms", namespaces = namespaces, xmlToList)
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
#' @examples \dontrun{
#' getitisterms("bear")
#' }
#' @export
#' @keywords internal
getitisterms <- function(x, ...) {
  out <- itis_GET("getITISTerms", list(srchKey = x), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  gg <- getNodeSet(out, "//ax21:itisTerms", namespaces = namespaces, xmlToList)
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
#' @examples \dontrun{
#' getitistermsfromscientificname("ursidae")
#' getitistermsfromscientificname("Ursus")
#' }
#' @export
#' @keywords internal
getitistermsfromscientificname <- function(x, ...) {
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
#' @examples \dontrun{
#' getjurisdictionaloriginfromtsn(180543, config=timeout(3))
#' }
#' @export
#' @keywords internal
getjurisdictionaloriginfromtsn <- function(tsn, ...) {
	out <- itis_GET("getJurisdictionalOriginFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("jurisdictionValue","origin","updateDate")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep = ''), namespaces = namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  if(nrow(df) == 0){
    data.frame(jurisdictionvalue=NA,origin=NA,updatedate=NA)
  } else {
    setNames(df, tolower(toget))
  }
}

#' Get jurisdiction origin values
#'
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getjurisdictionoriginvalues(config=timeout(3))
#' }
#' @export
#' @keywords internal
getjurisdictionoriginvalues <- function(...) {
	out <- itis_GET("getJurisdictionalOriginValues", list(), ...)
  namespaces <- c(ax23 = "http://metadata.itis_service.itis.usgs.gov/xsd")
	matches <- c("jurisdiction","origin")
	itisdf(a = out, b = namespaces, matches = matches, colnames = tolower(matches), pastens = "ax23")
}

#' Get possible jurisdiction values
#'
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getjurisdictionvalues(config=timeout(3))
#' }
#' @export
#' @keywords internal
getjurisdictionvalues <- function(...) {
	out <- itis_GET("getJurisdictionValues", list(), ...)
  namespaces <- c(ax23 = "http://metadata.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax23:jurisdictionValues", namespaces = namespaces)
  jurisdictionValues <- sapply(nodes, xmlValue)
  data.frame(jurisdictionvalues = jurisdictionValues, stringsAsFactors = FALSE)
}

#' Get kingdom names from tsn
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getkingdomnamefromtsn(202385, config=timeout(3))
#' }
#' @export
#' @keywords internal
getkingdomnamefromtsn <- function(tsn, ...) {
	out <- itis_GET("getKingdomNameFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("kingdomId","kingdomName","tsn")
	itis_parse(toget, out, namespaces)
}

#' Get all possible kingdom names
#'
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getkingdomnames(config=timeout(3))
#' }
#' @export
#' @keywords internal
getkingdomnames <- function(...) {
	out <- itis_GET("getKingdomNames", list(), ...)
  namespaces <- c(ax21 = "http://metadata.itis_service.itis.usgs.gov/xsd")
	matches <- c("kingdomId","kingdomName","tsn")
	itisdf(out, namespaces, matches, tolower(matches))
}

#' Provides the date the ITIS database was last updated.
#'
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getlastchangedate(config=timeout(3))
#' }
#' @export
#' @keywords internal
getlastchangedate <- function(...) {
	out <- itis_GET("getLastChangeDate", list(), ...)
  namespaces <- c(ax21 = "http://metadata.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax21:updateDate", namespaces = namespaces)
  sapply(nodes, xmlValue)
}

#' Gets the unique LSID for the TSN, or an empty result if there is no match.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getlsidfromtsn(155166, config=timeout(3))
#' }
#' @export
#' @keywords internal
getlsidfromtsn <- function(tsn, ...) xmlToList(itis_GET("getLSIDFromTSN", list(tsn = tsn), ...))[[1]]

#' Returns a list of the other sources used for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getothersourcesfromtsn(182662, config=timeout(3))
#' }
#' @export
#' @keywords internal
getothersourcesfromtsn <- function(tsn, ...) {
	out <- itis_GET("getOtherSourcesFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("acquisitionDate","name","referredTsn","source",
                "sourceType","updateDate","version")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep = ''), namespaces = namespaces),xmlValue)
  }
  nmslwr(setNames(do.call(cbind, lapply(toget, as.data.frame(xpathfunc))), toget))
}

#' Returns the parent TSN for the entered TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getparenttsnfromtsn(202385, config=timeout(3))
#' }
#' @export
#' @keywords internal
getparenttsnfromtsn <- function(tsn, ...) {
	out <- itis_GET("getParentTSNFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("parentTsn","tsn")
	itis_parse(toget, out, namespaces)
}

#' Returns a list of the pulications used for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getpublicationsfromtsn(70340, config=timeout(3))
#' }
#' @export
#' @keywords internal
getpublicationsfromtsn <- function(tsn, ...) {
	out <- itis_GET("getPublicationsFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("actualPubDate","isbn","issn","listedPubDate","pages",
                "pubComment","pubName","pubPlace","publisher","referenceAuthor",
                "name","refLanguage","referredTsn","title","updateDate")
  xpathfunc <- function(x) {
    sapply(getNodeSet(out, paste("//ax21:", x, sep = ''), namespaces = namespaces),xmlValue)
  }
  df <-  do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
  if (NROW(df) > 0) names(df) <- tolower(toget)
  df
}

#' Provides a list of all the unique rank names contained in the database and
#'  their kingdom and rank ID values.
#'
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getranknames(config=timeout(3))
#' }
#' @export
#' @keywords internal
getranknames <- function(...) {
	out <- itis_GET("getRankNames", list(), ...)
  namespaces <- c(ax23 = "http://metadata.itis_service.itis.usgs.gov/xsd")
	matches <- c("kingdomName","rankId","rankName")
	itisdf(out, namespaces, matches, tolower(matches), "ax23")
}

#' Gets the partial ITIS record for the TSN in the LSID, found by comparing the
#'  TSN in the search key to the TSN field. Returns an empty result set if
#'  there is no match or the TSN is invalid.
#'
#' @inheritParams getfullrecordfromlsid
#' @examples \dontrun{
#' getrecordfromlsid("urn:lsid:itis.gov:itis_tsn:180543", config=timeout(3))
#' }
#' @export
#' @keywords internal
getrecordfromlsid <- function(lsid, ...) {
	out <- itis_GET("getRecordFromLSID", list(lsid = lsid), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("authorship","genusPart","infragenericEpithet",
                "infraspecificEpithet","lsid","nameComplete","nomenclaturalCode",
                "rank","rankString","specificEpithet","uninomial","tsn")
	itis_parse(toget, out, namespaces)
}

#' Returns the review year for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getreviewyearfromtsn(180541, config=timeout(3))
#' }
#' @export
#' @keywords internal
getreviewyearfromtsn <- function(tsn, ...) {
	out <- itis_GET("getReviewYearFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("rankId","reviewYear","tsn")
	itis_parse(toget, out, namespaces)
}

#' Returns the scientific name for the TSN. Also returns the component parts
#'    (names and indicators) of the scientific name.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getscientificnamefromtsn(531894, config=timeout(3))
#' }
#' @export
#' @keywords internal
getscientificnamefromtsn <- function(tsn, ...) {
	out <- itis_GET("getScientificNameFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("combinedName","unitInd1","unitInd3","unitName1","unitName2",
                "unitName3","tsn")
	itis_parse(toget, out, namespaces)
}

#' Returns a list of the synonyms (if any) for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getsynonymnamesfromtsn(183671, config=timeout(3)) # tsn not accepted
#' getsynonymnamesfromtsn(526852, config=timeout(5)) # tsn accepted
#' }
#' @export
#' @keywords internal
getsynonymnamesfromtsn <- function(tsn, ...) {
	out <- itis_GET("getSynonymNamesFromTSN", list(tsn = tsn), ...)
  namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax21:sciName", namespaces = namespaces)
  if ( length(sapply(nodes, xmlValue)) == 0 ) {
    name <- list("nomatch")
  } else {
    name <- sapply(nodes, xmlValue)
  }
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
  if ( length(sapply(nodes, xmlValue)) == 1 ) {
    tsn <- sapply(nodes, xmlValue)
  } else {
    tsn <- sapply(nodes, xmlValue)
    tsn <- tsn[-1]
  }
  data.frame(name = name, tsn = tsn, stringsAsFactors = FALSE)
}

#' Returns the author information for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gettaxonauthorshipfromtsn(183671, config=timeout(3))
#' }
#' @export
#' @keywords internal
gettaxonauthorshipfromtsn <- function(tsn, ...) {
	out <- itis_GET("getTaxonAuthorshipFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("authorship","updateDate","tsn")
	itis_parse(toget, out, namespaces)
}

#' Returns the kingdom and rank information for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gettaxonomicranknamefromtsn(202385, config=timeout(3))
#' }
#' @export
#' @keywords internal
gettaxonomicranknamefromtsn <- function(tsn, ...) {
	out <- itis_GET("getTaxonomicRankNameFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("kingdomId","kingdomName","rankId","rankName","tsn")
	itis_parse(toget, out, namespaces)
}

#' Returns the usage information for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gettaxonomicusagefromtsn(526852, config=timeout(3))
#' }
#' @export
#' @keywords internal
gettaxonomicusagefromtsn <- function(tsn, ...) {
	out <- itis_GET("getTaxonomicUsageFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("taxonUsageRating","tsn")
	itis_parse(toget, out, namespaces)
}

#' Get tsn by vernacular language
#'
#' @param language A string containing the language. This is a language string,
#'    not the international language code (character)
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' gettsnbyvernacularlanguage("french", config=timeout(3))
#' }
#' @export
#' @keywords internal
gettsnbyvernacularlanguage <- function(language, ...) {
	out <- itis_GET("getTsnByVernacularLanguage", list(language = language), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
	matches <- c("commonName","language","tsn")
	itisdf(out, namespaces, matches, tolower(matches))
}

#' Gets the TSN corresponding to the LSID, or an empty result if there is no match.
#'
#' @inheritParams getfullrecordfromlsid
#' @examples \dontrun{
#' gettsnfromlsid(lsid="urn:lsid:itis.gov:itis_tsn:28726", config=timeout(3))
#' gettsnfromlsid("urn:lsid:itis.gov:itis_tsn:0", config=timeout(3))
#' }
#' @export
#' @keywords internal
gettsnfromlsid <- function(lsid, ...) {
	out <- itis_GET("getTSNFromLSID", list(lsid = lsid), ...)
  if ( !is.na( suppressWarnings(as.numeric(xmlToList(out)[[1]])) ) ) {
    suppressWarnings( as.numeric(xmlToList(out)[[1]]) )
  } else {
    return("invalid TSN")
  }
}

#' Returns the unacceptability reason, if any, for the TSN.
#'
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getunacceptabilityreasonfromtsn(183671, config=timeout(3))
#' }
#' @export
#' @keywords internal
getunacceptabilityreasonfromtsn <- function(tsn, ...) {
	out <- itis_GET("getUnacceptabilityReasonFromTSN", list(tsn = tsn), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  toget <- list("tsn","unacceptReason")
	itis_parse(toget, out, namespaces)
}

#' Provides a list of the unique languages used in the vernacular table.
#'
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' getvernacularlanguages(config=timeout(3))
#' }
#' @export
#' @keywords internal
getvernacularlanguages <- function(...) {
	out <- itis_GET("getVernacularLanguages", list(), ...)
  namespaces <- c(ax21 = "http://metadata.itis_service.itis.usgs.gov/xsd")
  nodes <- getNodeSet(out, "//ax21:languageNames", namespaces = namespaces)
  languageNames <- sapply(nodes, xmlValue)
  data.frame(languagenames = languageNames, stringsAsFactors = FALSE)
}

#' Search for tsn by common name
#'
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchbycommonname("american bullfrog", config=timeout(3))
#' searchbycommonname("ferret-badger", config=timeout(3))
#' searchbycommonname("polar bear", config=timeout(3))
#' }
#' @export
#' @keywords internal
searchbycommonname <- function(x, ...) {
	out <- itis_GET("searchByCommonName", list(srchKey = x), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:commonName", namespaces = namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:language", namespaces = namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
  tsn <- sapply(nodes, xmlValue)
  data.frame(comname=comname, lang=lang, tsn=tsn[-1], stringsAsFactors = FALSE)
}

#' Searches common name and acts as thin wrapper around \code{searchbycommonnamebeginswith} and \code{searchbycommonnameendswith}
#'
#' @export
#' @param x Search terms
#' @param from Default is to search from beginning. Use \code{end} to serch from end.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @seealso searchbycommonnamebeginswith searchbycommonnameendswith
#' @return \code{data.frame}
#' @examples \dontrun{
#' itis_searchcommon("inch", config=timeout(3))
#' itis_searchcommon("inch", from = "end", config=timeout(3))
#'}
itis_searchcommon <- function(x, from = "begin", ...) {
  switch(from,
         begin = searchbycommonnamebeginswith(x = x, ...),
         end = searchbycommonnameendswith(x = x, ...)
  )
}

#' Search for tsn by common name beginning with
#'
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchbycommonnamebeginswith("inch", config=timeout(3))
#' }
#' @export
#' @keywords internal
searchbycommonnamebeginswith <- function(x, ...) {
	out <- itis_GET("searchByCommonNameBeginsWith", list(srchKey = x), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:commonName", namespaces = namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:language", namespaces = namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
  tsn <- sapply(nodes, xmlValue) # last one is a repeat
  nodes <- getNodeSet(out, "//ax21:sciName", namespaces = namespaces)
  data.frame(comname=comname, lang=lang, tsn=tsn[-length(tsn)], stringsAsFactors = FALSE)
}

#' Search for tsn by common name ending with
#'
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchbycommonnameendswith("snake", config=timeout(3))
#' }
#' @export
#' @keywords internal
searchbycommonnameendswith <- function(x, ...) {
	out <- itis_GET("searchByCommonNameEndsWith", list(srchKey = x), ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
  nodes <- getNodeSet(out, "//ax21:commonName", namespaces = namespaces)
  comname <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:language", namespaces = namespaces)
  lang <- sapply(nodes, xmlValue)
  nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
  tsn <- sapply(nodes, xmlValue) # last one is a repeat
  data.frame(comname=comname, lang=lang, tsn=tsn[!nchar(tsn) == 0], stringsAsFactors = FALSE)
}

#' Search by scientific name
#'
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchbyscientificname("Tardigrada", config=timeout(3))
#' searchbyscientificname("Quercus douglasii", config=timeout(3))
#' }
#' @export
#' @keywords internal
searchbyscientificname <- function(x, ...) {
	out <- itis_GET("searchByScientificName", list(srchKey = x), ...)
  namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd")
	matches <- c("combinedName","tsn")
	itisdf(out, namespaces, matches, tolower(matches))
}

#' Search for any match
#'
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchforanymatch(202385, config=timeout(3))
#' searchforanymatch("dolphin", config=timeout(3))
#' }
#' @export
#' @keywords internal
searchforanymatch <- function(x, ...) {
	out <- itis_GET("searchForAnyMatch", list(srchKey = x), ...)
  namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd")

	if (is.character(x)) {
	  me <- getNodeSet(out, "//ax21:anyMatchList", namespaces = namespaces)
	  comname <- sapply(me, function(x) xmlValue(x[["commonNameList"]][["commonNames"]][["commonName"]]))
	  comname_lang <- sapply(me, function(x) xmlValue(x[["commonNameList"]][["commonNames"]][["language"]]))
	  sciname <- sapply(me, function(x) xmlValue(x[["sciName"]]))
	  tsn <- sapply(me, function(x) xmlValue(x[["tsn"]]))
	  data.frame(tsn=tsn, sciname=sciname, comname=comname, comname_lang=comname_lang, stringsAsFactors = FALSE)
	} else {
	  me <- getNodeSet(out, "//ax21:commonNames", namespaces = namespaces)
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
#' @examples \dontrun{
#' searchforanymatchpaged(202385, pagesize=100, pagenum=1, ascend=FALSE, config=timeout(3))
#' searchforanymatchpaged("Zy", pagesize=100, pagenum=1, ascend=FALSE, config=timeout(3))
#' }
#' @export
#' @keywords internal
searchforanymatchpaged <- function(x, pagesize = NULL, pagenum = NULL, ascend = NULL, ...) {
  args <- tc(list(srchKey=x, pageSize=pagesize, pageNum=pagenum, ascend=ascend))
	out <- itis_GET("searchForAnyMatchPaged", args, ...)
  namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))

	if (is.character(x)) {
	  me <- getNodeSet(out, "//ax21:anyMatchList", namespaces = namespaces)
	  comname <- sapply(me, function(x) xmlValue(x[["commonNameList"]][["commonNames"]][["commonName"]]))
	  comname_lang <- sapply(me, function(x) xmlValue(x[["commonNameList"]][["commonNames"]][["language"]]))
	  sciname <- sapply(me, function(x) xmlValue(x[["sciName"]]))
	  tsn <- sapply(me, function(x) xmlValue(x[["tsn"]]))
	  data.frame(tsn=tsn, sciname=sciname, comname=comname, comname_lang=comname_lang, stringsAsFactors = FALSE)
	} else {
	  me <- getNodeSet(out, "//ax21:commonNames", namespaces = namespaces)
	  comname <- sapply(me, function(x) xmlValue(x[["commonName"]]))
	  comname_tsn <- sapply(me, function(x) xmlValue(x[["tsn"]]))
	  comname_lang <- sapply(me, function(x) xmlValue(x[["language"]]))
	  data.frame(tsn=comname_tsn, comname=comname, comname_lang=comname_lang, stringsAsFactors = FALSE)
	}
}

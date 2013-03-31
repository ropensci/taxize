#' Get accepted names from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' taxize:::getacceptednamesfromtsn(tsn=208527)  # TSN accepted - good name
#' taxize:::getacceptednamesfromtsn(208527, locally=TRUE)  # run it locally
#' getacceptednamesfromtsn(tsn=504239)  # TSN not accepted - input TSN is old name
#' getacceptednamesfromtsn('504239', FALSE)  # TSN not accepted - input TSN is old name
#' }
getacceptednamesfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
# 		#
		query_ACCEPTED_FROM_TSN <- paste("SELECT t.tsn, t.complete_name as combinedName, a.taxon_author as author 
                      from taxonomic_units t  
                      left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id 
                      inner join synonym_links s on s.tsn_accepted = t.tsn and s.tsn = ", tsn, ";")
		return( dbGetQuery(conn=sqlconn, query_ACCEPTED_FROM_TSN) )
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getAcceptedNamesFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		tt <- getForm(url, .params = args, ..., curl = curl)
		tt_ <- xmlParse(tt)
		temp <- xmlToList(tt_)
		if (length(temp$return$acceptedNames) == 1) {
			message("Good name!")
			temp$return$tsn
		}
		else {
			message("Bad name!")
			c(submittedTsn = temp$return$tsn, temp$return$acceptedNames[1:2])
		}
	}
}

#' Get any match count.
#' 
#' @import XML RCurl RSQLite
#' @param srchkey text or taxonomic serial number (TSN) (character or numeric)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass 
#'  the returned value in here (avoids unnecessary footprint)
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#'  queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @param sqlconn The sqlite3 connection object.
#' @return An integer containing the number of matches the search will return.
#' @examples \dontrun{
#' getanymatchcount(202385)
#' getanymatchcount("dolphin")
#' }
#' @export
getanymatchcount <- function(srchkey = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		if (is.numeric(srchkey)) {
			query_ANY_TSN_MATCH_COUNT <- paste("Select count(*) from taxonomic_units where tsn = ", srchkey, ";")
			return(dbGetQuery(conn=sqlconn, query_ANY_TSN_MATCH_COUNT))
		}
		else {
			query_ANY_MATCH_COUNT <- paste("Select count(*)  
                        from taxonomic_units t 
                        inner join vernaculars v on v.tsn = t.tsn and v.vernacular_name like ", paste("'", srchkey, "'", sep = ""), "union  
                            Select count(*)  
                            from taxonomic_units t 
                            where t.complete_name like ", paste("'", srchkey, "'", sep = ""), ";")
			return(dbGetQuery(conn=sqlconn, query_ANY_MATCH_COUNT))
		}
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getAnyMatchCount"
		args <- list()
		if (!is.na(srchkey)) 
			args$srchKey <- srchkey
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		xmlToList(out)
	}
}

#' Get comment detail from TSN
#' 
#' @import XML RCurl RSQLite
#' @param tsn TSN for a taxonomic group (numeric)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass 
#'  the returned value in here (avoids unnecessary footprint)
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#'  queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @param sqlconn The sqlite3 connection object.
#' @return A data.frame with results.
#' @examples \dontrun{
#' getcommentdetailfromtsn(tsn=180543)
#' getcommentdetailfromtsn(tsn=180543, locally=TRUE)
#' }
#' @export
getcommentdetailfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		query_TAXON_COMMENT_FROM_TSN <- paste("Select c.* from comments c inner join tu_comments_links t  
                           on c.comment_id = t.comment_id and tsn = ", tsn, "order by comment_time_stamp;")
		return(dbGetQuery(conn=sqlconn, query_TAXON_COMMENT_FROM_TSN))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getCommentDetailFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		nodes <- getNodeSet(out, "//ax21:commentDetail", namespaces = namespaces)
		comment <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:commentId", namespaces = namespaces)
		commid <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:commentTimeStamp", namespaces = namespaces)
		commTime <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:commentator", namespaces = namespaces)
		commentator <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:updateDate", namespaces = namespaces)
		updatedate <- sapply(nodes, xmlValue)
		data.frame(comment = comment, commid = commid, commTime = commTime, 
							 commentator = commentator, updatedate = updatedate)
	}
}

#' Get common names from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcommonnamesfromtsn(183833)
#' }
#' @export 
getcommonnamesfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_COMMON_NAME_BY_TSN_SRCH <- paste("select t.tsn as tsn, v.language as language, a.taxon_author as author, 
                            v.vernacular_name as commonName, t.complete_name as combinedName 
                            from vernaculars v 
                            inner join taxonomic_units t on v.tsn = t.tsn 
                            and t.tsn = ", tsn, "left join taxon_authors_lkp a on a.taxon_author_id = t.taxon_author_id;")
		return(dbGetQuery(conn=sqlconn, query_COMMON_NAME_BY_TSN_SRCH))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getCommonNamesFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		nodes <- getNodeSet(out, "//ax21:commonName", namespaces = namespaces)
		comname <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:language", namespaces = namespaces)
		lang <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		data.frame(comname = comname, lang = lang, tsn = tsn[-length(tsn)])
	}
}

#' Get core metadata from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcoremetadatafromtsn(tsn = 28727)  # coverage and currrency data
#' getcoremetadatafromtsn(tsn = 183671)  # no coverage or currrency data
#' }
#' @export 
getcoremetadatafromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_CORE_METADATA_FROM_TSN <- paste("Select tsn, rank_id, name_usage, unaccept_reason, credibility_rtng, 
                           completeness_rtng, currency_rating from taxonomic_units where tsn = ", tsn, ";")
		return(dbGetQuery(conn=sqlconn, query_CORE_METADATA_FROM_TSN))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getCoreMetadataFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("credRating", "rankId", "taxonCoverage", 
									"taxonCurrency", "taxonUsageRating", "tsn")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Get coverge from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcoveragefromtsn(tsn = 28727)  # coverage data
#' getcoveragefromtsn(tsn = 526852)  # no coverage data
#' }
#' @export 
getcoveragefromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_COVERAGE_FROM_TSN <- paste("Select tsn, rank_id, completeness_rtng from taxonomic_units where tsn =", tsn, ";")
		return(dbGetQuery(conn=sqlconn, query_COVERAGE_FROM_TSN))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getCoverageFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		nodes <- getNodeSet(out, "//ax21:rankId", namespaces = namespaces)
		rankid <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:taxonCoverage", namespaces = namespaces)
		taxoncoverage <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		data.frame(rankid = rankid, taxoncoverage = taxoncoverage, tsn = tsn)
	}
}

#' Get credibility rating from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcredibilityratingfromtsn(tsn = 526852)
#' }
#' @export 
getcredibilityratingfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL)
{
	if (locally) {
		#
		query_CRED_FROM_TSN <- paste("Select tsn, credibility_rtng from taxonomic_units where tsn =", tsn, ";")
		return(dbGetQuery(conn=sqlconn, query_CRED_FROM_TSN))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getCredibilityRatingFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		nodes <- getNodeSet(out, "//ax21:credRating", namespaces = namespaces)
		credrating <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		data.frame(credrating = credrating, tsn = tsn)
	}
}

#' Get possible credibility ratings
#' 
#' @import RCurl XML
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#'  queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @param sqlconn The sqlite3 connection object.
#' @examples \dontrun{
#' getcredibilityratings()
#' }
#' @export 
getcredibilityratings <- function(locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_CREDIBILITY_RATINGS <- paste("select distinct credibility_rtng from taxonomic_units order by credibility_rtng;")
		return(dbGetQuery(conn=sqlconn, query_CREDIBILITY_RATINGS))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getCredibilityRatings"
		message(url)
		tt <- getURL(url)
		out <- xmlParse(tt)
		namespaces <- c(ax25 = "http://metadata.itis_service.itis.usgs.org/xsd")
		nodes <- getNodeSet(out, "//ax25:credibilityValues", 
												namespaces = namespaces)
		credibilityValues <- sapply(nodes, xmlValue)
		data.frame(credibilityValues = credibilityValues)
	}
}

#' Get currency from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getcurrencyfromtsn(tsn = 28727) # currency data
#' getcurrencyfromtsn(tsn = 526852) # no currency dat
#' }
#' @export 
getcurrencyfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_CURRENCY_FROM_TSN <- paste("Select tsn, rank_id, currency_rating from taxonomic_units where tsn =", tsn, ";")
		return(dbGetQuery(conn=sqlconn, query_CURRENCY_FROM_TSN))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getCurrencyFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.org/xsd")
		nodes <- getNodeSet(out, "//ax21:rankId", namespaces = namespaces)
		rankid <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:taxonCurrency", namespaces = namespaces)
		taxoncurrency <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		data.frame(rankid = rankid, taxoncurrency = taxoncurrency, tsn = tsn)
	}
}

#' Get date data from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getdatedatafromtsn(tsn = 180543)
#' }
#' @export 
getdatedatafromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_TAXON_DATES_FROM_TSN <- paste("Select initial_time_stamp, update_date from taxonomic_units where tsn = ", tsn, ";")
		return(dbGetQuery(conn=sqlconn, query_TAXON_DATES_FROM_TSN))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getDateDataFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.org/xsd")
		nodes <- getNodeSet(out, "//ax21:initialTimeStamp", namespaces = namespaces)
		initialTimeStamp <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:updateDate", namespaces = namespaces)
		updateDate <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		data.frame(initialTimeStamp = initialTimeStamp, updateDate = updateDate, tsn = tsn)
	}
}

#' Get description of the ITIS service
#' 
#' @import RCurl XML
#' @examples \dontrun{
#' getdescription()
#' }
#' @export
getdescription <- function() 
{
	url = "http://www.itis.gov/ITISWebService/services/ITISService/getDescription"
	message(url)
	tt <- getURL(url)
	out <- xmlParse(tt)
	namespaces <- c(ax26 = "http://itis_service.itis.usgs.org/xsd")
	nodes <- getNodeSet(out, "//ax26:description", namespaces = namespaces)
	sapply(nodes, xmlValue)
}

#' Get expert information for the TSN.
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getexpertsfromtsn(tsn = 180544)
#' }
#' @export 
getexpertsfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_TAXON_EXPERTS_FROM_TSN <- paste("Select '1' as sort_order, r.vernacular_name, NULL As language, e.*  
                           from reference_links r, experts e  
                           where r.doc_id_prefix = e.expert_id_prefix and r.documentation_id = e.expert_id  
                           and (r.vernacular_name = '' or r.vernacular_name is null) and r.tsn = ", tsn, "UNION Select '2' as sort_order, v.vernacular_name, v.language, e.*  
                               from vernaculars v, experts e, vern_ref_links vr  
                               where vr.doc_id_prefix = e.expert_id_prefix and vr.documentation_id = e.expert_id  
                               and vr.vern_id = v.vern_id and vr.tsn = v.tsn and v.tsn = ", tsn, "order by expert, sort_order;")
		return(dbGetQuery(conn=sqlconn, query_TAXON_EXPERTS_FROM_TSN))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getExpertsFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("comment", "expert", "name", "referredTsn", 
									"referenceFor", "updateDate")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Get full hierarchy from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @details Can't do local searcha s don't have the SQL syntax yet.
#' @examples \dontrun{
#' getfullhierarchyfromtsn(tsn = 37906)
#' getfullhierarchyfromtsn(tsn = 100800)
#' }
#' @export 
getfullhierarchyfromtsn <- function(tsn = NA, ..., curl = getCurlHandle()) 
{
	url = "http://www.itis.gov/ITISWebService/services/ITISService/getFullHierarchyFromTSN"
	args <- list()
	if (!is.na(tsn)) 
		args$tsn <- tsn
	message(paste(url, "?tsn=", tsn, sep = ""))
	tt <- getForm(url, .params = args, curl = curl)
	out <- xmlParse(tt)
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
	out <- data.frame(parentName = parentName, parentTsn = parentTsn, 
										rankName = rankName[-length(rankName)], taxonName = taxonName, tsn = tsn[-1], stringsAsFactors = FALSE)
	return(out)
}

#' Returns the full ITIS record for the TSN in the LSID, found by comparing the 
#'      TSN in the search key to the TSN field. Returns an empty result set if there 
#'      is no match or the TSN is invalid.
#' 
#' @import RCurl XML
#' @param lsid lsid for a taxonomic group (character)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass 
#'  the returned value in here (avoids unnecessary footprint) 
#' @examples \dontrun{
#' getfullrecordfromlsid(lsid = "urn:lsid:itis.gov:itis_tsn:180543")
#' }
#' @export 
getfullrecordfromlsid <- function(lsid = NA, ..., curl = getCurlHandle()) 
{
	url = "http://www.itis.gov/ITISWebService/services/ITISService/getFullRecordFromLSID"
	args <- list()
	if (!is.na(lsid)) 
		args$lsid <- lsid
	message(paste(url, "?lsid=", lsid, sep = ""))
	tt <- getForm(url, .params = args, ..., curl = curl)
	xmlParse(tt)
}

#' Get full record from TSN.
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getfullrecordfromtsn(tsn = 183833)
#' }
#' @export 
getfullrecordfromtsn <- function(tsn = NA, ..., curl = getCurlHandle()) 
{
	url = "http://www.itis.gov/ITISWebService/services/ITISService/getFullRecordFromTSN"
	args <- list()
	if (!is.na(tsn)) 
		args$tsn <- tsn
	message(paste(url, "?tsn=", tsn, sep = ""))
	tt <- getForm(url, .params = args, ..., curl = curl)
	xmlParse(tt)
}

#' Get geographic divisions from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getgeographicdivisionsfromtsn(tsn = 180543)
#' }
#' @export 
getgeographicdivisionsfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_TAXON_GEO_DIV_FROM_TSN <- paste("Select * from geographic_div where tsn = ", tsn, "order by geographic_value;")
		return(dbGetQuery(conn=sqlconn, query_TAXON_GEO_DIV_FROM_TSN))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getGeographicDivisionsFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("geographicValue", "updateDate", "tsn")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Get all possible geographic values
#' 
#' @import RCurl XML
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#'  queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @param sqlconn The sqlite3 connection object.
#' @examples \dontrun{
#' getgeographicvalues()
#' }
#' @export 
getgeographicvalues <- function(locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_GEOGRAPHIC_VALUES <- paste("select distinct geographic_value from geographic_div order by geographic_value;")
		return(dbGetQuery(conn=sqlconn, query_GEOGRAPHIC_VALUES))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getGeographicValues"
		message(url)
		tt <- getURL(url)
		out <- xmlParse(tt)
		namespaces <- c(ax21 = "http://metadata.itis_service.itis.usgs.org/xsd")
		nodes <- getNodeSet(out, "//ax21:geographicValues", namespaces = namespaces)
		geographicValues <- sapply(nodes, xmlValue)
		data.frame(geographicValues = geographicValues)
	}
}

#' Get global species completeness from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getglobalspeciescompletenessfromtsn(tsn = 180541)
#' }
#' @export
getglobalspeciescompletenessfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_COMPLETENESS_FROM_TSN <- paste("Select tsn, rank_id, completeness_rtng from taxonomic_units where tsn =", tsn, ";")
		return(dbGetQuery(conn=sqlconn, query_COMPLETENESS_FROM_TSN))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getGlobalSpeciesCompletenessFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("completeness", "rankId", "tsn")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Get hierarchy down from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gethierarchydownfromtsn(tsn = 161030)
#' gethierarchydownfromtsn(tsn = 161030, locally=TRUE)
#' }
#' @export 
gethierarchydownfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL)
{
	if (locally) {
		#
		query_HIERARCHY_DN_FROM_TSN <- paste("select t.tsn, t.parent_tsn, t.complete_name as combinedName, 
                           r.rank_name, r.rank_id, a.taxon_author as author 
                           from taxonomic_units t 
                           left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id 
                           inner join taxon_unit_types r on r.rank_id = t.rank_id and r.kingdom_id = t.kingdom_id 
                           where (t.parent_tsn= ", tsn, " or t.tsn= ", tsn, ")
                               and (t.name_usage='valid' or t.name_usage='accepted');")
		temp <- dbGetQuery(conn=sqlconn, query_HIERARCHY_DN_FROM_TSN)
		temp2 <- data.frame(parentTsn = temp$parent_tsn, rankName = temp$rank_name, rankId = temp$rank_id, taxonName = temp$combinedName, tsn = temp$tsn)
		temp3 <- temp2[!temp2$tsn %in% tsn,]
		return( temp3 )
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getHierarchyDownFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, 
# 									..., 
									curl = curl)
		out <- xmlParse(tt)
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
		data.frame(parentName = parentName, parentTsn = parentTsn,
							 rankName = rankName[-length(rankName)], taxonName = taxonName, tsn = tsn[-1])
	}
}

#' Get hierarchy up from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gethierarchyupfromtsn(tsn = 36485)
#' gethierarchyupfromtsn(tsn = 36485, locally=TRUE)
#' }
#' @export 
gethierarchyupfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_HIERARCHY_UP_FROM_TSN <- paste("select distinct t.parent_tsn, t.tsn, l.complete_name as parent_name, 
                          a.taxon_author as author, t.complete_name as combinedName, r.rank_name 
                          from taxonomic_units t  
                          left outer join Taxonomic_units l  on l.tsn = t.parent_tsn 
                          left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id 
                          inner join taxon_unit_types r on r.rank_id = t.rank_id and r.kingdom_id = t.kingdom_id 
                          where t.tsn=", tsn, ";")
		temp <- dbGetQuery(conn=sqlconn, query_HIERARCHY_UP_FROM_TSN)
		temp2 <- data.frame(parentName = temp$parent_name, parentTsn = temp$parent_tsn, rankName = temp$rank_name, taxonName = temp$combinedName, tsn = temp$tsn)
		return( temp2 )
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getHierarchyUpFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
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
		data.frame(parentName = parentName, parentTsn = parentTsn, 
							 rankName = rankName, taxonName = taxonName, tsn = tsn)
	}
}

#' Get itis terms from common names
#' 
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' getitistermsfromcommonname("buya")
#' }
#' @export 
getitistermsfromcommonname <- function(srchkey = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_ITIS_TERMS_BY_CMN_NAME <- paste("select t.tsn, t.name_usage, t.complete_name as combinedName, v.vernacular_name, a.taxon_author as author 
                           from taxonomic_units t 
                           left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id 
                           inner join vernaculars v on v.tsn = t.tsn and v.vernacular_name like ", paste("'", srchkey, "'", sep = ""), "order by t.tsn;")
		return(dbGetQuery(conn=sqlconn, query_ITIS_TERMS_BY_CMN_NAME))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getITISTermsFromCommonName"
		args <- list()
		if (!is.na(srchkey)) 
			args$srchKey <- srchkey
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		nodes <- getNodeSet(out, "//ax21:commonNames", namespaces = namespaces)
		comname <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:nameUsage", namespaces = namespaces)
		nameusage <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:scientificName", namespaces = namespaces)
		sciname <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		data.frame(comname = comname[-length(comname)], nameusage = nameusage, 
							 sciname = sciname, tsn = tsn)
	}
}

#' Get itis terms from scientific names
#' 
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' getitistermsfromscientificname(srchkey = "ursidae")
#' }
#' @export 
getitistermsfromscientificname <- function(srchkey = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_ITIS_TERMS_BY_SCI_NAME <- paste("select t.tsn, t.name_usage, t.complete_name as combinedName, v.vernacular_name, a.taxon_author as author 
                           from taxonomic_units t 
                           left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id 
                           left outer join vernaculars v on v.tsn = t.tsn 
                           where t.complete_name like ", paste("'", srchkey, "'", sep = ""), "order by t.tsn;")
		return(dbGetQuery(conn=sqlconn, query_ITIS_TERMS_BY_SCI_NAME))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getITISTermsFromScientificName"
		args <- list()
		if (!is.na(srchkey)) 
			args$srchKey <- srchkey
		message(paste(url, "?srchKey=", srchkey, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		nodes <- getNodeSet(out, "//ax21:commonNames", namespaces = namespaces)
		comname <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:nameUsage", namespaces = namespaces)
		nameusage <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:scientificName", namespaces = namespaces)
		sciname <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		data.frame(comname = comname, nameusage = nameusage, 
							 sciname = sciname, tsn = tsn)
	}
}

#' Get jurisdictional origin from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getjurisdictionaloriginfromtsn(tsn = 180543)
#' }
#' @export 
getjurisdictionaloriginfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_TAXON_JUR_ORIG_FROM_TSN <- paste("Select * from jurisdiction where tsn = ", tsn, "order by jurisdiction_value;")
		return(dbGetQuery(conn=sqlconn, query_TAXON_JUR_ORIG_FROM_TSN))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getJurisdictionalOriginFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("jurisdictionValue", "origin", "updateDate")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Get jurisdiction origin values
#' 
#' @import RCurl XML
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#'  queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @param sqlconn The sqlite3 connection object.
#' @examples \dontrun{
#' getjurisdictionoriginvalues()
#' }
#' @export 
getjurisdictionoriginvalues <- function(locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_ORIGIN_VALUES <- paste("select distinct jurisdiction_value, origin from jurisdiction order by jurisdiction_value, origin;")
		return(dbGetQuery(conn=sqlconn, query_ORIGIN_VALUES))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getJurisdictionalOriginValues"
		message(url)
		tt <- getURL(url)
		out <- xmlParse(tt)
		namespaces <- c(ax21 = "http://metadata.itis_service.itis.usgs.org/xsd")
		nodes <- getNodeSet(out, "//ax21:jurisdiction", namespaces = namespaces)
		jurisdiction <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:origin", namespaces = namespaces)
		origin <- sapply(nodes, xmlValue)
		data.frame(jurisdiction = jurisdiction, origin = origin)
	}
}

#' Get possible jurisdiction values
#' 
#' @import RCurl XML
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#'  queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @param sqlconn The sqlite3 connection object.
#' @examples \dontrun{
#' getjurisdictionvalues()
#' }
#' @export 
getjurisdictionvalues <- function(locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_JURISDICTION_VALUES <- paste("select distinct jurisdiction_value from jurisdiction order by jurisdiction_value;")
		return(dbGetQuery(conn=sqlconn, query_JURISDICTION_VALUES))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getJurisdictionValues"
		message(url)
		tt <- getURL(url)
		out <- xmlParse(tt)
		namespaces <- c(ax21 = "http://metadata.itis_service.itis.usgs.org/xsd")
		nodes <- getNodeSet(out, "//ax21:jurisdictionValues", 
												namespaces = namespaces)
		jurisdictionValues <- sapply(nodes, xmlValue)
		data.frame(jurisdictionValues = jurisdictionValues)
	}
}

#' Get kingdom names from tsn
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getkingdomnamefromtsn(tsn = 202385)
#' }
#' @export 
getkingdomnamefromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_KINGDOM_FROM_TSN <- paste("SELECT kingdom_name as KingdomName, kingdom_id as KingdomID from kingdoms where kingdom_id=(select kingdom_id from taxonomic_units where tsn=", tsn, ");")
		return(dbGetQuery(conn=sqlconn, query_KINGDOM_FROM_TSN))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getKingdomNameFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("kingdomId", "kingdomName", "tsn")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Get all possible kingdom names
#' 
#' @import RCurl XML
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#'  queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @param sqlconn The sqlite3 connection object.
#' @examples \dontrun{
#' getkingdomnamefromtsn(tsn = 202385)
#' }
#' @export 
getkingdomnames <- function(locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_KINGDOM_NAMES <- paste("select distinct k.*, t.tsn 
                  from kingdoms k  
                  inner join taxonomic_units t on t.unit_name1 = k.kingdom_name and t.parent_tsn=0 
                  order by k.kingdom_id;")
		return(dbGetQuery(conn=sqlconn, query_KINGDOM_NAMES))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getKingdomNames"
		message(url)
		tt <- getURL(url)
		out <- xmlParse(tt)
		namespaces <- c(ax23 = "http://metadata.itis_service.itis.usgs.gov/xsd")
		nodes <- getNodeSet(out, "//ax23:kingdomId", namespaces = namespaces)
		kingdomId <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax23:kingdomName", namespaces = namespaces)
		kingdomName <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax23:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		data.frame(kingdomId = kingdomId, kingdomName = kingdomName, tsn = tsn)
	}
}

#' Provides the date the ITIS database was last updated.
#' 
#' @import RCurl XML
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#'  queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @param sqlconn The sqlite3 connection object.
#' @examples \dontrun{
#' getlastchangedate()
#' }
#' @export 
getlastchangedate <- function(locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_UPDATE_DATE <- paste("select max(update_date) from taxonomic_units;")
		return(dbGetQuery(conn=sqlconn, query_UPDATE_DATE))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getLastChangeDate"
		message(url)
		tt <- getURL(url)
		out <- xmlParse(tt)
		namespaces <- c(ax23 = "http://metadata.itis_service.itis.usgs.gov/xsd")
		nodes <- getNodeSet(out, "//ax23:updateDate", namespaces = namespaces)
		sapply(nodes, xmlValue)
	}
}

#' Gets the unique LSID for the TSN, or an empty result if there is no match.
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getlsidfromtsn(tsn = 155166)
#' }
#' @export 
getlsidfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	url = "http://www.itis.gov/ITISWebService/services/ITISService/getLSIDFromTSN"
	args <- list()
	if (!is.na(tsn)) 
		args$tsn <- tsn
	message(paste(url, "?tsn=", tsn, sep = ""))
	tt <- getForm(url, .params = args, ..., curl = curl)
	xmlToList(xmlParse(tt))[[1]]
}

#' Returns a list of the other sources used for the TSN.
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getothersourcesfromtsn(tsn = 182662)
#' }
#' @export 
getothersourcesfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_TAXON_OTHER_SRC_FROM_TSN <- paste("Select '1' as sort_order, r.original_desc_ind, NULL as language, r.vernacular_name, o.* 
                              from reference_links r, other_sources o where r.doc_id_prefix = o.source_id_prefix 
                              and r.documentation_id = o.source_id and (r.vernacular_name = '' or r.vernacular_name is null) and r.tsn = ", tsn, "UNION Select '2' as sort_order,'N' AS original_desc_ind, v.language, v.vernacular_name, o.* 
                                  from vern_ref_links vr, other_sources o, vernaculars v 
                                  where vr.doc_id_prefix = o.source_id_prefix and vr.documentation_id = o.source_id 
                                  and vr.vern_id = v.vern_id and vr.tsn = v.tsn and v.tsn = ", tsn, "order by source, version, sort_order ;")
		return(dbGetQuery(conn=sqlconn, query_TAXON_OTHER_SRC_FROM_TSN))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getOtherSourcesFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("acquisitionDate", "name", "referredTsn", 
									"source", "sourceType", "updateDate", "version")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Returns the parent TSN for the entered TSN.
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getparenttsnfromtsn(tsn = 202385)
#' }
#' @export 
getparenttsnfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_PARENT_FROM_TSN <- paste("SELECT parent_tsn as ParentTSN from taxonomic_units where tsn=", tsn, ";", sep = "")
		temp <- dbGetQuery(conn=sqlconn, query_PARENT_FROM_TSN)
		temp2 <- cbind(temp, tsn)
		names(temp2) <- c("parentTsn", "tsn")
		return(temp2)
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getParentTSNFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax23 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("parentTsn", "tsn")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax23:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Returns a list of the pulications used for the TSN.
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getpublicationsfromtsn(tsn = 70340)
#' }
#' @export 
getpublicationsfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_TAXON_PUBS_FROM_TSN <- paste("Select '1' as sort_order, r.vernacular_name, NULL as language, r.original_desc_ind, p.* 
                        from reference_links r, publications p  
                        where r.doc_id_prefix = p.pub_id_prefix and r.documentation_id = p.publication_id  
                        and (r.vernacular_name ='' or r.vernacular_name is null) and r.tsn = ", tsn, "UNION Select '2' as sort_order, v.vernacular_name, v.language, 'N' as original_desc_ind, p.*  
                            From vern_ref_links vr, publications p, vernaculars v  
                            where vr.doc_id_prefix = p.pub_id_prefix and vr.documentation_id = p.publication_id and vr.vern_id = v.vern_id  
                            and vr.tsn = v.tsn and vr.tsn = ", tsn, "order by reference_author, actual_pub_date, title, publication_name, sort_order;")
		temp <- dbGetQuery(conn=sqlconn, query_TAXON_PUBS_FROM_TSN)
		toget <- c("actual_pub_date", "isbn", "issn", "listed_pub_date", 
							 "pages", "pub_comment", "publication_name", "pub_place", 
							 "publisher", "reference_author", "r.vernacular_name", 
							 "language", "title", "update_date")
		return(temp[, toget])
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getPublicationsFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("actualPubDate", "isbn", "issn", "listedPubDate", 
									"pages", "pubComment", "pubName", "pubPlace", "publisher", 
									"referenceAuthor", "name", "refLanguage", "referredTsn", 
									"title", "updateDate")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Provides a list of all the unique rank names contained in the database and 
#'  their kingdom and rank ID values.
#' 
#' @import RCurl XML
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#'  queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @param sqlconn The sqlite3 connection object.
#' @examples \dontrun{
#' getranknames()
#' }
#' @export 
getranknames <- function(locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_RANK_NAMES <- paste("select k.kingdom_name, t.kingdom_id, t.rank_name, t.rank_id 
               from taxon_unit_types t 
               inner join kingdoms k on t.kingdom_id = k.kingdom_id  
               order by t.kingdom_id, t.rank_id;")
		return(dbGetQuery(conn=sqlconn, query_RANK_NAMES))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getRankNames"
		message(url)
		tt <- getURL(url)
		out <- xmlParse(tt)
		namespaces <- c(ax25 = "http://metadata.itis_service.itis.usgs.org/xsd")
		nodes <- getNodeSet(out, "//ax25:kingdomName", namespaces = namespaces)
		kingdomName <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax25:rankId", namespaces = namespaces)
		rankId <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax25:rankName", namespaces = namespaces)
		rankName <- sapply(nodes, xmlValue)
		data.frame(kingdomName = kingdomName, rankId = rankId, 
							 rankName = rankName)
	}
}

#' Gets the partial ITIS record for the TSN in the LSID, found by comparing the 
#'  TSN in the search key to the TSN field. Returns an empty result set if 
#'  there is no match or the TSN is invalid.
#' 
#' @import RCurl XML
#' @param lsid lsid for a taxonomic group (numeric)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass 
#'  the returned value in here (avoids unnecessary footprint)
#' @examples \dontrun{
#' getrecordfromlsid(lsid = "urn:lsid:itis.gov:itis_tsn:180543")
#' }
#' @export 
getrecordfromlsid <- function(lsid = NA, ..., curl = getCurlHandle()) 
{
	url = "http://www.itis.gov/ITISWebService/services/ITISService/getRecordFromLSID"
	args <- list()
	if (!is.na(lsid)) 
		args$lsid <- lsid
	message(paste(url, "?lsid=", lsid, sep = ""))
	tt <- getForm(url, .params = args, ..., curl = curl)
	out <- xmlParse(tt)
	namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
	toget <- list("authorship", "genusPart", "infragenericEpithet", 
								"infraspecificEpithet", "lsid", "nameComplete", "nomenclaturalCode", 
								"rank", "rankString", "specificEpithet", "uninomial", 
								"tsn")
	xpathfunc <- function(x) {
		sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
											namespaces = namespaces), xmlValue)
	}
	df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
	names(df) <- toget
	df
}

#' Returns the review year for the TSN.
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getreviewyearfromtsn(tsn = 180541)
#' }
#' @export 
getreviewyearfromtsn <- function(tsn = NA, ..., curl = getCurlHandle()) 
{
	url = "http://www.itis.gov/ITISWebService/services/ITISService/getReviewYearFromTSN"
	args <- list()
	if (!is.na(tsn)) 
		args$tsn <- tsn
	message(paste(url, "?tsn=", tsn, sep = ""))
	tt <- getForm(url, .params = args, ..., curl = curl)
	out <- xmlParse(tt)
	namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
	toget <- list("rankId", "reviewYear", "tsn")
	xpathfunc <- function(x) {
		sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
											namespaces = namespaces), xmlValue)
	}
	df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
	names(df) <- toget
	df
}

#' Returns the scientific name for the TSN. Also returns the component parts 
#'    (names and indicators) of the scientific name.
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getscientificnamefromtsn(tsn = 531894)
#' }
#' @export 
getscientificnamefromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_SCI_NAME_BY_TSN_SRCH <- paste("select t.tsn, t.unit_ind1, t.unit_name1, t.unit_ind2, t.unit_name2, 
                         t.unit_ind3, t.unit_name3, t.unit_ind4, t.unit_name4, 
                         t.complete_name as combinedName, a.taxon_author as author, k.kingdom_name as kingdom 
                         from taxonomic_units t 
                         join kingdoms k on t.kingdom_id = k.kingdom_id 
                         left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id 
                         where t.tsn =", tsn, ";")
		temp <- dbGetQuery(conn=sqlconn, query_SCI_NAME_BY_TSN_SRCH)
		temp <- temp[, c("tsn", "unit_ind1", "unit_name1", "unit_name2", 
										 "unit_ind3", "unit_name3", "combinedName")]
		names(temp) <- c("tsn", "unitInd1", "unitName1", "unitName2", 
										 "unitInd3", "unitName3", "combinedName")
		return(temp[, c("combinedName", "unitInd1", "unitInd3", 
										"unitName1", "unitName2", "unitName3", "tsn")])
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getScientificNameFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("combinedName", "unitInd1", "unitInd3", 
									"unitName1", "unitName2", "unitName3", "tsn")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Returns a list of the synonyms (if any) for the TSN.
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getsynonymnamesfromtsn(tsn = 183671) # tsn not accepted
#' getsynonymnamesfromtsn(tsn = 526852) # tsn accepted
#' }
#' @export 
getsynonymnamesfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_SYNONYM_FROM_TSN <- paste("select t.tsn, t.complete_name as combinedName, a.taxon_author as author 
                     from taxonomic_units t 
                     left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id 
                     inner join synonym_links s on t.tsn = s.tsn and s.tsn_accepted = ", tsn, ";")
		temp <- dbGetQuery(conn=sqlconn, query_SYNONYM_FROM_TSN)
		return(data.frame(name = temp$combinedName, tsn = temp$tsn))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getSynonymNamesFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		nodes <- getNodeSet(out, "//ax21:sciName", namespaces = namespaces)
		if (length(sapply(nodes, xmlValue)) == 0) {
			name <- list("nomatch")
		}
		else {
			name <- sapply(nodes, xmlValue)
		}
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		if (length(sapply(nodes, xmlValue)) == 1) {
			tsn <- sapply(nodes, xmlValue)
		}
		else {
			tsn <- sapply(nodes, xmlValue)
			tsn <- tsn[-length(tsn)]
		}
		return(data.frame(name = name, tsn = tsn))
	}
}

#' Returns the author information for the TSN.
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gettaxonauthorshipfromtsn(tsn = 183671)
#' }
#' @export 
gettaxonauthorshipfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_TAXON_AUTHOR_FROM_TSN <- paste("select t.tsn as tsn, a.taxon_author as author, a.update_date as date 
                          from taxonomic_units t 
                          inner join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id and t.tsn = ", tsn, ";")
		temp <- dbGetQuery(conn=sqlconn, query_TAXON_AUTHOR_FROM_TSN)
		return(data.frame(authorship = temp$author, updateDate = temp$date, tsn = temp$tsn))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getTaxonAuthorshipFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("authorship", "updateDate", "tsn")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Returns the kingdom and rank information for the TSN.
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gettaxonomicranknamefromtsn(tsn = 202385)
#' }
#' @export 
gettaxonomicranknamefromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_RANK_FROM_TSN <- paste("SELECT t.kingdom_id, t.rank_id, t.tsn, r.rank_name, k.kingdom_name from taxonomic_units t 
                  inner join taxon_unit_types r on r.rank_id = t.rank_id 
                  inner join kingdoms k on k.kingdom_id = t.kingdom_id and t.tsn =", tsn, ";")
		temp <- dbGetQuery(conn=sqlconn, query_RANK_FROM_TSN)[1, ]
		return(data.frame(kingdomId = temp$kingdom_id, kingdomName = temp$kingdom_name, 
											rankId = temp$rank_id, rankName = temp$rank_name, tsn = temp$tsn))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getTaxonomicRankNameFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("kingdomId", "kingdomName", "rankId", "rankName", 
									"tsn")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Returns the usage information for the TSN.
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' gettaxonomicusagefromtsn(tsn = 526852)
#' }
#' @export 
gettaxonomicusagefromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_USAGE_FROM_TSN <- paste("SELECT tsn, name_usage from taxonomic_units where tsn = ", tsn, ";", sep = "")
		temp <- dbGetQuery(conn=sqlconn, query_USAGE_FROM_TSN)
		return(data.frame(taxonUsageRating = temp$name_usage, tsn = temp$tsn))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getTaxonomicUsageFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("taxonUsageRating", "tsn")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Retrieve accepted TSn(with accepted name)
#' 
#' @import RCurl XML
#' @param language A string containing the language. This is a language string, 
#'    not the international language code (character)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass 
#'  the returned value in here (avoids unnecessary footprint)
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#'  queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @param sqlconn The sqlite3 connection object.
#' @examples \dontrun{
#' gettsnbyvernacularlanguage("french")
#' }
#' @export 
gettsnbyvernacularlanguage <- function(language = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_TSNS_BY_LANGUAGE <- paste("select tsn, vernacular_name, vern_id from vernaculars where language like ", paste("'", language, "'", sep = ""), " order by tsn, vernacular_name;")
		temp <- dbGetQuery(conn=sqlconn, query_TSNS_BY_LANGUAGE)
		return(data.frame(comname = temp$vernacular_name, tsn = temp$tsn))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getTsnByVernacularLanguage"
		args <- list()
		if (!is.na(language)) 
			args$language <- language
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		nodes <- getNodeSet(out, "//ax21:commonName", namespaces = namespaces)
		comname <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		data.frame(comname = comname, tsn = tsn)
	}
}

#' Gets the TSN corresponding to the LSID, or an empty result if there is no match.
#' 
#' @param lsid lsid for a taxonomic group (numeric)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass 
#'  the returned value in here (avoids unnecessary footprint)
#' @examples \dontrun{
#' gettsnfromlsid(lsid = "urn:lsid:itis.gov:itis_tsn:28726")
#' gettsnfromlsid(lsid = "urn:lsid:itis.gov:itis_tsn:0")
#' }
#' @export 
gettsnfromlsid <- function(lsid = NA, ..., curl = getCurlHandle()) 
{
	url = "http://www.itis.gov/ITISWebService/services/ITISService/getTSNFromLSID"
	args <- list()
	if (!is.na(lsid)) 
		args$lsid <- lsid
	message(paste(url, "?lsid=", lsid, sep = ""))
	tt <- getForm(url, .params = args, ..., curl = curl)
	out <- xmlParse(tt)
	if (!is.na(suppressWarnings(as.numeric(xmlToList(out)[[1]])))) {
		suppressWarnings(as.numeric(xmlToList(out)[[1]]))
	}
	else {
		"invalid TSN"
	}
}

#' Returns the unacceptability reason, if any, for the TSN.
#' 
#' @inheritParams getcommentdetailfromtsn
#' @examples \dontrun{
#' getunacceptabilityreasonfromtsn(tsn = 183671)
#' }
#' @export 
getunacceptabilityreasonfromtsn <- function(tsn = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_UNACCEPT_FROM_TSN <- paste("Select tsn, unaccept_reason from taxonomic_units where tsn =", tsn, ";")
		temp <- dbGetQuery(conn=sqlconn, query_UNACCEPT_FROM_TSN)
		return(data.frame(tsn = temp$tsn, unacceptReason = temp$unaccept_reason))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getUnacceptabilityReasonFromTSN"
		args <- list()
		if (!is.na(tsn)) 
			args$tsn <- tsn
		message(paste(url, "?tsn=", tsn, sep = ""))
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		toget <- list("tsn", "unacceptReason")
		xpathfunc <- function(x) {
			sapply(getNodeSet(out, paste("//ax21:", x, sep = ""), 
												namespaces = namespaces), xmlValue)
		}
		df <- do.call(cbind, lapply(toget, as.data.frame(xpathfunc)))
		names(df) <- toget
		df
	}
}

#' Provides a list of the unique languages used in the vernacular table.
#' 
#' @import RCurl XML 
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#'  queries are run against the ITIS web API. lofcally=TRUE should be faster in almost all cases.
#' @param sqlconn The sqlite3 connection object.
#' @examples \dontrun{
#' getvernacularlanguages()
#' }
#' @export 
getvernacularlanguages <- function(locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_LANGUAGE_VALUES <- paste("select distinct language from vernaculars order by language;")
		temp <- dbGetQuery(conn=sqlconn, query_LANGUAGE_VALUES)
		return(data.frame(languageNames = temp$language))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/getVernacularLanguages"
		message(url)
		tt <- getURL(url)
		out <- xmlParse(tt)
		namespaces <- c(ax23 = "http://metadata.itis_service.itis.usgs.gov/xsd")
		nodes <- getNodeSet(out, "//ax23:languageNames", namespaces = namespaces)
		languageNames <- sapply(nodes, xmlValue)
		data.frame(languageNames = languageNames)
	}
}

#' Search for tsn by common name
#' 
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchbycommonname("american bullfrog")
#' searchbycommonname("ferret-badger")
#' searchbycommonname(srchkey="polar bear")
#' }
#' @export 
searchbycommonname <- function(srchkey = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_COMMON_NAME_CONTAINS_SRCH <- paste("select v.tsn as tsn, v.language as language, a.taxon_author as author, 
                              v.vernacular_name as commonName, t.complete_name as combinedName 
                              from vernaculars v 
                              inner join taxonomic_units t on v.tsn = t.tsn 
                              and v.vernacular_name like ", paste("'", srchkey, "'", sep = ""), "left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id ;")
		temp <- dbGetQuery(conn=sqlconn, query_COMMON_NAME_CONTAINS_SRCH)
		return(data.frame(comname = temp$commonName, lang = temp$language, tsn = temp$tsn))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/searchByCommonName"
		args <- list()
		if (!is.na(srchkey)) 
			args$srchKey <- srchkey
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		nodes <- getNodeSet(out, "//ax21:commonName", namespaces = namespaces)
		comname <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:language", namespaces = namespaces)
		lang <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		data.frame(comname = comname, lang = lang, tsn = tsn[-1])
	}
}

#' Search for tsn by common name beginning with 
#' 
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchbycommonnamebeginswith("inch")
#' }
#' @export 
searchbycommonnamebeginswith <- function(srchkey = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_COMMON_NAME_CONTAINS_SRCH <- paste("select v.tsn as tsn, v.language as language, a.taxon_author as author, 
                              v.vernacular_name as commonName, t.complete_name as combinedName 
                              from vernaculars v 
                              inner join taxonomic_units t on v.tsn = t.tsn 
                              and v.vernacular_name like ", paste("'", srchkey, "%'", sep = ""), "left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id ;")
		temp <- dbGetQuery(conn=sqlconn, query_COMMON_NAME_CONTAINS_SRCH)
		return(data.frame(comname = temp$commonName, lang = temp$language, tsn = temp$tsn))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/searchByCommonNameBeginsWith"
		args <- list()
		if (!is.na(srchkey)) 
			args$srchKey <- srchkey
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		nodes <- getNodeSet(out, "//ax21:commonName", namespaces = namespaces)
		comname <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:language", namespaces = namespaces)
		lang <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:sciName", namespaces = namespaces)
		data.frame(comname = comname, lang = lang, tsn = tsn[-length(tsn)])
	}
}

#' Search for tsn by common name ending with
#' 
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchbycommonnameendswith(srchkey="snake")
#' }
#' @export 
searchbycommonnameendswith <- function(srchkey = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_COMMON_NAME_CONTAINS_SRCH <- paste("select v.tsn as tsn, v.language as language, a.taxon_author as author, 
                              v.vernacular_name as commonName, t.complete_name as combinedName 
                              from vernaculars v 
                              inner join taxonomic_units t on v.tsn = t.tsn 
                              and v.vernacular_name like ", paste("'%", srchkey, "'", sep = ""), "left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id ;")
		temp <- dbGetQuery(conn=sqlconn, query_COMMON_NAME_CONTAINS_SRCH)
		return(data.frame(comname = temp$commonName, lang = temp$language, tsn = temp$tsn))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/searchByCommonNameEndsWith"
		args <- list()
		if (!is.na(srchkey)) 
			args$srchKey <- srchkey
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		nodes <- getNodeSet(out, "//ax21:commonName", namespaces = namespaces)
		comname <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:language", namespaces = namespaces)
		lang <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		data.frame(comname = comname, lang = lang, tsn = tsn[!nchar(tsn) == 
																												 	0])
	}
}

#' Search by scientific name
#' 
#' @inheritParams getanymatchcount
#' @param returnindex Retrun the index of each searched string with the resulting data.frame.
#' 		Useful in get_tsn to split results by searched string.
#' @examples \dontrun{
#' searchbyscientificname("Tardigrada")
#' 
#' # Using the web API, have to submit one at a time
#' ldply(c("oryza sativa","Chironomus riparius","Helianthus annuus","Quercus lobata"), searchbyscientificname)
#' 
#' # Using local search, can submit many in one vector
#' searchbyscientificname(srchkey=c("oryza sativa","Chironomus riparius","Helianthus annuus","Quercus lobata"), locally=TRUE, sqlconn=conn)
#' 
#' searchbyscientificname("Quercus douglasii", locally=TRUE, sqlconn=conn)
#' }
#' @export 
searchbyscientificname <- function(srchkey = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL, returnindex=FALSE)
{
	if (locally) {
		#
# 		query_SCI_NAME_LIKE <- paste("SELECT t.tsn as tsn, t.unit_name1, t.unit_name2, t.unit_name3, t.unit_name4,
#             t.unit_ind1, t.unit_ind2, t.unit_ind3, t.unit_ind4,
#         t.complete_name as combinedName, a.taxon_author as author, k.kingdom_name as kingdom
#         from taxonomic_units t
#         left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id
#         join kingdoms k on t.kingdom_id = k.kingdom_id
#         where t.complete_name like", paste("'%", srchkey, "%'", sep = ""), "order by tsn;")
		
# 		query_SCI_NAME_LIKE <- paste("SELECT 'searchstring' AS querystring, t.tsn as tsn, t.unit_name1, t.unit_name2, t.unit_name3, t.unit_name4,
# 			t.unit_ind1, t.unit_ind2, t.unit_ind3, t.unit_ind4,
# 			t.complete_name as combinedName, a.taxon_author as author, k.kingdom_name as kingdom
# 			from taxonomic_units t
# 			left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id
# 			join kingdoms k on t.kingdom_id = k.kingdom_id
# 			where", 
# 			paste(sapply(srchkey, function(x) paste("t.complete_name like ", paste("'%", x, "%'", sep = ""), sep = ""), USE.NAMES=FALSE),collapse=" OR "), ";")
# 		order by tsn
		
		query_SCI_NAME_LIKE <- paste("SELECT", 
paste("CASE", paste(sapply(srchkey, function(x) paste("WHEN t.complete_name LIKE ", paste("'%", x, "%'", sep = ""), " THEN ", paste0("'",x,"'"), sep = ""), USE.NAMES=FALSE),collapse=" "),"END AS querystring,"),
			"t.tsn as tsn, t.complete_name as combinedName
			FROM taxonomic_units t
			WHERE", 
			paste(sapply(srchkey, function(x) paste("t.complete_name like ", paste("'%", x, "%'", sep = ""), sep = ""), USE.NAMES=FALSE),collapse=" OR "), " order by querystring;")
		
		temp <- dbGetQuery(conn=sqlconn, query_SCI_NAME_LIKE)
		if(!returnindex)
			temp <- data.frame(combinedname = temp$combinedName, tsn = temp$tsn)
		return( temp )
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/searchByScientificName"
		args <- list()
		if (!is.na(srchkey)) 
			args$srchKey <- srchkey
		tt <- getForm(url, .params = args, curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd")
		nodes <- getNodeSet(out, "//ax21:combinedName", namespaces = namespaces)
		combinedname <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		data.frame(combinedname = combinedname, tsn = tsn)
	}
}

#' Search for any match
#' 
#' @inheritParams getanymatchcount
#' @examples \dontrun{
#' searchforanymatch(srchkey = 202385)
#' searchforanymatch(srchkey = "dolphin")
#' }
#' @export 
searchforanymatch <- function(srchkey = NA, ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_ANY_TSN_MATCH_SRCH <- paste("Select t.tsn as tsn, t.complete_name as combinedName, a.taxon_author as author, 
                       null as commonName, null as language, 'TSN' as  matchType 
                       from taxonomic_units t 
                       left join taxon_authors_lkp a on a.taxon_author_id = t.taxon_author_id 
                       where t.tsn= ", 
																			srchkey, ";")
		query_ANY_COMMON_MATCH_SRCH <- paste("Select t.tsn as tsn, t.complete_name as combinedName, v.vernacular_name as commonName, v.language as language,  
                          t.unit_name1, t.unit_name2, t.unit_name3, t.unit_name4, 
                          t.unit_ind1, t.unit_ind2, t.unit_ind3, t.unit_ind4, 
                          a.taxon_author as author, k.kingdom_name as kingdom 
                          from taxonomic_units t 
                          join kingdoms k on t.kingdom_id = k.kingdom_id 
                          left join taxon_authors_lkp a on t.taxon_author_id = a.taxon_author_id 
                          inner join vernaculars v 
                          on v.tsn = t.tsn and v.vernacular_name like ", paste("'%", srchkey, "%'", sep = ""), "order by tsn;")
		if (is.numeric(srchkey)) {
			temp <- dbGetQuery(conn=sqlconn, query_ANY_TSN_MATCH_SRCH)
			return(data.frame(combinedname = temp$combinedName, 
												tsn = temp$tsn))
		}
		else {
			temp <- dbGetQuery(conn=sqlconn, query_ANY_COMMON_MATCH_SRCH)
			return(data.frame(tsn = temp$tsn, combinedName = temp$combinedName, 
												commonName = temp$commonName))
		}
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/searchForAnyMatch"
		args <- list()
		if (!is.na(srchkey)) 
			args$srchKey <- srchkey
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd")
		nodes <- getNodeSet(out, "//ax21:commonName", namespaces = namespaces)
		comname <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:sciName", namespaces = namespaces)
		sciName <- sapply(nodes, xmlValue)
		list(comname = comname, tsn = tsn[-length(tsn)], sciName = sciName)
	}
}

#' Search for any matched page
#' 
#' @inheritParams getanymatchcount
#' @param pagesize An integer containing the page size (numeric)
#' @param pagenum An integer containing the page number (numeric)
#' @param ascend A boolean containing true for ascending sort order or false for descending (logical)
#' @examples \dontrun{
#' searchforanymatchpaged(202385, 100, 1, FALSE)
#' }
#' @export 
searchforanymatchpaged <- function(srchkey = NA, pagesize = NA, pagenum = NA, ascend = NA, 
																	 ..., curl = getCurlHandle(), locally = FALSE, sqlconn = NULL) 
{
	if (locally) {
		#
		query_ANY_MATCH_SEARCH <- paste("Select t.tsn as tsn, t.complete_name as combinedName, a.taxon_author as TaxonAuthor, v.vernacular_name as commonName, 
                            v.language as language, 'COMMON' as  matchType 
                         from taxonomic_units t 
                         join vernaculars v  on v.tsn = t.tsn and v.vernacular_name like ", paste("'", srchkey, "'", sep = ""), "left join taxon_authors_lkp a on a.taxon_author_id = t.taxon_author_id 
                         union Select t.tsn as tsn, t.complete_name as combinedName, a.taxon_author as TaxonAuthor, null as commonName, 
                                      null as language, 'SCIENTIFIC' as  matchType 
                             from taxonomic_units t  
                             left join taxon_authors_lkp a on a.taxon_author_id = t.taxon_author_id 
                             where t.complete_name like ", paste("'", srchkey, "'", sep = ""), "union Select t.tsn as tsn, t.complete_name as combinedName, a.taxon_author as TaxonAuthor, v.vernacular_name as commonName, 
                                      v.language as language, 'VERN' as  matchType 
                             from taxonomic_units t 
                             join vernaculars v  on v.tsn = t.tsn and t.complete_name like ", paste("'", srchkey, "'", sep = ""), "left join taxon_authors_lkp a on a.taxon_author_id = t.taxon_author_id 
                           order by t.complete_name, matchType;")
		temp <- dbGetQuery(conn=sqlconn, query_ANY_MATCH_SEARCH)
		return(data.frame(combinedname = temp$combinedName, tsn = temp$tsn))
	}
	else {
		url = "http://www.itis.gov/ITISWebService/services/ITISService/searchForAnyMatchPaged"
		args <- list()
		if (!is.na(srchkey)) 
			args$srchKey <- srchkey
		if (!is.na(pagesize)) 
			args$pageSize <- pagesize
		if (!is.na(pagenum)) 
			args$pageNum <- pagenum
		if (!is.na(ascend)) 
			args$ascend <- ascend
		tt <- getForm(url, .params = args, ..., curl = curl)
		out <- xmlParse(tt)
		namespaces <- c(namespaces <- c(ax21 = "http://data.itis_service.itis.usgs.gov/xsd"))
		nodes <- getNodeSet(out, "//ax21:commonName", namespaces = namespaces)
		comname <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:language", namespaces = namespaces)
		lang <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:tsn", namespaces = namespaces)
		tsn <- sapply(nodes, xmlValue)
		nodes <- getNodeSet(out, "//ax21:sciName", namespaces = namespaces)
		sciName <- sapply(nodes, xmlValue)
		list(comname = comname, lang = lang, tsn = tsn[-length(tsn)], 
				 sciName = sciName)
	}
}
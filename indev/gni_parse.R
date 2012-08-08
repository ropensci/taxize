# #' Pasre scientific names using the Global Names Index.
# #' 
# #' Uses the Global Names Index, see \link{http://gni.globalnames.org/} for information. 
# #' 
# #' @import stringr RJSONIO RCurl plyr
# #' @param names Scientic names (each quoted, in a vector).
# #' @param url Base url for the API; leave as is.
# #' @author Scott Chamberlain {myrmecocystus@@gmail.com}
# #' @return Data.frame of results.
# #' @seealso \code{\link{gnr}}, \code{\link{gnr_datasources}}, \code{\link{gni_details}}, and \code{\link{gni_search}}.
# #' @keywords globalnamesindex names taxonomy
# #' @examples \dontrun{
# #' gni_parse(names = c("Plantago minor","Homo sapiens"))
# #' }
# #' @export
# gni_parse <- function(names = NULL, url = "http://gni.globalnames.org/parsers.xml") 
# {
# # 	names <- paste(names, collapse="|")
# 	names <- paste(names, collapse="|")
# 	names <- gsub(" ", "+", names)
# 	url2 <- paste(url, "?names=", names, sep="")
# 	out <- xmlParse(getURL(url2))
# 	getNodeSet(out, "//scientific_names")
# 	xpathApply(out, "//scientificName")
# 	
# 	
# 	llply(out, function(x) c(x[["scientificName"]]))
# }
# # 	query <- compact(list(names = names))
# # 	out <- parsed_content( GET(url, query = query, ) )
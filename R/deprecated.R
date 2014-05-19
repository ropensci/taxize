#' Search Catalogue of Life for taxonomic classifications.
#'
#' THIS FUNCTION IS DEPRECATED.
#'
#' @import RCurl XML plyr
#' @param name The string to search for. Only exact matches found the name given
#'   	will be returned, unless one or wildcards are included in the search
#'   	string. An * (asterisk) character denotes a wildcard; a % (percentage)
#'    character may also be used. The name must be at least 3 characters long,
#'    not counting wildcard characters.
#' @param id The record ID of the specific record to return (only for scientific
#' 		names of species or infraspecific taxa)
#' @param format format of the results returned. Valid values are format=xml and
#' 		format=php; if the format parameter is omitted, the results are returned
#'   	in the default XML format. If format=php then results are returned as a
#'    PHP array in serialized string format, which can be converted back to an
#'    array in PHP using the unserialize command
#' @param start The first record to return. If omitted, the results are returned
#' 		from the first record (start=0). This is useful if the total number of
#' 		results is larger than the maximum number of results returned by a single
#' 		Web service query (currently the maximum number of results returned by a
#' 		single query is 500 for terse queries and 50 for full queries).
#' @param checklist The year of the checklist to query, if you want a specific
#' 		year's checklist instead of the lastest as default (numeric).
#' @details You must provide one of name or id. The other parameters (format
#' 		and start) are optional.
#' @return A list of data.frame's.
#' @export
#' @rdname col_classification-deprecated
#' @keywords internal
#' @examples \dontrun{
#' # A basic example
#' col_classification(name="Apis")
#'
#' # An example where there is no classification
#' col_classification(id=11935941)
#'
#' # Use a specific year's checklist
#' col_classification(name="Apis", checklist="2012")
#' col_classification(name="Apis", checklist="2009")
#'
#' # Pass in many names or many id's
#' out <- col_classification(name=c("Buteo","Apis","Accipiter","asdf"),
#'    checklist="2012")
#' out$Apis # get just the output you want
#' ldply(out) # or combine to one data.frame
#'
#' # Use get_colid to pass in ID directly
#' col_classification(id=get_colid(sciname='Puma concolor'))
#' }
col_classification <- function(name=NULL, id=NULL, format=NULL, start=NULL, checklist=NULL)
{
  .Deprecated(msg="This function is deprecated. See classification().")

  # url <- "http://www.catalogueoflife.org/col/webservice"
  # func <- function(x, y) {
  #   if(is.null(checklist)){NULL} else {
  #     cc <- match.arg(checklist, choices = c(2012, 2011, 2010, 2009, 2008, 2007))
  #     if (cc %in% c(2012, 2011, 2010)){
  #       url <- gsub("col", paste("annual-checklist/", cc, sep = ""), url)
  #     } else {
  #       url <- "http://webservice.catalogueoflife.org/annual-checklist/year/search.php"
  #       url <- gsub("year", cc, url)
  #     }
  #   }
  #
  #   args <- compact(list(name = x, id = y, format = format,
  #                        response = "full", start = start))
  #   out <- getForm(url, .params = args)
  #   tt <- xmlParse(out)
  #
  #   classif_id <- xpathSApply(tt, "//classification//id", xmlValue)
  #   classif_name <- xpathSApply(tt, "//classification//name", xmlValue)
  #   classif_rank <- xpathSApply(tt, "//classification//rank", xmlValue)
  #   data.frame(classif_name, classif_rank, classif_id, stringsAsFactors = FALSE)
  # }
  # safe_func <- plyr::failwith(NULL, func)
  # if(is.null(id)){
  #   temp <- llply(name, safe_func, y = NULL)
  #   names(temp) <- name
  # } else {
  #   temp <- llply(id, safe_func, x = NULL)
  #   names(temp) <- id
  # }
  # return(temp)
}


#' Retrieve the taxonomic hierarchy from given EOL taxonID.
#'
#' THIS FUNCTION IS DEPRECATED.
#'
#' @import httr plyr
#' @param taxonid the EOL page identifier (character)
#' @param common_names Return common names or not (defaults to returning them,
#'   	give commonnames=0 if not)
#' @param synonyms Return synonyms or not (defaults to returning them,
#' 		give synonyms=0 if not)
#' @param key Your EOL API key; loads from .Rprofile.
#' @param callopts Further args passed on to GET.
#' @details It's possible to return JSON or XML with the EOL API. However,
#' 		this function only returns JSON for now.
#' @return List or dataframe of results.
#' @export
#' @rdname eol_hierarchy-deprecated
#' @keywords internal
#' @examples \dontrun{
#' # Using get_eolid
#' eol_hierarchy(get_eolid(sciname='Poa annua'))
#' }
eol_hierarchy <- function(taxonid, common_names = NULL, synonyms = NULL,
                          key = NULL, callopts=list())
{
  .Deprecated(msg="This function is deprecated. See classification().")

  # # if NA input, return NA
  # if(is.na(taxonid)){ NA } else
  # {
  #   url = 'http://www.eol.org/api/hierarchy_entries/1.0/'
  #   key <- getkey(key, "eolApiKey")
  #   urlget <- paste(url, taxonid, '.json', sep="")
  #   args <- compact(list(common_names=common_names, synonyms=synonyms))
  #   tt <- GET(urlget, query=args, callopts)
  #   stop_for_status(tt)
  #   res <- content(tt)
  #   if(length(res$ancestors)==0){
  #     sprintf("No hierarchy information for %s", taxonid)
  #   } else
  #   {
  #     do.call(rbind.fill, lapply(res$ancestors, data.frame))[,c('taxonID','scientificName','taxonRank')]
  #   }
  # }
}


#' Return all synonyms for a taxon name with a given id.
#'
#' THIS FUNCTION IS DEPRECATED.
#'
#' @import httr plyr
#' @param id A Tropicos name ID
#' @param key Your Tropicos API key; loads from .Rprofile.
#' @param callopts Further args passed on to httr::GET
#' @return A data.frame giving the hierarchy.
#' @references \url{http://services.tropicos.org/help?method=GetNameHigherTaxaXml}
#' @export
#' @rdname tp_classification-deprecated
#' @keywords internal
#' @examples \dontrun{
#' tp_classification(id = 25509881)
#' tp_classification(id = c(25509881,2700851))
#' tp_classification(id = c(25509881,2700851), callopts=verbose())
#' }
tp_classification <- function(id=NULL, key=NULL, callopts=list())
{
  .Deprecated(msg="This function is deprecated. See classification().")

  # fun <- function(x){
  #   url <- sprintf('http://services.tropicos.org/Name/%s/HigherTaxa', x)
  #   key <- getkey(key, "tropicosApiKey")
  #   args <- compact(list(format='json', apikey=key))
  #   tt <- GET(url, query=args, callopts)
  #   stop_for_status(tt)
  #   out <- content(tt)
  #   if(names(out[[1]])[[1]] == "Error"){ data.frame(NameId=NA, ScientificName=NA, Rank=NA) } else {
  #     do.call(rbind.fill, lapply(out, data.frame))[,c('NameId','ScientificName','Rank')]
  #   }
  # }
  # tmp <- lapply(id, fun)
  # names(tmp) <- id
  # tmp
}

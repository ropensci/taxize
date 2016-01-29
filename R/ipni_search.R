#' Search for names in the International Plant Names Index (IPNI).
#'
#' Note: This data source is also provided in the Global Names Index (GNI)
#' (http://gni.globalnames.org/data_sources). The interface to the data is different among the two
#' services though.
#'
#' @export
#' @param family Family name to search on (Optional)
#' @param infrafamily Infrafamilial name to search on (Optional)
#' @param genus Genus name to search on (Optional)
#' @param infragenus Infrageneric name to search on (Optional)
#' @param species Species name to search on (Optional) - Note, this is the epithet, not the full
#' genus - epithet name combination.
#' @param infraspecies Infraspecies name to search on (Optional)
#' @param publicationtitle Publication name or abbreviation to search on. Again, replace any
#' spaces with a '+' (e.g. 'J.+Bot.') (Optional)
#' @param authorabbrev Author standard form to search on (publishing author, basionym author
#' or both - see below) (Optional)
#' @param includepublicationauthors TRUE (default) to include the taxon author in the search
#' or FALSE to exclude it
#' @param includebasionymauthors TRUE (default) to include the basionum author in the search
#' or FALSE to exclude it
#' @param geounit Country name or other geographical unit to search on (see the help pages
#' for more information and warnings about the use of this option) (Optional)
#' @param addedsince Date to search on in the format 'yyyy-mm-dd', e.g. 2005-08-01 for all
#' records added since the first of August, 2005. (see the help pages for more information and
#' warnings about the use of this option) (Optional. If supplied must be in format YYYY-MM-DD and
#' must be greater than or equal to 1984-01-01.)
#' @param modifiedsince Date to search on in the format 'yyyy-mm-dd', e.g. 2005-08-01 for
#' all records edited since the first of August, 2005. (See the help pages for more information
#' about the use of this option)  (Optional. If supplied must be in format YYYY-MM-DD and must
#' be greater than or equal to 1993-01-01.)
#' @param isapnirecord FALSE (default) to exclude records from the Australian Plant Name Index
#' @param isgcirecord FALSE (default) to exclude records from the Gray Cards Index
#' @param isikrecord FALSE (default) to exclude records from the Index Kewensis
#' @param ranktoreturn One of a few options to choose the ranks returned. See details.
#' @param output One of minimal (default), classic, short, or extended
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' (Optional). Default: returns all ranks.
#' @references \url{http://www.ipni.org/link_to_ipni.html}
#' @details
#' \code{rankToReturn} options:
#' \itemize{
#'  \item
#'  \item "all" - all records
#'  \item "fam" - family records
#'  \item "infrafam" - infrafamilial records
#'  \item "gen" - generic records
#'  \item "infragen" - infrageneric records
#'  \item "spec" - species records
#'  \item "infraspec" - infraspecific records
#' }
#' @return A data frame
#' @examples \dontrun{
#' ipni_search(genus='Brintonia', isapnirecord=TRUE, isgcirecord=TRUE, isikrecord=TRUE)
#' head(ipni_search(genus='Ceanothus'))
#' head(ipni_search(genus='Pinus', species='contorta'))
#'
#' # Different output formats
#' head(ipni_search(genus='Ceanothus'))
#' head(ipni_search(genus='Ceanothus', output='short'))
#' head(ipni_search(genus='Ceanothus', output='extended'))
#' }

ipni_search <- function(family=NULL, infrafamily=NULL, genus=NULL, infragenus=NULL, species=NULL,
  infraspecies=NULL, publicationtitle=NULL, authorabbrev=NULL, includepublicationauthors=NULL,
  includebasionymauthors=NULL, geounit=NULL, addedsince=NULL, modifiedsince=NULL,
  isapnirecord=NULL, isgcirecord=NULL, isikrecord=NULL, ranktoreturn=NULL, output="minimal",
  ...) {

  output <- match.arg(output, c('minimal','classic','short','extended'), FALSE)
  output_format <- sprintf('delimited-%s', output)
  url <- "http://www.ipni.org/ipni/advPlantNameSearch.do"
  args <- tc(list(output_format=output_format, find_family=family,
          find_infrafamily=infrafamily, find_genus=genus, find_infragenus=infragenus,
          find_species=species, find_infraspecies=infraspecies,
          find_publicationTitle=publicationtitle, find_authorAbbrev=authorabbrev,
          find_includePublicationAuthors=l2(includepublicationauthors),
          find_includebasionymauthors=l2(includebasionymauthors), find_geounit=geounit,
          find_addedSince=addedsince, find_modifiedSince=modifiedsince,
          find_isAPNIRecord=l2(isapnirecord), find_isGCIRecord=l2(isgcirecord),
          find_isIKRecord=l2(isikrecord), rankToReturn=ranktoreturn))
  tt <- GET(url, query = args, ...)
  if (tt$status_code > 200 || tt$headers$`content-type` != "text/plain;charset=UTF-8") {
    stop("No results", call. = FALSE)
  }
  res <- con_utf8(tt)
  if (nchar(res, keepNA = FALSE) == 0) {
    warning("No data found")
    df <- NA
  } else {
    df <- read.delim(text = res, sep = "%", stringsAsFactors = FALSE)
    names(df) <- gsub("\\.", "_", tolower(names(df)))
  }
  return( df )
}

l2 <- function(x) {
  if (!is.null(x)) {
    if (x) {
      "on"
    } else {
      "off"
    }
  } else{
    NULL
  }
}

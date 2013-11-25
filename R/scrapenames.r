#' Resolve names using Global Names Recognition and Discovery.
#' 
#' Uses the Global Names Recognition and Discovery service, see 
#' \url{http://gnrd.globalnames.org/}.
#' 
#' @import httr data.table plyr
#' @param url An encoded URL for a web page, PDF, Microsoft Office document, or 
#'    image file, see examples
#' @param file When using multipart/form-data as the content-type, a file may be sent.
#'    This should be a path to your file on your machine.
#' @param text Type: string. Text content; best used with a POST request, see 
#'    examples
#' @param engine (optional) Type: integer, Default: 0. Either 1 for TaxonFinder, 
#'    2 for NetiNeti, or 0 for both. If absent, both engines are used.
#' @param unique (optional) Type: boolean. If TRUE (default), 
#'    response has unique names without offsets.  
#' @param verbatim (optional) Type: boolean, If TRUE (default to FALSE), 
#'    response excludes verbatim strings. 
#' @param detect_language (optional) Type: boolean, When 
#'    TRUE (default), NetiNeti is not used if the language of incoming text is 
#'    determined not to be English. When 'false', NetiNeti will be used if requested. 
#' @param all_data_sources (optional) Type: bolean. Resolve found 
#'    names against all available Data Sources. 
#' @param data_source_ids (optional) Type: string. Pipe separated list of data 
#'    source ids to resolve found names against. See list of Data Sources.
#' @param callopts Further args passed to \code{\link[httr]{GET}}
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return A list of length two, first is metadata, second is the data as a data.frame.
#' @details One of url, file, or text must be specified - and only one of them. 
#' @export
#' @examples \dontrun{
#' # Get data from a website using its URL
#' scrapenames(url = 'http://en.wikipedia.org/wiki/Araneae')
#' 
#' # Scrape names from a pdf at a URL
#' scrapenames(url = 'http://www.mapress.com/zootaxa/2012/f/z03372p265f.pdf')
#' 
#' # With arguments
#' scrapenames(url = 'http://www.mapress.com/zootaxa/2012/f/z03372p265f.pdf', 
#' unique=TRUE)
#' scrapenames(url = 'http://www.mapress.com/zootaxa/2012/f/z03372p265f.pdf', all_data_sources=TRUE)
#'
#' # Get data from a file - NOT WORKING YET
#' scrapenames(file = '~/github/sac/joshwork/species_for_Scott.txt')
#'  
#' # Get data from text string as an R object
#' scrapenames(text='A spider named Pardosa moesta Banks, 1892')
#' }
scrapenames <- function(url = NULL, file = NULL, text = NULL, engine = NULL, 
  unique = NULL, verbatim = NULL, detect_language = NULL, all_data_sources = NULL,
  data_source_ids = NULL, callopts=list())
{
  method <- compact(list(url=url, file=file, text=text))
  if(length(method) > 1)
    stop("Only one of url, file, or text can be used")
  
  base <- "http://gnrd.globalnames.org/name_finder.json"
  args <- compact(list(url=url,text=text,engine=engine,unique=unique,
                       verbatim=verbatim, detect_language=detect_language, 
                       all_data_sources=all_data_sources, 
                       data_source_ids=data_source_ids))
  if(names(method) %in% c('url','text')){
    tt <- GET(base, query=args, callopts)
  } else
  {
    tt <- POST(base, query=args, multipart=TRUE, body = list(file=upload_file(file)))
  }
  stop_for_status(tt)
  out <- content(tt)
  
  if(!out$status == 303){
    stop("Woops, something went wrong")
  } else {
    token_url <- out$token_url
    st <- 303
    while(st == 303){
      dat <- GET(token_url)
      stop_for_status(dat)
      datout <- content(dat)
      st <- datout$status
    }
    meta <- datout[!names(datout) %in% c("names")]
    dd <- data.frame(rbindlist(datout$names))
    list(meta = meta, data = dd)
  }
}
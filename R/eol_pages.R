#' Search for pages in EOL database using a taxonconceptID.
#'
#' @import RCurl plyr jsonlite
#' @export
#' @param taxonconceptID The taxonconceptID (numeric), which is also the page
#' 		number.
#' @param iucn Include the IUCN Red List status object (Default: False)
#' @param images Limits the number of returned image objects (values 0 - 75)
#' @param videos Limits the number of returned video objects (values 0 - 75)
#' @param sounds Limits the number of returned sound objects (values 0 - 75)
#' @param maps Limits the number of returned map objects (values 0 - 75)
#' @param text Limits the number of returned text objects (values 0 - 75)
#' @param subject 'overview' (default) to return the overview text (if exists), a
#'    pipe | delimited list of subject names from the list of EOL accepted subjects
#'    (e.g. TaxonBiology, FossilHistory), or 'all' to get text in any subject. Always
#'    returns an overview text as a first result (if one exists in the given context).
#' @param licenses A pipe | delimited list of licenses or 'all' (default) to get objects
#'    under any license. Licenses abbreviated cc- are all Creative Commons licenses.
#'    Visit their site for more information on the various licenses they offer.
#' @param details Include all metadata for data objects. (Default: False)
#' @param common_names Return all common names for the page's taxon (Default: False)
#' @param synonyms Return all synonyms for the page's taxon (Default: False)
#' @param references Return all references for the page's taxon (Default: False)
#' @param vetted If 'vetted' is given a value of '1', then only trusted content will
#'    be returned. If 'vetted' is '2', then only trusted and unreviewed content will
#'    be returned (untrusted content will not be returned). The default is to return all
#'    content. (Default: False)
#' @param cache_ttl The number of seconds you wish to have the response cached.
#' @param key Your EOL API key; loads from .Rprofile, or you can specify the
#' 		key manually the in the function call.
#' @param callopts Further args passed on to RCurl::getForm.
#' @details It's possible to return JSON or XML with the EOL API. However,
#' 		this function only returns JSON for now.
#' @return JSON list object, or data.frame.
#' @examples \donttest{
#' (pageid <- eol_search('Pomatomus')$pageid[1])
#' (out <- eol_pages(taxonconceptID=pageid)$scinames)
#' }

eol_pages <- function(taxonconceptID, iucn=FALSE, images=0, videos=0, sounds=0,
  maps=0, text=0, subject='overview', licenses='all', details=FALSE,
  common_names=FALSE, synonyms=FALSE, references=FALSE, vetted=0,
  cache_ttl=NULL, key = NULL, callopts=list())
{
  iucn <- tolower(iucn)
  details <- tolower(details)
  common_names <- tolower(common_names)
  synonyms <- tolower(synonyms)
  references <- tolower(references)

  url <- 'http://eol.org/api/pages/1.0/'
	key <- getkey(key, "eolApiKey")
  args <- taxize_compact(list(iucn=iucn,images=images,videos=videos,sounds=sounds,
                       maps=maps,text=text,subject=subject,licenses=licenses,
                       details=details,common_names=common_names,synonyms=synonyms,
                       references=references,vetted=vetted,cache_ttl=cache_ttl))
  urlget <- paste(url, taxonconceptID, '.json', sep="")
#   tt <- GET(urlget, query=args, callopts)
#   stop_for_status(tt)
#   res <- content(tt, as = "text", encoding="utf-8")
  tt <- getForm(urlget, .params = args, .opts = callopts)
  stopifnot(attr(tt, "Content-Type")[[1]] == "application/json")
  stopifnot(attr(tt, "Content-Type")[[2]] == "utf-8")
  res <- jsonlite::fromJSON(tt, FALSE)

  scinames <- do.call(rbind.fill, lapply(res$taxonConcepts, data.frame, stringsAsFactors=FALSE))
  if(!is.null(scinames))
    names(scinames) <- tolower(names(scinames))
  syns <- parseeoldata('synonyms', res)
  names(syns) <- tolower(names(syns))
  vernac <- parseeoldata('vernacularNames', res)
  names(vernac) <- tolower(names(vernac))
  refs <- parseeoldata('references', res)
  names(refs) <- "references"
  dataobj <- parseeoldata('dataObjects', res)
  names(dataobj) <- tolower(names(dataobj))
  list(scinames=scinames, syns=syns, vernac=vernac, refs=refs, dataobj=dataobj)
}

parseeoldata <- function(x, y){
  xx <- y[[x]]
  if(length(xx)==0){  "no data" } else
  {
    tmp <- lapply(xx, data.frame)
    if(length(tmp)==1)
      tmp[[1]]
    else
      ldply(tmp)
  }
}

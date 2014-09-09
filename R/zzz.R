#' Function to get API key.
#'
#' Checks first to get key from your .Rprofile file for an API key with the
#' 		name 'tropicoskey'. If it is not found, the default key is used.
#'
#' @param x An API key, defaults to NULL.
#' @param service The API data provider, used to match to default guest key.
#' @examples \donttest{
#' getkey(service="tropicos")
#' getkey(service="eol")
#' }
#' @keywords internal
#' @export
getkey <- function(x = NULL, service) {
	if(is.null(x)){
	  keynames <- c("tropicosApiKey","eolApiKey","ubioApiKey","pmApiKey")
		service <- match.arg(service, keynames, several.ok=F)
		key <- getOption(service)
		if(is.null(key)){
			keys <- c("00ca3d6a-cbcc-4924-b882-c26b16d54446",
								"44f1a53227f1c0b6238a997fcfe7513415f948d2",
								"750bc6b8a550f2b9af1e8aaa34651b4c1111862a",
								"530763730")
			names(keys) <- keynames
			key <- keys[[service]]
			urls <- c("http://services.tropicos.org/help?requestkey",
								"http://eol.org/users/register",
								"http://www.ubio.org/index.php?pagename=form",
								"http://www.plantminer.com/")
			names(urls) <- keynames
			message(paste("Using default key: Please get your own API key at ",
										urls[service], sep=""))
		} else
			if(class(key)=="character"){key <- key} else
				{ stop("check your key input - it should be a character string") }
	} else
		{ key <- x }
	key
}

#' Replacement function for ldply that should be faster in all cases.
#'
#' @import plyr
#' @param x A list.
#' @param convertvec Convert a vector to a data.frame before rbind is called.
#' @export
#' @keywords internal
taxize_ldfast <- function(x, convertvec=FALSE){
  convert2df <- function(x){
    if(!inherits(x, "data.frame"))
      data.frame(rbind(x))
    else
      x
  }

  if(convertvec)
    do.call(rbind.fill, lapply(x, convert2df))
  else
    do.call(rbind.fill, x)
}

mssg <- function(v, ...) if(v) message(...)

taxize_compact <- function (l) Filter(Negate(is.null), l)

#' Lookup details for specific names in all taxonomies in GBIF.
#'
#'
#' This is a taxize version of the same function in the \code{rgbif} package so as to not have to
#' import rgbif and thus require GDAL binary installation.
#'
#' @import httr plyr jsonlite
#' @export
#' @param rank (character) Taxonomic rank. Filters by taxonomic rank as one of:
#'     CLASS, CULTIVAR, CULTIVAR_GROUP, DOMAIN, FAMILY, FORM, GENUS, INFORMAL,
#'   	INFRAGENERIC_NAME, INFRAORDER, INFRASPECIFIC_NAME, INFRASUBSPECIFIC_NAME,
#'     KINGDOM, ORDER, PHYLUM, SECTION, SERIES, SPECIES, STRAIN, SUBCLASS, SUBFAMILY,
#'     SUBFORM, SUBGENUS, SUBKINGDOM, SUBORDER, SUBPHYLUM, SUBSECTION, SUBSERIES,
#'     SUBSPECIES, SUBTRIBE, SUBVARIETY, SUPERCLASS, SUPERFAMILY, SUPERORDER,
#'     SUPERPHYLUM, SUPRAGENERIC_NAME, TRIBE, UNRANKED, VARIETY
#' @param datasetKey (character) Filters by the dataset's key (a uuid)
#' @param uuid (character) A uuid for a dataset. Should give exact same results as datasetKey.
#' @param key (numeric) A GBIF key for a taxon
#' @param name (character) Filters by a case insensitive, canonical namestring,
#'    e.g. 'Puma concolor'
#' @param data (character) Specify an option to select what data is returned. See Description
#'    below.
#' @param language (character) Language, default is english
#' @param sourceId (numeric) Filters by the source identifier. Not used right now.
#' @param shortname (character) A short name..need more info on this?
#' @param callopts Pass on options to httr::GET for more refined control of
#'    http calls, and error handling
#' @param limit Number of records to return
#' @param start Record number to start at
#' @references \url{http://www.gbif.org/developer/summary}
#' @return A list of length two. The first element is metadata. The second is
#' either a data.frame (verbose=FALSE, default) or a list (verbose=TRUE)

gbif_name_usage <- function(key=NULL, name=NULL, data='all', language=NULL, datasetKey=NULL, uuid=NULL,
                       sourceId=NULL, rank=NULL, shortname=NULL, start=NULL, limit=20, callopts=list())
{
  calls <- names(sapply(match.call(), deparse))[-1]
  calls_vec <- c("sourceId") %in% calls
  if(any(calls_vec))
    stop("Parameters not currently accepted: \n sourceId")


  args <- compact(list(language=language, name=name, datasetKey=datasetKey,
                       rank=rank, offset=start, limit=limit, sourceId=sourceId))
  data <- match.arg(data,
                    choices=c('all', 'verbatim', 'name', 'parents', 'children',
                              'related', 'synonyms', 'descriptions',
                              'distributions', 'images', 'references', 'species_profiles',
                              'vernacular_names', 'type_specimens', 'root'), several.ok=TRUE)

  # Define function to get data
  getdata <- function(x){
    if(!x == 'all' && is.null(key))
      stop('You must specify a key if data does not equal "all"')

    if(x == 'all' && is.null(key)){
      url <- 'http://api.gbif.org/v0.9/species'
    } else
    {
      if(x=='all' && !is.null(key)){
        url <- sprintf('http://api.gbif.org/v0.9/species/%s', key)
      } else
        if(x %in% c('verbatim', 'name', 'parents', 'children',
                    'related', 'synonyms', 'descriptions',
                    'distributions', 'images', 'references', 'species_profiles',
                    'vernacular_names', 'type_specimens')){
          url <- sprintf('http://api.gbif.org/v0.9/species/%s/%s', key, x)
        } else
          if(x == 'root'){
            url <- sprintf('http://api.gbif.org/v0.9/species/root/%s/%s', uuid, shortname)
          }
    }
    tt <- GET(url, query=args, callopts)
    stop_for_status(tt)
    assert_that(tt$headers$`content-type`=='application/json')
    res <- content(tt, as = 'text', encoding = "UTF-8")
    jsonlite::fromJSON(res, FALSE)
  }

  # Get data
  if(length(data)==1){ out <- getdata(data) } else
  { out <- lapply(data, getdata) }

  out
}

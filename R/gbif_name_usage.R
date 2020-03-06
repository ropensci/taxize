#' Lookup details for specific names in all taxonomies in GBIF.
#'
#'
#' This is a taxize version of the same function in the `rgbif` package so as to not have to
#' import rgbif and thus require GDAL binary installation.
#'
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
#' @param ... Curl options passed on to [crul::HttpClient]
#' @param limit Number of records to return
#' @param start Record number to start at
#' @references <https://www.gbif.org/developer/summary>
#' @return A list of length two. The first element is metadata. The second is
#' either a data.frame (verbose=FALSE, default) or a list (verbose=TRUE)

gbif_name_usage <- function(key=NULL, name=NULL, data='all', language=NULL, datasetKey=NULL, uuid=NULL,
                            sourceId=NULL, rank=NULL, shortname=NULL, start=NULL, limit=20, ...)
{
  calls <- names(sapply(match.call(), deparse))[-1]
  calls_vec <- c("sourceId") %in% calls
  if(any(calls_vec))
    stop("Parameters not currently accepted: \n sourceId")


  args <- tc(list(language=language, name=name, datasetKey=datasetKey,
                  rank=rank, offset=start, limit=limit, sourceId=sourceId))
  data <- match.arg(data,
                    choices=c('all', 'verbatim', 'name', 'parents', 'children',
                              'related', 'synonyms', 'descriptions',
                              'distributions', 'images', 'references', 'species_profiles',
                              'vernacular_names', 'type_specimens', 'root'), several.ok=TRUE)

  # Define function to get data
  getdata <- function(x){
    if (!x == 'all' && is.null(key)) {
      stop('You must specify a key if data does not equal "all"', call. = FALSE)
    }

    if (x == 'all' && is.null(key)) {
      url <- 'https://api.gbif.org/v1/species'
    } else {
      if (x == 'all' && !is.null(key)) {
        url <- sprintf('https://api.gbif.org/v1/species/%s', key)
      } else
        if (x %in% c('verbatim', 'name', 'parents', 'children',
                     'related', 'synonyms', 'descriptions',
                     'distributions', 'images', 'references', 'species_profiles',
                     'vernacular_names', 'type_specimens')) {
          url <- sprintf('https://api.gbif.org/v1/species/%s/%s', key, x)
        } else
          if (x == 'root') {
            url <- sprintf('https://api.gbif.org/v1/species/root/%s/%s', uuid, shortname)
          }
    }
    cli <- crul::HttpClient$new(url = url, headers = tx_ual, opts = list(...))
    res <- cli$get(query = args)
    res$raise_for_status()
    stopifnot(res$response_headers$`content-type` == 'application/json')
    jsonlite::fromJSON(res$parse("UTF-8"), FALSE)
  }

  # Get data
  if (length(data) == 1) {
    getdata(data)
  } else {
    lapply(data, getdata)
  }
}

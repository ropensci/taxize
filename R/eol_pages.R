#' Search for pages in EOL database using a taxonconceptID.
#'
#' @export
#' @param taxonconceptID (numeric) a taxonconceptID, which is also the page
#' number
#' @param images_per_page (integer) number of returned image objects (0-75)
#' @param images_page (integer) images page
#' @param videos_per_page (integer) number of returned video objects (0-75)
#' @param videos_page (integer) videos page
#' @param sounds_per_page (integer) number of returned sound objects (0-75)
#' @param sounds_page (integer) sounds page
#' @param maps_per_page (integer) number of returned map objects (0-75)
#' @param maps_page (integer) maps page
#' @param texts_per_page (integer) number of returned text objects (0-75)
#' @param texts_page (integer) texts page
#' @param subjects 'overview' (default) to return the overview text
#' (if exists), a pipe | delimited list of subject names from the list of EOL
#' accepted subjects (e.g. TaxonBiology, FossilHistory), or 'all' to get text
#' in any subject. Always returns an overview text as a first result (if one
#' exists in the given context).
#' @param licenses A pipe | delimited list of licenses or 'all' (default) to
#' get objects under any license. Licenses abbreviated cc- are all Creative
#' Commons licenses. Visit their site for more information on the various
#' licenses they offer.
#' @param details Include all metadata for data objects. (Default: `FALSE`)
#' @param common_names Return all common names for the page's taxon
#' (Default: `FALSE`)
#' @param synonyms Return all synonyms for the page's taxon
#' (Default: `FALSE`)
#' @param references Return all references for the page's taxon
#' (Default: `FALSE`)
#' @param taxonomy (logical) Whether to return any taxonomy details from
#' different taxon hierarchy providers, in an array named `taxonconcepts`
#' (Default: `TRUE`)
#' @param vetted If 'vetted' is given a value of '1', then only trusted
#' content will be returned. If 'vetted' is '2', then only trusted and
#' unreviewed content will be returned (untrusted content will not be returned).
#' The default is to return all content. (Default: `FALSE`)
#' @param cache_ttl The number of seconds you wish to have the response cached.
#' @param ... Curl options passed on to [crul::HttpClient]
#' @details It's possible to return JSON or XML with the EOL API. However,
#' this function only returns JSON for now.
#' @return JSON list object, or data.frame.
#' @examples \dontrun{
#' (pageid <- eol_search('Pomatomus')$pageid[1])
#' x <- eol_pages(taxonconceptID = pageid)
#' x
#' x$scinames
#'
#' z <- eol_pages(taxonconceptID = pageid, synonyms = TRUE)
#' z$synonyms
#'
#' z <- eol_pages(taxonconceptID = pageid, common_names = TRUE)
#' z$vernacular
#' }

eol_pages <- function(taxonconceptID, images_per_page=NULL, images_page=NULL,
  videos_per_page=NULL, videos_page=NULL, sounds_per_page=NULL, sounds_page=NULL,
  maps_per_page=NULL, maps_page=NULL, texts_per_page=NULL, texts_page=NULL,
  subjects='overview', licenses='all', details=FALSE,
  common_names=FALSE, synonyms=FALSE, references=FALSE, taxonomy=TRUE, vetted=0,
  cache_ttl=NULL, ...) {

  details <- tolower(details)
  common_names <- tolower(common_names)
  synonyms <- tolower(synonyms)
  references <- tolower(references)

	args <- tc(list(images_per_page=images_per_page, images_page=images_page,
    videos_per_page=videos_per_page, videos_page=videos_page,
    sounds_per_page=sounds_per_page, sounds_page=sounds_page,
    maps_per_page=maps_per_page, maps_page=maps_page,
    texts_per_page=texts_per_page, texts_page=texts_page,
    subjects=subjects, licenses=licenses, details=details,
    common_names=common_names, synonyms=synonyms, references=references,
    taxonomy=taxonomy, vetted=vetted, cache_ttl=cache_ttl))
  cli <- crul::HttpClient$new(
    url = file.path(eol_url("pages"), paste0(taxonconceptID, ".json")),
    headers = tx_ual,
    opts = list(...)
  )
  res <- cli$get(query = argsnull(args))
  res$raise_for_status()
  tt <- res$parse("UTF-8")
  stopifnot(res$response_headers$`content-type` ==
    "application/json; charset=utf-8")
  res <- jsonlite::fromJSON(tt, FALSE)

  # scinames <- lapply(res$taxonConcept$taxonConcepts, function(z) {
  scinames <- lapply(res$taxonConcept$taxonConcepts, function(z) {
    z[vapply(z, class, "") == "NULL"] <- NA_character_
    z
  })
  scinames <- dt2tibble(scinames)
  if (!is.null(scinames)) {
    names(scinames) <- tolower(names(scinames))
    if ("taxonrank" %in% names(scinames)) {
      scinames$taxonrank <- tolower(scinames$taxonrank)
    }
    scinames$name <- NULL
  }
  syns <- parseeoldata('synonyms', res)
  vernac <- parseeoldata('vernacularNames', res)
  refs <- parseeoldata('references', res)
  dataobj <- parseeoldata('dataObjects', res)
  list(scinames=scinames, synonyms=syns, vernacular=vernac, refs=refs,
    data_objects=dataobj)
}

parseeoldata <- function(x, y) {
  xx <- y$taxonConcept[[x]]
  if (length(xx) == 0) {
    NULL
  } else {
    tmp <- lapply(xx, data.frame)
    if (length(tmp) == 1) {
      tmp2 <- tmp[[1]]
    } else {
      tmp2 <- dt2df(tmp, idcol = FALSE)
    }
    names(tmp2) <- tolower(names(tmp2))
    tmp2
  }
}

#' Search Australian Plant Names Index
#' @export
#' @name apni
#' @param q (character) name to query
#' @param ... Further args passed on to [crul::HttpClient].
#' @return a list with slots for metadata (`meta`) with list of response
#' attributes, and data (`data`) with a data.frame of results
#' @references https://biodiversity.org.au/nsl/docs/main.html
#' @family apni
#' @examples \dontrun{
#' x <- apni_search(q = "Pinus")
#' x
#' x$accepted_names
#' x$synonyms
#' x$synonyms$acceptedNameUsage
#' x$meta$perPage
#' 
#' apni_search(q = "Acacia")
#' 
#' # Acacia dealbata 
#' apni_classification(id = 61294)
#' # Acacia 
#' apni_classification(id = 56859)
#' 
#' apni_family(id = 158548)
#' 
#' acceptable_names(q = 'Poa fax')
#' acceptable_names(q = 'Poa fa')
#' acceptable_names(q = 'Poa')
#' }

#' @export
#' @rdname apni
apni_search <- function(q, ...) {
  assert(q, "character")
  z <- apni_GET(file.path(apni_base(), "nsl/services/api/name/taxon-search"),
    list(q = q, tree = "APC"), ...)
  json <- jsonlite::fromJSON(z$parse("UTF-8"))
  list(
    accepted_names = json$records$acceptedNames, 
    synonyms = json$records$synonyms
  )
}

#' @export
#' @rdname apni
apni_classification <- function(id, ...) {
  assert(id, c('numeric', 'integer'))
  x <- apni_GET(file.path(apni_base(TRUE),
    sprintf("name/apni/%s/api/branch", id)), args = list(), ...)
  txt <- x$parse("UTF-8")
  json <- jsonlite::fromJSON(txt, FALSE)
  bb <- json$branch
  branch <- dt2tibble(lapply(bb, function(z) {
    list(
      name = z$fullName,
      rank = z$rank$name,
      rank_order = z$rank$sortOrder,
      type = z$type,
      status = z$status,
      parent = z$parent$nameElement,
      author = z$author$name
    )
  }))
  list(
    name = json$name$nameElement,
    link = json$name$`_links`$permalink$link,
    hierarchy = branch
  )
}

#' @export
#' @rdname apni
apni_family <- function(id, ...) {
  assert(id, c('numeric', 'integer'))
  x <- apni_GET(file.path(apni_base(TRUE),
    sprintf("name/apni/%s/api/family", id)), args = list(), ...)
  txt <- x$parse("UTF-8")
  json <- jsonlite::fromJSON(txt, FALSE)
  bb <- json$familyName$instances
  branch <- dt2tibble(lapply(bb, function(z) {
    list(
      type = z$instanceType,
      link = z$`_links`$permalink$link,
      pages = z$page,
      name = z$name,
      protologue = z$protologue,
      citation = z$citation,
      auth_year = z$citationAuthYear
    )
  }))
  list(
    name = json$name$nameElement,
    link = json$name$`_links`$permalink$link,
    instances = branch
  )
}

#' @export
#' @rdname apni
acceptable_names <- function(q, ...) {
  assert(q, 'character')
  x <- apni_GET(file.path(apni_base(),
    "nsl/services/api/name/acceptable-name.json"),
    args = list(name = q))
  txt <- x$parse("UTF-8")
  json <- jsonlite::fromJSON(txt, FALSE)
  dt2tibble(lapply(json$names, function(w) {
    list(
      name_type = w$nameType,
      status = w$nameStatus,
      rank = w$nameRank,
      name_full = w$fullName,
      name_simple = w$simpleName
    )
  }))
}

# Helpers ============
apni_GET <- function(url, args, ...){
  con <- crul::HttpClient$new(url = url,
    headers = tx_ual, opts = list(...))
  tt <- con$get(query = argsnull(args))
  tt$raise_for_status()
  return(tt)
}

apni_base <- function(id = FALSE) {
  scheme = "https://"
  uri <- if (id) "id.biodiversity.org.au" else "biodiversity.org.au"
  paste0(scheme, uri)
}

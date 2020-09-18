#' Search Australian Plant Names Index
#' @export
#' @name apni
#' @param q (character) name to query
#' @param ... Further args passed on to [crul::verb-GET].
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
#' # children
#' x <- apni_children(id = 2902806)
#' x
#' x$children
#' x$children$id
#' 
#' # family
#' apni_family(id = 158548)
#' 
#' # acceptable names
#' apni_acceptable_names(q = 'Poa fax')
#' apni_acceptable_names(q = 'Poa fa')
#' apni_acceptable_names(q = 'Poa')
#' apni_acceptable_names(q = 'Acacia')
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
apni_sugggest <- function(q, ...) {
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
apni_children <- function(id, ...) {
  assert(id, c('numeric', 'integer'))
  x <- apni_GET(file.path(apni_base(),
    sprintf("nsl/services/rest/taxon/apni/%s.json", id)),
    args = list(), ...)
  txt <- x$parse("UTF-8")
  json <- jsonlite::fromJSON(txt, FALSE)
  childs <- json$treeElement$children
  children <- dt2tibble(lapply(childs, function(z) {
    html <- xml2::read_html(z$displayHtml)
    name <- xml2::xml_text(xml2::xml_find_all(html, "//scientific//element"))
    name <- strtrim(Reduce(paste, name, ""))
    name_id <- xml2::xml_attr(xml2::xml_find_first(html, "//name"), "data-id")
    name_auth <- xml2::xml_attr(xml2::xml_find_all(html, "//authors//author"), "title")
    list(
      id = name_id, 
      name = name,
      authority = name_auth,
      link_element = z$elementLink,
      link_name = z$nameLink,
      link_instance = z$instanceLink
    )
  }))
  # drop self
  children <- children[children$name != json$treeElement$simpleName,]
  list(
    name = json$treeElement$simpleName,
    link = json$treeElement$`_links`$elementLink,
    children = children
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
apni_acceptable_names <- function(q, ...) {
  assert(q, 'character')
  x <- apni_GET(file.path(apni_base(),
    "nsl/services/api/name/acceptable-name.json"),
    args = list(name = q), ...)
  txt <- x$parse("UTF-8")
  json <- jsonlite::fromJSON(txt, FALSE)
  dt2tibble(lapply(json$names, function(w) {
    list(
      name_type = w$nameType,
      status = w$nameStatus,
      rank = tolower(w$nameRank),
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

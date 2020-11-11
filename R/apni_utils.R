#' Search Australian Plant Names Index
#' @export
#' @name apni-search
#' @param q (character) one or more names to query. vectorized
#' @param ... curl options passed on to [crul::verb-GET].
#' @return a list of lists, each with a slot for metadata (`meta`)
#' with list of response attributes, and data (`data`) with a
#' data.frame of results
#' @references https://biodiversity.org.au/nsl/docs/main.html
#' @family apni
#' @details `apni_search()` uses the `nsl/services/api/name/taxon-search`
#' route, and searches via exact match mor or less. `apni_suggest()` uses
#' the `nsl/services/suggest/acceptableName` route and does a fuzzy search.
#' `apni_acceptable_names()` uses the `nsl/services/api/name/acceptable-name`
#' route and does a similarly fuzzy search to suggest;
#' `apni_acceptable_names()` is used in [get_apni()]
#' @examples \dontrun{
#' x <- apni_search(q = "Pinus")
#' x
#' x[[1]]$accepted_names
#' x[[1]]$synonyms
#' x[[1]]$synonyms$acceptedNameUsage
#' x[[1]]$meta$perPage
#' apni_search(q = c("Acacia", "Pinus"))
#' 
#' apni_suggest(c("Pinus clau", "Acacia ab"))
#' apni_acceptable_names(c("Pinus clau", "Acacia ab"))
#' apni_suggest("Acacia ab")
#' apni_acceptable_names("Acacia ab")
#' 
#' # acceptable names
#' apni_acceptable_names(q = 'Poa fax')
#' apni_acceptable_names(q = 'Poa fa')
#' apni_acceptable_names(q = 'Poa')
#' apni_acceptable_names(q = 'Acacia')
#' }

#' @export
#' @rdname apni-search
apni_search <- function(q, ...) {
  assert(q, "character")
  lapply(q, apni_search_one, ...)
}
apni_search_one <- function(q, ...) {
  z <- apni_GET(file.path(apni_base(), "nsl/services/api/name/taxon-search"),
    list(q = q, tree = "APC"), ...)
  json <- jsonlite::fromJSON(z$parse("UTF-8"))
  list(
    accepted_names = json$records$acceptedNames, 
    synonyms = json$records$synonyms
  )
}

#' @export
#' @rdname apni-search
apni_suggest <- function(q, ...) {
  assert(q, "character")
  lapply(q, apni_suggest_one, ...)
}
apni_suggest_one <- function(q, ...) {
  z <- apni_GET(file.path(apni_base(), "nsl/services/suggest", "acceptableName"),
    list(term = q, tree = "APC"), ...)
  res <- jsonlite::fromJSON(z$parse("UTF-8"))
  if (length(res) == 0) return(data.frame())
  lks <- res$link
  w <- crul::Async$new(lks)$get()
  cbind(res, dt2df(lapply(w, function(b) {
    tmp <- jsonlite::fromJSON(b$parse("UTF-8"))
    data.frame(rank=tolower(tmp$rank$name), 
      status=tmp$status, family=tmp$family$nameElement)
  }), FALSE))
}

#' @export
#' @rdname apni-search
apni_acceptable_names <- function(q, ...) {
  assert(q, 'character')
  lapply(q, apni_acceptable_names_one, ...)
}
apni_acceptable_names_one <- function(q, ...) {
  x <- apni_GET(file.path(apni_base(),
    "nsl/services/api/name/acceptable-name.json"),
    args = list(name = q), ...)
  txt <- x$parse("UTF-8")
  json <- jsonlite::fromJSON(txt, FALSE)
  dt2tibble(lapply(json$names, function(w) {
    list(
      id = basename(w$`_links`$permalink$link),
      name_type = w$nameType,
      status = w$nameStatus,
      rank = tolower(w$nameRank),
      name_full = w$fullName,
      name_simple = w$simpleName
    )
  }))
}

#' Australian Plant Names Index classification
#' @export
#' @param id (numeric/character) one or more APNI ids
#' @param ... curl options passed on to [crul::verb-GET].
#' @return a list with slots for name (`name`), link (`link`),
#' and the taxonomic classification (`hierarchy`)
#' @references https://biodiversity.org.au/nsl/docs/main.html
#' @family apni
#' @examples \dontrun{
#' # Acacia dealbata 
#' apni_classification(id = 61294)
#' # Acacia 
#' apni_classification(id = 56859)
#' }
apni_classification <- function(id, ...) {
  assert(id, c('numeric', 'integer'))
  lapply(id, apni_classification1, ...)
}
apni_classification1 <- function(id, ...) {
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

#' Australian Plant Names Index children
#' @export
#' @param id (numeric/character) one or more APNI ids
#' @param ... curl options passed on to [crul::verb-GET].
#' @return a list with slots for name (`name`), link (`link`),
#' and the taxonomic children (`children`)
#' @references https://biodiversity.org.au/nsl/docs/main.html
#' @family apni
#' @examples \dontrun{
#' x <- apni_children(id = 2902806)
#' x
#' x$children
#' x$children$id
#' }
apni_children <- function(id, ...) {
  assert(id, c('numeric', 'integer'))
  lapply(id, apni_children1, ...)
}
apni_children1 <- function(id, ...) {
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


#' Australian Plant Names Index family information
#' 
#' Returns the family of the name according to the APNI or
#' 'Name classification'. The Name classification may be different
#' to other classifications such as APC.
#' 
#' @export
#' @param id (numeric/character) one or more APNI ids
#' @param ... curl options passed on to [crul::verb-GET].
#' @return a list with slots for name (`name`), link (`link`),
#' and the instances (`instances`)
#' @references https://biodiversity.org.au/nsl/docs/main.html
#' @family apni
#' @examples \dontrun{
#' apni_family(id = 158548)
#' }
apni_family <- function(id, ...) {
  assert(id, c('numeric', 'integer'))
  lapply(id, apni_family_one, ...)
}
apni_family_one <- function(id, ...) {
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

#' Australian Plant Names Index ID lookup
#' 
#' Get APNI metadata for an APNI ID or check that an ID exists
#' 
#' @export
#' @param id (numeric/character) one or more APNI ids
#' @param ... curl options passed on to [crul::verb-GET].
#' @return for `apni_id()` a list with lots of data. for
#' `apni_id_exists()` a boolean (`FALSE` if the id does not exist).
#' `apni_id_exists()` uses `apni_id()` internally
#' @references https://biodiversity.org.au/nsl/docs/main.html
#' @family apni
#' @examples \dontrun{
#' # fetch an id
#' apni_id(56859)
#' # apni_id(3333333) # does not exist, fails
#' 
#' # check that an id exists within APNI
#' apni_id_exists(56859) # exists
#' apni_id_exists(3333333) # does not exist
#' apni_id_exists(c(56859, 3333333))
#' }
apni_id <- function(id, ...) {
  assert(id, c('character', 'numeric', 'integer'))
  lapply(id, apni_id_one, ...)
}
apni_id_one <- function(id, ...) {
  assert(id, c('character', 'numeric', 'integer'))
  x <- apni_GET(file.path(apni_base(), "nsl/services/rest/name/apni", id),
    list(), ...)
  jsonlite::fromJSON(x$parse("UTF-8"))
}

#' @export
#' @rdname apni_id
apni_id_exists <- function(id) {
  vapply(id, apni_id_exists1, logical(1))
}
apni_id_exists1 <- function(id) {
  z <- tryCatch(apni_id(id), error = function(e) e)
  !inherits(z, "error")
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

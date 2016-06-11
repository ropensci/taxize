#' Wikipedia taxonomy data
#'
#' @export
#' @param x (character) a taxonomic name
#' @param property (character) a property id, e.g., P486
#' @param ... curl options passed on to \code{\link[httr]{GET}}
#' @param language (character) two letter language code
#' @param limit (integer) records to return. Default: 10
#' @examples \dontrun{
#' wiki_data("Poa annua")
#' wiki_data("Mimulus alsinoides")
#' wiki_data("Mimulus foliatus")
#' wiki_data("Mimulus foliatus", property = "P846")
#' wiki_data("Mimulus foliatus", property = c("P846", "P815"))
#'
#' wiki_data(get_wiki_id("Mimulus foliatus"))
#' }
wiki_data <- function(x, property = NULL, ...) {
  UseMethod("wiki_data")
}

#' @export
wiki_data.wiki_id <- function(x, property = NULL, ...) {
  data_wiki(x, property = property, ...)
}

#' @export
wiki_data.default <- function(x, property = NULL, ...) {
  x <- WikidataR::find_item(search_term = x, ...)
  if (length(x) == 0) stop("no results found", call. = FALSE)
  data_wiki(x[[1]]$id, property = property, ...)
}

#' @export
#' @rdname wiki_data
get_wiki_id <- function(x, language = "en", limit = 10, ...) {
  x <- WikidataR::find_item(search_term = x, language = language, limit = limit, ...)
  x <- if (length(x) == 0) NA else x[[1]]$id
  structure(x, class = "wiki_id")
}

data_wiki <- function(x, property = NULL, ...) {
  xx <- WikidataR::get_item(x, ...)

  if (is.null(property)) {
    claims <- create_claims(xx$claims)
  } else{
    cl <- Filter(function(x) x$mainsnak$property %in% property, xx$claims)
    if (length(cl) == 0) stop("No matching properties", call. = FALSE)
    claims <- create_claims(cl)
  }

  list(
    labels = dt_df(xx$labels),
    descriptions = dt_df(xx$descriptions),
    aliases = dt_df(xx$aliases),
    sitelinks = dt_df(lapply(xx$sitelinks, function(x) x[names(x) %in% c('site', 'title')])),
    claims = dt_df(claims)
  )
}

dt_df <- function(x) {
  (ffff <- data.table::setDF(data.table::rbindlist(x, fill = TRUE, use.names = TRUE)))
}

fetch_property <- function(x) {
  tmp <- WikidataR::get_property(x)
  list(
    property_value = tmp$labels$en$value,
    property_description = tmp$descriptions$en$value
  )
}

create_claims <- function(x) {
  lapply(x, function(z) {
    c(
      property = paste0(z$mainsnak$property, collapse = ","),
      fetch_property(z$mainsnak$property),
      value = {
        if (inherits(z$mainsnak$datavalue$value, "data.frame")) {
          z$mainsnak$datavalue$value$`numeric-id`
        } else {
          z$mainsnak$datavalue$value
        }
      }
    )
  })
}

# alldat$labels
# alldat$descriptions
# alldat$aliases
# alldat$sitelinks
# alldat$claims

# ## Wikipedia taxonomy - trying to incorporate
# library(WikipediR)
# library(xml2)
# library(dplyr)
#
# foo("Helianthus")
# foo(name = "Helianthus annuus")
# foo(name = "Quercus lobata")
# foo(name = "Poa annua")
#
# foo <- function(name) {
#   bb <- WikipediR::page_content("en", domain = "species.wikimedia.org", page_name = name)
#   html <- xml2::read_html(bb$parse$text$`*`)
#   xx <- rvest::html_table(
#     xml2::xml_find_first(html, '//table[@class="wikitable mw-collapsible mw-collapsed"]')
#   )
#   setNames(
#     dplyr::bind_rows(
#       lapply(strsplit(xx[[1]], "\n")[[1]], function(z) {
#         tmp <- gsub("^\\s+|\\s+$", "", strsplit(z, ":")[[1]])
#         dplyr::as_data_frame(t(tmp))
#       })
#     ), c('rank', 'name')
#   )
# }
#
#
#
# bb <- WikipediR::page_content("en", domain = "commons.wikimedia.org", page_name = name)
# html <- xml2::read_html(bb$parse$text$`*`)
# xx <- rvest::html_table(
#   xml2::xml_find_first(html, '//table[@class="wikitable mw-collapsible mw-collapsed"]')
# )

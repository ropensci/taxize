#' Search Barcode of Life for taxonomic IDs
#'
#' @export
#' @param sci (character) One or more scientific names.
#' @param id (integer) One or more BOLD taxonomic identifiers.
#' @param fuzzy (logical) Whether to use fuzzy search or not (default: `FALSE`).
#' Only used if `name` passed.
#' @param dataTypes (character) Specifies the datatypes that will be returned.
#' See Details for options. This variable is ignored if `name` parameter is passed,
#' but is used if the `id` parameter is passed.
#' @param includeTree (logical) If TRUE (default: FALSE), returns a list containing
#' information for parent taxa as well as the specified taxon. Only used if `id`
#' passed.
#' @param response (logical) Note that response is the object that returns from the
#' curl call, useful for debugging, and getting detailed info on the API call.
#' @param name Deprecated, see `sci`
#' @param ... named curl options passed on to [crul::verb-GET]
#' @details You must provide one of `sci` or `id` to this function. The other
#' parameters are optional. Note that when passing in `sci`, `fuzzy` can be used
#' as well, while if `id` is passed, then `fuzzy` is ignored, and `dataTypes`
#' `includeTree` can be used.
#'
#' Options for `dataTypes` parameter:
#'
#' * all returns all data
#' * basic returns basic taxon information
#' * images returns specimen image. Includes copyright information, image URL,
#' image metadata.
#' * stats Returns specimen and sequence statistics. Includes public species
#' count, public BIN count, public marker counts, public record count,
#' specimen count, sequenced specimen count, barcode specimen count, species
#' count, barcode species count.
#' * geo Returns collection site information. Includes country, collection
#' site map.
#' * sequencinglabs Returns sequencing labs. Includes lab name, record count.
#' * depository Returns specimen depositories. Includes depository name,
#' record count.
#' * thirdparty Returns information from third parties. Includes wikipedia
#' summary, wikipedia URL, GBIF map.
#'
#' @references http://www.boldsystems.org/index.php/resources/api
#' @return A list of data.frame's.
#' @examples \dontrun{
#' # A basic example
#' bold_search(sci="Apis")
#' bold_search(sci="Agapostemon")
#' bold_search(sci="Poa")
#'
#' # Fuzzy search
#' head(bold_search(sci="Po", fuzzy=TRUE))
#' head(bold_search(sci="Aga", fuzzy=TRUE))
#'
#' # Many names
#' bold_search(sci=c("Apis","Puma concolor"))
#' nms <- names_list('species')
#' bold_search(sci=nms)
#'
#' # Searching by ID - dataTypes can be used, and includeTree can be used
#' bold_search(id=88899)
#' bold_search(id=88899, dataTypes="stats")
#' bold_search(id=88899, dataTypes="geo")
#' bold_search(id=88899, dataTypes="basic")
#' bold_search(id=88899, includeTree=TRUE)
#' }
bold_search <- function(sci = NULL, id = NULL, fuzzy = FALSE,
  dataTypes = 'basic', includeTree=FALSE, response=FALSE, name = NULL, ...) {

  if (!is.null(name)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "bold_search(name)", with = "bold_search(sci)")
    sci <- name
  }
  
  stopifnot(!is.null(sci) | !is.null(id))
  type <- if (is.null(sci)) "id" else "sci"
  tmp <- switch(type,
         sci = bold_tax_name(name = sci, fuzzy = fuzzy, response = response, ...),
         id = bold_tax_id2(id = id, dataTypes = dataTypes, includeTree = includeTree,
                           response = response, ...)
  )
  return(tmp)
}

#' Barcode of Life taxonomic children
#' 
#' BEWARE: this function scrapes data from the BOLD website, so may 
#' be unstable. That is, one day it may work, and the next it may fail.
#' Open an issue if you encounter an error: 
#' https://github.com/ropensci/taxize/issues
#'
#' @export
#' @keywords internal
#' @param id (integer) A BOLD taxonomic identifier. length=1. required
#' @param ... named curl options passed on to [crul::verb-GET]
#' debugging
#' @return list of data.frame's
#' @examples \dontrun{
#' # Osmia (genus): 253 children
#' bold_children(id = 4940)
#' # Momotus (genus): 3 children
#' bold_children(id = 88899)
#' # Momotus aequatorialis (species): no children
#' bold_children(id = 115130)
#' # Osmia sp1 (species): no children
#' bold_children(id = 293378)
#' # Arthropoda (phylum): 27 children
#' bold_children(id = 82)
#' # Psocodea (order): 51 children
#' bold_children(id = 737139)
#' # Megachilinae (subfamily): 2 groups (tribes: 3, genera: 60)
#' bold_children(id = 4962)
#' # Stelis (species): 78 taxa
#' bold_children(id = 4952)
#' }
bold_children <- function(id, ...) {
  stopifnot(length(id) == 1)
  x <- crul::HttpClient$new("https://v4.boldsystems.org", opts = list(...))
  res <- x$get("index.php/Taxbrowser_Taxonpage", query = list(taxid = id))
  res$raise_for_status()
  html <- xml2::read_html(res$parse("UTF-8"))
  nodes <- xml2::xml_find_all(html,
    '//div[@class = "row"]//div[@class = "ibox float-e-margins"]//ol')
  if (length(nodes) == 0) {
    message("no children found")
    return(list(tibble::tibble()))
  }
  group_nmz <- xml2::xml_find_all(html,
    '//div[@class = "row"]//div[@class = "ibox float-e-margins"]//lh')
  bb <- lapply(nodes, bold_children_each_node)
  if (length(group_nmz) > 0) {
    lst_nmz <- tolower(gsub("\\([0-9]+\\)|\\s", "", xml2::xml_text(group_nmz)))
    for (i in seq_along(lst_nmz)) {
      ranknm <- bold_make_rank(lst_nmz[i])
      bb[[i]]$rank <- ranknm
      if (!is.na(ranknm)) names(bb)[i] <- ranknm
    }
  }
  return(bb)
}

bold_children_each_node <- function(x) {
  out <- lapply(xml2::xml_find_all(x, ".//a"), function(w) {
    nm <- gsub("\\s\\[[0-9]+\\]$", "", xml2::xml_text(w))
    id <- strextract(xml2::xml_attr(w, "href"), "[0-9]+$")
    data.frame(name = nm, id = id, stringsAsFactors = FALSE)
  })
  tibble::as_tibble(data.table::rbindlist(out))
}

bold_make_rank <- function(z) {
  if (grepl("order", z)) return("order")
  if (grepl("class", z)) return("class")
  if (grepl("fam", z)) return("family")
  if (grepl("tribe", z)) return("tribe")
  if (grepl("gen", z)) return("genus")
  if (grepl("spec", z)) return("species")
  return(NA_character_)
}

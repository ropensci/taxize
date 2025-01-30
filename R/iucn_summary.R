#' @title Get a summary from the IUCN Red List
#'
#' @description Get a summary from the IUCN Red List
#'   (https://www.iucnredlist.org/).
#'
#' @export
#' @param x character; Scientific name. Should be cleaned and in the format
#'   `*<Genus> <Species>*`.
#' @param distr_detail logical; If `TRUE`, the geographic distribution is
#'   returned as a list of vectors corresponding to the different range types:
#'   native, introduced, etc.
#' @param key a Redlist API key, get one from
#'   https://apiv3.iucnredlist.org/api/v3/token Required for `iucn_summary`.
#'   Defaults to `NULL` in case you have your key stored (see `Redlist
#'   Authentication` below).
#' @param ... curl options passed on to [crul::verb-GET]
#'
#' @return A list (for every species one entry) of data returned by
#'   [rredlist::rl_species_latest()].
#'
#' @note Not all data types are available for every species
#'   and NA is returned. [iucn_status()] is an extractor function to easily
#'   extract status into a vector.
#'
#' @seealso [iucn_status()]
#'
#' @details Beware: IUCN functions can give back incorrect data. This isn't our
#'   fault. We do our best to get you the correct data quickly, but sometimes
#'   IUCN gives back the wrong data, and sometimes Global Names gives back the
#'   wrong data. We will fix these as soon as possible. In the meantime, just
#'   make sure that the data you get back is correct.
#'
#'   `iucn_summary` has a default method that errors when anything's passed in
#'   that's not `character` or `iucn` class - a `iucn_summary.character` method
#'   for when you pass in taxon names - and a `iucn_summary.iucn` method so you
#'   can pass in iucn class objects as output from [get_iucn()] or [as.iucn()].
#'   If you already have IUCN IDs, coerce them to `iucn` class via `as.iucn(...,
#'   check = FALSE)`
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @author Philippe Marchand, \email{marchand.philippe@@gmail.com}
#' @author Scott Chamberlain,
#' @author Zachary S.L. Foster
#'
#' @section Redlist Authentication: `iucn_summary` uses the new Redlist API for
#'   searching for a IUCN ID, so we use the [rredlist::rl_species()] function internally.
#'   This function requires an API key. Get the key at
#'   https://apiv3.iucnredlist.org/api/v3/token, and pass it to the `key`
#'   parameter, or store in your `.Renviron` file like
#'   `IUCN_REDLIST_KEY=yourkey` or in your `.Rprofile` file like
#'   `options(iucn_redlist_key="yourkey")`. We strongly encourage you to not
#'   pass the key in the function call but rather store it in one of those two
#'   files. This key will also set you up to use the \pkg{rredlist} package.
#'
#' @examples \dontrun{
#' # if you send a taxon name, an IUCN API key is required
#' ## here, the key is being detected from a .Rprofile file
#' ## or .Renviron file, See "Redlist Authentication" above
#' iucn_summary("Lutra lutra")
#'
#' ia <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
#' ia <- iucn_summary(c("Panthera uncia", "Lynx lynx", "aaa"))
#' iucn_summary("Muntiacus rooseveltorum/truongsonensis")
#' iucn_summary(c("Muntiacus rooseveltorum/truongsonensis", "Lynx lynx"))
#'
#' ## get detailed distribution
#' iac <- iucn_summary(x="Ara chloropterus", distr_detail = TRUE)
#' iac[[1]]$distr
#'
#'
#' # If you pass in an IUCN ID, you don't need to pass in a Redlist API Key
#' # extract status
#' iucn_status(iac)
#' }
iucn_summary <- function(x, distr_detail = FALSE, key = NULL, ...) {
  UseMethod("iucn_summary")
}

#' @export
iucn_summary.default <- function(x, distr_detail = FALSE, key = NULL, ...) {
  stop("no 'iucn_summary' method for ", class(x), call. = FALSE)
}

#' @export
iucn_summary.character <- function(x, distr_detail = FALSE, key = NULL, ...) {
  raw_data <- get_iucn_data(x, key = key, latest = TRUE)
  structure(raw_data, class = "iucn_summary")
}


## helpers --------
try_red <- function(fun, x, key, ...) {
  tryCatch(fun(id = x, key = key, ...), error = function(e) e)
}

null_res <- list(status = NA, history = NA, distr = NA, trend = NA)

#' Extractor functions for `iucn`-class.
#'
#' @export
#' @param x an `iucn`-object as returned b`iucn_summary`ry
#' @param ... Currently not used
#' @return A character vector with the status.
#' @seealso [iucn_summary()]
#' @examples \dontrun{
#' ia <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
#' iucn_status(ia)}
iucn_status <- function(x, ...){
  UseMethod("iucn_status")
}

#' @export
iucn_status.default <- function(x, ...) {
  stop("no method for 'iucn_status' for ", class(x), call. = FALSE)
}

#' @export
iucn_status.iucn_summary <- function(x, ...) {
  unlist(lapply(x, function(x) x$red_list_category$code))
}

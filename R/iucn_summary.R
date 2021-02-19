#' @title Get a summary from the IUCN Red List
#'
#' @description Get a summary from the IUCN Red List (https://www.iucnredlist.org/).
#'
#' @export
#' @param x character; Scientific name. Should be cleaned and in the
#' format `*<Genus> <Species>*`.
#' @param distr_detail logical; If `TRUE`, the geographic distribution is
#' returned as a list of vectors corresponding to the different range types:
#' native, introduced, etc.
#' @param key a Redlist API key, get one from 
#' https://apiv3.iucnredlist.org/api/v3/token Required for 
#' `iucn_summary`. Defaults to `NULL` in case you have your key 
#' stored (see `Redlist Authentication` below).
#' @param ... curl options passed on to [crul::verb-GET]
#'
#' @return A list (for every species one entry) of lists with the following
#' items:
#' * `status` Red List Category.
#' * `history` History of status, if available.
#' * `distr` Geographic distribution, if available.
#' * `trend` Trend of population size, if available.
#'
#' @note Not all entries (history, distr, trend) are available for every species
#' and NA is returned.
#' [iucn_status()] is an extractor function to easily extract
#' status into a vector.
#'
#' @seealso [iucn_status()]
#'
#' @details Beware: IUCN functions can give back incorrect data. This isn't our fault.
#' We do our best to get you the correct data quickly, but sometimes IUCN gives
#' back the wrong data, and sometimes Global Names gives back the wrong data.
#' We will fix these as soon as possible. In the meantime, just make sure that
#' the data you get back is correct.
#'
#' `iucn_summary` has a default method that errors when anything's
#' passed in that's not `character` or `iucn` class - a
#' `iucn_summary.character` method for when you pass in taxon names -
#' and a `iucn_summary.iucn` method so you can pass in iucn class objects
#' as output from [get_iucn()] or [as.iucn()]. If you
#' already have IUCN IDs, coerce them to `iucn` class via
#' `as.iucn(..., check = FALSE)`
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @author Philippe Marchand, \email{marchand.philippe@@gmail.com}
#' @author Scott Chamberlain, 
#'
#' @section Redlist Authentication:
#' `iucn_summary` uses the new Redlist API for searching for a IUCN ID, so we
#' use the [rl_search()] function internally. This function
#' requires an API key. Get the key at https://apiv3.iucnredlist.org/api/v3/token,
#' and pass it to the `key` parameter, or store in your `.Renviron` file like
#' `IUCN_REDLIST_KEY=yourkey` or in your `.Rprofile` file like
#' `options(iucn_redlist_key="yourkey")`. We strongly encourage you to not pass
#' the key in the function call but rather store it in one of those two files.
#' This key will also set you up to use the \pkg{rredlist} package.
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
  safe_get_iucn <- function(x, key, ...) {
    tmp <- tryCatch(get_iucn(x, key = key, ...), error = function(e) e)
    if (any(is.na(tmp)) || inherits(tmp, "error")) {
      if (inherits(tmp, "error")) {
        nas <- x
      } else {
        nas <- x[is.na(tmp)]
      }
      warning("taxa '", paste0(nas, collapse = ", ") ,
              "' not found!\n Returning NA!", call. = FALSE)
      return(NA_character_)
    }
    return(tmp)
  }
  xid <- lapply(x, safe_get_iucn, key = key)
  xid <- as.numeric(xid)
  res <- get_iucn_summary2(xid, distr_detail, key = key, ...)
  structure(stats::setNames(res, x), class = "iucn_summary")
}

#' @export
iucn_summary.iucn <- function(x, distr_detail = FALSE, key = NULL, ...) {
  res <- get_iucn_summary2(x, distr_detail, key = key, ...)
  structure(stats::setNames(res, x), class = "iucn_summary")
}

#' DEFUNCT
#' @export
#' @keywords internal
#' @rdname iucn_summary_id-defunct
iucn_summary_id <- function(...) {
  .Defunct(msg = "use iucn_summary()")
}


## helpers --------
try_red <- function(fun, x, key, ...) {
  tryCatch(fun(id = x, key = key, ...), error = function(e) e)
}

null_res <- list(status = NA, history = NA, distr = NA, trend = NA)

get_iucn_summary2 <- function(query, distr_detail, key = NULL, ...) {
  fun <- function(z, ...) {
    if (is.na(z)) return(null_res)
    res <- try_red(rredlist::rl_search, z, key, ...)
    if (!inherits(res, "error")) {
      # history
      history <- try_red(rredlist::rl_history, z, key, ...)
      if (NROW(history$result) == 0 || inherits(history, "error")) {
        history <- NA
      } else {
        history <- history$result
      }

      # distribution
      distr <- try_red(rredlist::rl_occ_country, z, key, ...)
      if (NROW(distr$result) == 0 || inherits(distr, "error")) {
        distr <- NA
      } else {
        distr <- distr$result
        if (distr_detail) {
          distr <- split(distr, distr$distribution_code)
        } else {
          distr <- distr$country
        }
      }

      # trend - NOT SURE HOW TO GET IT
      # build output
      out <- list(status = res$result$category,
                  history = history, distr = distr, trend = NA)
    } else {
      warning("taxon ID '", z , "' not found!\n Returning NA!", call. = FALSE)
      out <- null_res
    }
    return(out)
  }
  lapply(query, fun, ...)
}

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
  unlist(lapply(x, function(x) x$status))
}

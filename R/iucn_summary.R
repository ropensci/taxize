#' @title Get a summary from the IUCN Red List
#'
#' @description Get a summary from the IUCN Red List (\url{http://www.iucnredlist.org/}).
#'
#' @export
#' @param x character; Scientific name. Should be cleaned and in the
#' format \emph{<Genus> <Species>}.
#' @param parallel logical; Search in parallel to speed up search. You have to
#' register a parallel backend if \code{TRUE}. See e.g., doMC, doSNOW, etc.
#' @param distr_detail logical; If \code{TRUE}, the geographic distribution is
#' returned as a list of vectors corresponding to the different range types:
#' native, introduced, etc.
#' @param key a Redlist API key, get one from 
#' \url{http://apiv3.iucnredlist.org/api/v3/token}. Required for 
#' \code{iucn_summary}. Defaults to \code{NULL} in case you have your key 
#' stored (see \code{Redlist Authentication} below).
#' @param ... curl options passed on to [crul::verb-GET]
#'
#' @return A list (for every species one entry) of lists with the following
#' items:
#' \item{status}{Red List Category.}
#' \item{history}{History of status, if available.}
#' \item{distr}{Geographic distribution, if available.}
#' \item{trend}{Trend of population size, if available.}
#'
#' @note Not all entries (history, distr, trend) are available for every species
#' and NA is returned.
#' \code{\link[taxize]{iucn_status}} is an extractor function to easily extract
#' status into a vector.
#'
#' @seealso \code{\link[taxize]{iucn_status}}
#'
#' @details Beware: IUCN functions can give back incorrect data. This isn't our fault.
#' We do our best to get you the correct data quickly, but sometimes IUCN gives
#' back the wrong data, and sometimes Global Names gives back the wrong data.
#' We will fix these as soon as possible. In the meantime, just make sure that
#' the data you get back is correct.
#'
#' \code{iucn_summary} has a default method that errors when anything's
#' passed in that's not \code{character} or \code{iucn} class - a
#' \code{iucn_summary.character} method for when you pass in taxon names -
#' and a \code{iucn_summary.iucn} method so you can pass in iucn class objects
#' as output from \code{\link{get_iucn}} or \code{\link{as.iucn}}. If you
#' already have IUCN IDs, coerce them to \code{iucn} class via
#' \code{as.iucn(..., check = FALSE)}
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @author Philippe Marchand, \email{marchand.philippe@@gmail.com}
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#'
#' @section Redlist Authentication:
#' \code{iucn_summary} uses the new Redlist API for searching for a IUCN ID, so we
#' use the \code{\link[rredlist]{rl_search}} function internally. This function
#' requires an API key. Get the key at \url{http://apiv3.iucnredlist.org/api/v3/token},
#' and pass it to the \code{key} parameter, or store in your \code{.Renviron} file like
#' \code{IUCN_REDLIST_KEY=yourkey} or in your \code{.Rprofile} file like
#' \code{options(iucn_redlist_key="yourkey"}. We strongly encourage you to not pass
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
#'
#' ## get detailed distribution
#' iac <- iucn_summary(x="Ara chloropterus", distr_detail = TRUE)
#' iac[[1]]$distr
#'
#'
#' # If you pass in an IUCN ID, you don't need to pass in a Redlist API Key
#' # extract status
#' iucn_status(iac)
#'
#' # using parallel, e.g., with doMC package, register cores first
#' # library(doMC)
#' # registerDoMC(cores = 2)
#' # nms <- c("Panthera uncia", "Lynx lynx", "Ara chloropterus", "Lutra lutra")
#' # (res <- iucn_summary(nms, parallel = TRUE))
#' }
iucn_summary <- function(x, parallel = FALSE, distr_detail = FALSE,
                         key = NULL, ...) {
  UseMethod("iucn_summary")
}

#' @export
iucn_summary.default <- function(x, parallel = FALSE, distr_detail = FALSE,
                                 key = NULL, ...) {
  stop("no 'iucn_summary' method for ", class(x), call. = FALSE)
}

#' @export
iucn_summary.character <- function(x, parallel = FALSE, distr_detail = FALSE,
                                   key = NULL, ...) {
  xid <- get_iucn(x, key = key, ...)
  if (any(is.na(xid))) {
    nas <- x[is.na(xid)]
    warning("taxa '", paste0(nas, collapse = ", ") ,
            "' not found!\n Returning NAs!")
    if (all(is.na(xid))) {
      tmp <- list(status = NA, history = NA, distr = NA, trend = NA)
      tmp <- stats::setNames(replicate(length(x), tmp, simplify = FALSE), x)
      class(tmp) <- "iucn_summary"
      return(tmp)
    }
  }
  xid <- as.numeric(xid)
  res <- get_iucn_summary2(xid, parallel, distr_detail, key = key, ...)
  structure(stats::setNames(res, x), class = "iucn_summary")
}

#' @export
iucn_summary.iucn <- function(x, parallel = FALSE, distr_detail = FALSE,
                              key = NULL, ...) {
  res <- get_iucn_summary2(x, parallel, distr_detail, key = key, ...)
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

get_iucn_summary2 <- function(query, parallel, distr_detail, key = NULL, ...) {
  fun <- function(z) {
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

  if (parallel) {
    llply(query, fun, .parallel = TRUE)
  } else {
    lapply(query, fun)
  }
}

#' Extractor functions for \code{iucn}-class.
#'
#' @export
#' @param x an \code{iucn}-object as returned by \code{iucn_summary}
#' @param ... Currently not used
#' @return A character vector with the status.
#' @seealso \code{\link[taxize]{iucn_summary}}
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

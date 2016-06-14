#' @title Get a summary from the IUCN Red List
#'
#' @description Get a summary from the IUCN Red List (\url{http://www.iucnredlist.org/}).
#'
#' @param sciname character; Scientific name. Should be cleaned and in the
#' format \emph{<Genus> <Species>}.
#' @param silent logical; Make errors silent or not (when species not found).
#' @param parallel logical; Search in parallel to speed up search. You have to
#' register a parallel backend if \code{TRUE}. See e.g., doMC, doSNOW, etc.
#' @param distr_detail logical; If \code{TRUE}, the geographic distribution is
#' returned as a list of vectors corresponding to the different range types:
#' native, introduced, etc.
#' @param key a Redlist API key, get one from \url{http://apiv3.iucnredlist.org/api/v3/token}
#' Required for \code{iucn_summary} but not needed for \code{iucn_summary_id}. Defaults to
#' \code{NULL} in case you have your key stored (see \code{Redlist Authentication} below).
#' @param ... Currently not used.
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
#' # if you send a taxon name, pass in a key
#' iucn_summary("Lutra lutra")
#'
#' ia <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
#' ia <- iucn_summary(c("Panthera uncia", "Lynx lynx", "aaa"))
#'
#' ## get detailed distribution
#' iac <- iucn_summary("Ara chloropterus", distr_detail = TRUE)
#' iac[[1]]$distr
#'
#' # If you pass in an IUCN ID, you don't need to pass in a Redlist API Key
#' ia <- iucn_summary_id(c(22732, 12519))
#' # extract status
#' iucn_status(ia)
#' # extract other available information
#' ia[['Lynx lynx']]$history
#' ia[['Panthera uncia']]$distr
#' ia[[2]]$trend
#' }
#' @export
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @author Philippe Marchand, \email{marchand.philippe@@gmail.com}
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
iucn_summary <- function(sciname, silent = TRUE, parallel = FALSE,
                         distr_detail = FALSE, key = NULL, ...) {
  get_iucn_summary(sciname, silent, parallel, distr_detail, by_id = FALSE, key = key, ...)
}

#' @param species_id an IUCN ID
#' @export
#' @rdname iucn_summary
iucn_summary_id <- function(species_id, silent = TRUE, parallel = FALSE,
                            distr_detail = FALSE, ...) {
    get_iucn_summary(species_id, silent, parallel, distr_detail, by_id = TRUE, ...)
}


get_iucn_summary <- function(query, silent, parallel, distr_detail, by_id, key = NULL, ...) {

  fun <- function(query) {

    if (!by_id) {
        #to deal with subspecies
        sciname_q <- strsplit(query, " ")
        spec <- tolower(paste(sciname_q[[1]][1], sciname_q[[1]][2]))
        res <- tryCatch(rredlist::rl_search(spec, key = key), error = function(e) e)
        if (inherits(res, "error")) {
          stop(res$message, " - see ?iucn_summary and http://apiv3.iucnredlist.org/api/v3/token", call. = FALSE)
        }
        if (!inherits(res, "try-error") && NROW(res$result) > 0) {
            df <- unique(res$result)
            #check if there are several matches
            scinamelist <- df$scientific_name
            species_id <- df$taxonid[which(tolower(scinamelist) == tolower(query))]
        }
    } else {
        species_id <- query
    }
    if (!exists('species_id')) {
        warning("Species '", query , "' not found!\n Returning NA!")
        out <- list(status = NA,
                    history = NA,
                    distr = NA,
                    trend = NA)
    } else {
      url <- paste("http://api.iucnredlist.org/details/", species_id, "/0", sep = "")
      e <- try(h <- xml2::read_html(url), silent = silent)
      if (!inherits(e, "try-error")) {
        # scientific name
        if (by_id) {
            sciname <- xml2::xml_text(xml2::xml_find_all(h, '//h1[@id = "scientific_name"]'))
        }

        # status
        status <- xml2::xml_text(xml2::xml_find_all(h, '//div[@id ="red_list_category_code"]'))
        # history
        history <- data.frame(year = xml2::xml_text(xml2::xml_find_all(h, '//div[@class="year"]')),
                              category = xml2::xml_text(xml2::xml_find_all(h, '//div[@class="category"]')))
        if (nrow(history) == 0) history <- NA
        # distribution
        distr <- xml2::xml_text(xml2::xml_find_all(h, '//ul[@class="countries"]'))
        if (length(distr) == 0) {
          distr <- NA
        } else {
          distr <- sub("^\n", "", distr)  # remove leading newline
          distr <- strsplit(distr, "\n")
          if (distr_detail) {
              names(distr) <- xml2::xml_text(xml2::xml_find_all(h, '//ul[@class="country_distribution"]//div[@class="distribution_type"]'))
          } else {
              distr <- unlist(distr)
          }
        }

        # trend
        trend <- xml2::xml_text(xml2::xml_find_all(h, '//div[@id="population_trend"]'))
        if (length(trend) == 0) trend <- NA

        out <- list(status = status,
                    history = history,
                    distr = distr,
                    trend = trend)
        if (by_id) out$sciname <- sciname
      } else {
        warning("Species '", query , "' not found!\n Returning NA!", call. = FALSE)
        out <- list(status = NA,
                    history = NA,
                    distr = NA,
                    trend = NA)
      }
    }
    return(out)
  }

  if (parallel) {
    out <- llply(query, fun, .parallel = TRUE)
  } else {
    out <- llply(query, fun)
  }

  if (by_id) {
      names(out) <- llply(out, `[[`, "sciname")
      out <- llply(out, function(x) {x$sciname <- NULL; x})
  } else {
      names(out) <- query
  }
  class(out) <- "iucn"
  return(out)
}


#' Extractor functions for \code{iucn}-class.
#'
#' @param x an \code{iucn}-object as returned by \code{iucn_summary}
#' @param ... Currently not used
#' @return A character vector with the status.
#' @seealso \code{\link[taxize]{iucn_summary}}
#' @export
#' @examples \dontrun{
#' ia <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
#' iucn_status(ia)}
iucn_status <- function(x, ...){
  UseMethod("iucn_status")
}

#' @method iucn_status default
iucn_status.default <- function(x, ...) {
  stop("No default method for status defined!\n
       Did you mean iucn_status.iucn?\n", .call = FALSE)
}

#' @method iucn_status iucn
#' @param x an \code{iucn} object as returned by
#' \code{\link[taxize]{iucn_summary}}.
#' @export
#' @rdname iucn_summary
iucn_status.iucn <- function(x, ...) {
  unlist(lapply(x, function(x) x$status))
}

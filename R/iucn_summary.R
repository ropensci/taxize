#' @title Get a summary from the IUCN Red List
#'
#' @description Get a summary from the IUCN Red List (\url{http://www.iucnredlist.org/}).
#'
#' @param sciname character; Scientific name. Should be cleand and in the
#' format \emph{<Genus> <Species>}.
#' @param silent logical; Make errors silent or not (when species not found).
#' @param parallel logical; Search in parallel to speed up search. You have to
#' register a parallel backend if \code{TRUE}. See e.g., doMC, doSNOW, etc.
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
#' @examples \dontrun{
#' ia <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
#' ia <- iucn_summary(c("Panthera uncia", "Lynx lynx", "aaa"))
#' # extract status
#' iucn_status(ia)
#' # extract other available information
#' ia[['Lynx lynx']]$history
#' ia[['Panthera uncia']]$distr
#' ia[[2]]$trend
#' }
#'
#'
#' @export
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
iucn_summary <- function(sciname, silent = TRUE, parallel = FALSE, ...) {

  fun <- function(sciname){
    #to deal with subspecies
    sciname_q <- strsplit(sciname, " ")
    spec <- tolower(paste(sciname_q[[1]][1], sciname_q[[1]][2]))
    res <- tryCatch(rredlist::rl_search(spec), error = function(e) e)
    if (!inherits(res, "try-error") && NROW(res$result) > 0) {
      df <- unique(res$result)
      #check if there are several matches
      scinamelist <- df$scientific_name
      species_id <- df$taxonid[which(tolower(scinamelist) == tolower(sciname))]
    }

    if (!exists('species_id')) {
        warning("Species '", sciname , "' not found!\n Returning NA!")
        out <- list(status = NA,
                    history = NA,
                    distr = NA,
                    trend = NA)
    } else {
      url <- paste("http://api.iucnredlist.org/details/", species_id, "/0", sep = "")
      e <- try(h <- xml2::read_html(url), silent = silent)
      if (!inherits(e, "try-error")) {
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
          distr <- unlist(strsplit(distr, "\n"))
        }

        # trend
        trend <- xml2::xml_text(xml2::xml_find_all(h, '//div[@id="population_trend"]'))
        if (length(trend) == 0) trend <- NA

        out <- list(status = status,
                    history = history,
                    distr = distr,
                    trend = trend)
      } else {
        warning("Species '", sciname , "' not found!\n Returning NA!", call. = FALSE)
        out <- list(status = NA,
                    history = NA,
                    distr = NA,
                    trend = NA)
      }
    }
    return(out)
  }

  if (parallel) {
    out <- llply(sciname, fun, .parallel = TRUE)
  } else {
    out <- llply(sciname, fun)
  }

  names(out) <- sciname
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

#' Get a summary from the IUCN Red List.
#' 
#' Get a summary from the IUCN Red List (\url{http://www.iucnredlist.org/}).
#' 
#' @import XML plyr
#' @param sciname character; Scientific name. 
#' @param silent logical; Make errors silent or not (when species not found).
#' @param parallel logical; Search in parallel to speed up search. You have to register 
#' a parallel backend if \code{TRUE}. See e.g., doMC, doSNOW, etc.
#' @param ... Currently not used.
#'    
#' @return A list (for every species one entry) of lists with the following items:
#' \item{status}{Red List Category.}
#' \item{history}{History of status, if available.}
#' \item{distr}{Geographic distribution, if available.}
#' \item{trend}{Trend of population size, if available.}
#' 
#' @note Not all entries (history, distr, trend) are available for every species and NA is returned. 
#' \code{\link[taxize]{iucn_status}} is an extractor function to easily extract status into a vector.
#' 
#' @seealso \code{\link[taxize]{iucn_status}}
#' @examples \dontrun{
#' ia <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
#' ia <- iucn_summary(c("Panthera uncia", "Lynx lynx", "aaa"))
#' # extract status
#' iucn_status(ia)
#' # extract other available information
#' ia[['Lynx lynx']]$history
#' ia[['Panthera uncia']]$distr
#' ia[[1]]$pop
#' ia[[2]]$trend
#' }
#' 
#' @author Eduard Szoecs, \email{szoe8822@@uni-landau.de}
#' 
#' @export
iucn_summary <- function(sciname, silent = TRUE, parallel = FALSE, ...) 
{
  fun <- function(sciname){
    spec <- tolower(sciname)
    spec <- gsub(" ", "-", spec)
    url <- paste("http://api.iucnredlist.org/go/", spec, sep = "")
    e <- try(h <- htmlParse(url), silent=silent)
    if(!inherits(e, "try-error")){
      # status
      status <- xpathSApply(h, '//div[@id ="red_list_category_code"]', xmlValue)
      # history
      history <- data.frame(year = xpathSApply(h, '//div[@class="year"]', xmlValue),
                            category = xpathSApply(h, '//div[@class="category"]', xmlValue))
      if(nrow(history) == 0)
        history <- NA
      # distribution
      distr <- xpathSApply(h, '//ul[@class="countries"]', xmlValue)
      if(length(distr) == 0) {
        distr <- NA
      } else {
        distr <- unlist(strsplit(distr, "\n"))
      }
      # trend
      trend <- xpathSApply(h, '//div[@id="population_trend"]', xmlValue)
      if(length(trend) == 0)
        trend <- NA
      
      out <- list(status = status, 
                  history = history, 
                  distr = distr, 
                  trend = trend)
    } else {
      warning("Species '", sciname , "' not found!\n Returning NA!")
      out <- list(status = NA, 
                  history = NA, 
                  distr = NA, 
                  trend = NA)
    }    
    return(out)
  }
  if(parallel){
    out <- llply(sciname, fun, .parallel = TRUE)
  } else
  {
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

#' @S3method iucn_status default
iucn_status.default <- function(x, ...) {
  stop("No default method for status defined!\n
       Did you mean iucn_status.iucn?\n")
}

#' @method iucn_status iucn
#' @export
#' @rdname iucn_summary
iucn_status.iucn <- function(x, ...) {
  out <- unlist(lapply(x, function(x) x$status))
  return(out)
}

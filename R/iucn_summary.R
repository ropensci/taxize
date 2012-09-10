#' Get a summary from the IUCN Red List.
#' 
#' Get a summary from the IUCN Red List (\url{http://www.iucnredlist.org/}).
#' 
#' @import XML
#' @param sciname Scientific name. 
#' @return A list (for every species one entry) of lists with the following items:
#' \item{status}{Red List Category.}
#' \item{history}{History of status.}
#' \item{distr}{Geographic distribution.}
#' \item{pop}{Population size estimates.}
#' \item{trend}{Trend of population size.}
#' @examples \dontrun{
#' ia <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
#' ia <- iucn_summary(c("Panthera uncia", "Lynx lynx", "aaa"))
#' # extract status
#' ldply(ia, function(x) x$status)
#' laply(ia, function(x) x$status)
#' ia[['Lynx lynx']]$history
#' ia[['Panthera uncia']]$distr
#' ia[[1]]$pop
#' ia[[2]]$trend
#' }
#' 
#' @author Eduard Szöcs \email{szoe8822@@uni-landau.de}
#' 
#' @export
iucn_summary <- function(sciname) 
{
  fun <- function(sciname){
    spec <- tolower(sciname)
    spec <- gsub(" ", "-", spec)
    url <- paste("http://api.iucnredlist.org/go/", spec, sep="")
    e <- try(h <- htmlParse(url))
    if(class(e) != "try-error"){
      status <- xpathSApply(h, '//div[@id ="red_list_category_code"]', xmlValue)
      history <- data.frame(year = xpathSApply(h, '//div[@class="year"]', xmlValue),
                            category = xpathSApply(h, '//div[@class="category"]', xmlValue))
      distr <- xpathSApply(h, '//ul[@class="countries"]', xmlValue)
      distr <- unlist(strsplit(distr, "\n"))
      pop <- xpathSApply(h, '//div[@id="population"]/text()[preceding-sibling::br]', xmlValue)
      pop <- do.call(rbind, lapply(strsplit(pop, split=":"), rbind)) 
      trend <- xpathSApply(h, '//div[@id="population_trend"]', xmlValue)
      out <- list(status = status, 
                  history = history, 
                  distr = distr, 
                  pop = pop, 
                  trend = trend)
    } else {
      warning("Species '", sciname , "' not found!\n Returning NA!")
      out <- list(status = NA, 
                  history = NA, 
                  distr = NA, 
                  pop = NA, 
                  trend = NA)
    }    
    return(out)
  }
  out <- llply(sciname, fun)
  names(out) <- sciname
  class(out) <- "iucn"
  return(out)
}
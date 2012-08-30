#' Get a summary from the IUCN Red List.
#' 
#' Get a summary from the IUCN Red List (\url{http://www.iucnredlist.org/}).
#' 
#' @import XML
#' @param sciname Scientific name. 
#' @return A list with the following items:
#' \item{status}{Red List Category.}
#' \item{history}{History of status.}
#' \item{distr}{Geographic distribution.}
#' \item{pop}{Population size estimates.}
#' \item{trend}{Trend of population size.}
#' @examples \dontrun{
#' ia <- iucn_summary("Panthera uncia")
#' ia$status
#' ia$history
#' ia$distr
#' ia$pop
#' ia$trend
#' }
#' 
#' @author Eduard Szöcs \email{szoe8822@@uni-landau.de}
#' 
#' @export
iucn_summary <- function(sciname) 
{
  spec <- tolower(sciname)
  spec <- gsub(" ", "-", spec)
  url <- paste("http://api.iucnredlist.org/go/", spec, sep="")
  h <- htmlParse(url)
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
  return(out)
}
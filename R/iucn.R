iucn_summary <- function(x) {
  spec <- tolower(x)
  spec <- gsub(" ", "-", spec)
  url <- paste("http://api.iucnredlist.org/go/", spec, sep="")
  h <- htmlParse(url)
  status <- xpathSApply(h, '//div[@id ="red_list_category_code"]', xmlValue)
  history <- data.frame(year = xpathSApply(h, '//div[@class="year"]', xmlValue),
                        category = xpathSApply(h, '//div[@class="category"]', xmlValue))
  distr <- xpathSApply(h, '//ul[@class="countries"]', xmlValue)
  distr <- unlist(strsplit(distr1, "\n"))
  pop <- xpathSApply(h, '//div[@id="population"]/text()[preceding-sibling::br]', xmlValue)
  trend <- xpathSApply(h, '//div[@id="population_trend"]', xmlValue)
  out <- list(status = status, 
              histroy = history, 
              distr = distr, 
              pop = pop, 
              trend = trend)
  return(out)
}

#ia <- iucn_summary("Panthera uncia")


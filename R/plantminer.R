#' Search for taxonomy data from Plantminer.com
#' 
#' @import RCurl foreach
#' @param plants Vector of plant species names.
#' @param key Your api key for the plantminer.com site.  Go to 
#' 		\url{http://www.plantminer.com/} to get your api key.  Two options for 
#' 		inputting your key.  1) You can input it manually within the function as 
#' 		the second argument, or 2) you can put the key in your .Rprofile file, 
#' 		which will then be loaded when you start R. See 
#' 		\url{https://github.com/ropensci/rOpenSci/wiki/Installation-and-use-of-API-keys} 
#' 		for help on how to put api keys in your .Rprofile file.
#' @return data.frame of results.
#' @examples \dontrun{
#' plants <- c("Myrcia lingua", "Myrcia bella", "Ocotea pulchella", 
#' 		"Miconia", "Coffea arabica var. amarella", "Bleh")
#' plantminer(plants)
#' }
#' @export
plantminer <- function(plants, key = as.numeric(getOption("pmkey", stop("need an API key"))))
{
  i <- NULL
  compiled.list <- foreach (i=1:length(plants), .combine =
    rbind) %do% {
    full.name <- strsplit(plants[i], " ")[[1]]
    cat(full.name, "\n")
    genus <- full.name[1]
    if (length(full.name) > 1) {
      species <- paste(full.name[2:length(full.name)], collapse = " ")
      url.pm <- URLencode(paste("http://www.plantminer.com/search/",
          genus, "/", species, "?key=", key, sep = ""))
    } else {
      species <- ""
      url.pm <- URLencode(paste("http://www.plantminer.com/search/",
          genus, "?key=", key, sep = ""))
    }
    sp <- getURL(url.pm)
    sp <- c(unlist(strsplit(sp, "\\|")))
    sp
  }
  colnames(compiled.list) <- c("fam", "genus", "sp", "author", "source",
    "source.id", "status", "confidence", "suggestion", "database")
  rownames(compiled.list) <- 1:NROW(compiled.list)
  return(data.frame(compiled.list))
}
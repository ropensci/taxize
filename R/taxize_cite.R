#' Get citations and licenses for data sources used in taxize
#'
#' @export
#' @param fxn Function to search on. A special case is the package name 'taxize' that will give the
#' citations for the package.
#' @param what One of citation (default), license, or both.
#' @examples
#' taxize_cite(fxn='tnrs')
#' taxize_cite(fxn='eol')
#'
#' # Get the taxize citation
#' taxize_cite(fxn='taxize')
#'
#' # Get license information
#' taxize_cite(fxn='taxize', "license")
#' taxize_cite(fxn='tnrs', "license")

taxize_cite <- function(fxn = "itis", what = 'citation'){
  what <- match.arg(what, c("citation","license","both"))
  if(what == "citation"){
    out <- data_citations(fxn)
    if(is.null(out)){
      cat("Nothing found, try different inputs", "\n\n")
      cat("Can this citation be improved? https://github.com/ropensci/taxize/issues", "\n")
      cat("Please cite taxize in your paper: citation(package = 'taxize')")
    } else {
      if(fxn == 'taxize')
      {
        cat("The paper: ","\n")
        print(out[[1]])
        cat("\n")
        cat("The software: ","\n")
        print(out[[2]])
      } else {
        cat(sprintf("Home page: %s", out$url_home), "\n")
        cat(sprintf("API help: %s", out$apidocs), "\n\n")
        cat("Can this citation be improved? https://github.com/ropensci/taxize/issues")
      }
    }
  } else if(what == "license") {
    out <- data_licenses(fxn)
    if(is.null(out)){
      cat("Unkown...")
    } else{
      cat(sprintf("License: %s", out$license), "\n")
      cat(sprintf("URL:     %s", out$url))
    }
  }
}

data_citations <- function(x){
  switch(x,
    tnrs = list(url_home='http://taxosaurus.org/', apidocs='http://www.evoio.org/wiki/Phylotastic/TNRS'),
    eol = list(url_home='http://eol.org/', apidocs='http://eol.org/api/'),
    gbif = list(url_home='http://www.gbif.org', apidocs='http://www.gbif.org/developer/summary',
                citation='GBIF (2013). GBIF (Ed.), Global Biodiversity Information Facility Data Portal (2013)',
                dataset_citation='http://www.gbif.org/resources/2381'),
    taxize = citation("taxize")
  )
}

data_licenses <- function(x){
  switch(x,
    taxize = list(license="MIT", url="http://opensource.org/licenses/MIT")
  )
}

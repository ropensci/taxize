#' Get citations and licenses for data sources used in taxize
#' 
#' @param fxn
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
    taxize = citation("taxize")
  )
}

data_licenses <- function(x){
  switch(x,
    taxize = list(license="MIT", url="http://opensource.org/licenses/MIT")
  )
}
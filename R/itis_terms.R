#' Get ITIS terms, i.e., tsn's, authors, common names, and scientific names.
#' 
#' @param query One or more common or scientific names, or partial names
#' @param what One of both (search common and scientific names), common (search just 
#'    common names), or scientific (search just scientific names)
#' @param ... Further arguments passed on to \code{\link{getitisterms}}, 
#'    \code{\link{getitistermsfromcommonname}}, \code{\link{getitistermsfromscientificname}}
#' @examples \dontrun{
#' # Get terms searching both common and scientific names
#' itis_terms(query='bear')
#' 
#' # Get terms searching just common names
#' itis_terms(query='tarweed', "common")
#' 
#' # Get terms searching just scientific names
#' itis_terms(query='Poa annua', "scientific")
#' }
#' @export

itis_terms <- function(query, what="both", ...)
{
  temp <- switch(what, 
                 both = lapply(query, function(x) getitisterms(x, ...)),
                 common = lapply(query, function(x) getitistermsfromcommonname(x, ...)),
                 scientific = lapply(query, function(x) getitistermsfromscientificname(x, ...)))
  if(length(query)==1){
    temp[[1]]
  } else
  {
    names(temp) <- query
    temp
  } 
}
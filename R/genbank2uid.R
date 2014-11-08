#' Get NCBI taxonomy UID from GenBankID
#' 
#' @export
#' @param id A GenBankID
#' @param ... Curl args passed on to \code{\link[httr]{GET}}
#' @examples \donttest{
#' genank2uid(id = 'AJ748748')
#' }
genank2uid <- function(id, ...){
  url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
  bb <- GET(url, query=list(db='nuccore', retmode="text", rettype="xml", id=id))
  cc <- xmlParse(content(bb, "text"))
  uid <- xpathSApply(cc, '//Object-id_id', xmlValue)
  as.uid(uid)
}

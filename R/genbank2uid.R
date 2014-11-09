#' Get NCBI taxonomy UID from GenBankID
#' 
#' @export
#' @param id A GenBank accession alphanumeric string, or a gi numeric string.
#' @param ... Curl args passed on to \code{\link[httr]{GET}}
#' @details See \url{http://www.ncbi.nlm.nih.gov/Sitemap/sequenceIDs.html} for help on why
#' there are two identifiers, and the difference between them.
#' @examples \donttest{
#' # giving accession numbers
#' genbank2uid(id = 'AJ748748')
#' genbank2uid(id = 'Y13155')
#' genbank2uid(id = 'X78312')
#' genbank2uid(id = 'KM495596')
#' 
#' # gi numbers
#' genbank2uid(id = 62689767)
#' genbank2uid(id = 22775511)
#' genbank2uid(id = 156446673)
#' }
genbank2uid <- function(id, ...){
  if(is_acc(id)){
    url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
    bb <- GET(url, query=list(db='nuccore', retmode="text", rettype="fasta", id=id))
    res <- content(bb, "text")
    id <- str_extract(res, "[0-9]+")
  }
  url2 <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=nuccore&db=taxonomy&id="
  uid <- xpathSApply(content(GET(paste0(url2, id))), "//LinkSetDb//Link//Id", xmlValue)
  as.uid(uid)
}
# genbank2uid <- function(id, ...){
#   url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
#   bb <- GET(url, query=list(db='nuccore', retmode="text", rettype="xml", id=id))
#   cc <- xmlParse(content(bb, "text"))
#   uid <- xpathSApply(cc, '//Object-id_id', xmlValue)
#   as.uid(uid)
# }

is_acc <- function(x){
  gg <- suppressWarnings(as.numeric(x))
  is.na(gg)
}

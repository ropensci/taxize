#' Ping an API used in taxize to see if it's working.
#'
#' @name ping
#' @param what (character) One of status (default), content, or an HTTP status code. If status,
#' we just check that the HTTP status code is 200, or similar signifying the service is up.
#' If content, we do a simple, quick check to determine if returned content matches what's
#' expected. If an HTTP status code, it must match an appropriate code.
#' See \code{\link{status_codes}}.
#' @param ... Curl options passed on to \code{\link[httr]{GET}}
#' @return A logical, TRUE or FALSE
#' @details For ITIS, see \code{\link{getdescription}}, which provides number of
#' scientific and common names in a character string.
#' @examples \dontrun{
#' col_ping()
#' col_ping("content")
#' col_ping(200)
#' col_ping("200")
#' col_ping(204)
#'
#' itis_ping()
#' eol_ping()
#' ncbi_ping()
#' tropicos_ping()
#' nbn_ping()
#'
#' gbif_ping()
#' gbif_ping(200)
#' ubio_ping()
#'
#' bold_ping()
#' bold_ping(200)
#' bold_ping("content")
#'
#' ipni_ping()
#' ipni_ping(200)
#' ipni_ping("content")
#'
#' vascan_ping()
#' vascan_ping(200)
#' vascan_ping("content")
#' }

#' @export
#' @rdname ping
col_ping <- function(what = "status", ...) {
  res <- GET('http://www.catalogueoflife.org/col/webservice?name=Apis', ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("Apis", xmlToList(content(res))$result$name, ignore.case = TRUE))
}

#' @export
#' @rdname ping
eol_ping <- function(what = "status", ...) {
  res <- GET('http://eol.org/api/ping', ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("success", xmlToList(content(res))$message, ignore.case = TRUE))
}

#' @export
#' @rdname ping
itis_ping <- function(what = "status", ...) {
  res <- GET(paste0(itbase(), "getDescription"), ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = {
           tt <- xmlToList(content(res))$return$description
           grepl("this is the itis web service", tt, ignore.case = TRUE)
         })
}

#' @export
#' @rdname ping
ncbi_ping <- function(what = "status", ...) {
  res <- GET("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy&ID=4232", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("4232", xmlToList(content(res))$Taxon$TaxId, ignore.case = TRUE))
}

#' @export
#' @rdname ping
tropicos_ping <- function(what = "status", ...) {
  res <- GET("http://services.tropicos.org/Name/25509881?apikey=f3e499d4-1519-42c9-afd1-685a16882f5a&format=json", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("25509881", content(res)$NameId, ignore.case = TRUE))
}

#' @export
#' @rdname ping
nbn_ping <- function(what = "status", ...) {
  res <- GET("https://data.nbn.org.uk/api/search/taxa?=blackbird", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = content(res)$header$rows == 25)
}

#' @export
#' @rdname ping
gbif_ping <- function(what = "status", ...) {
  res <- GET("http://api.gbif.org/v1/species/1", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("1", content(res)$key, ignore.case = TRUE))
}

#' @export
#' @rdname ping
ubio_ping <- function(what = "status", ...) {
  res <- GET("http://www.ubio.org/webservices/service.php?function=namebank_object&namebankID=2483153&keyCode=b052625da5f330e334471f8efe725c07bf4630a6", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("2483153", xpathApply(content(res), "//namebankID", xmlValue)[[1]], ignore.case = TRUE))
}

#' @export
#' @rdname ping
bold_ping <- function(what = "status", ...) {
  res <- GET("http://www.boldsystems.org/index.php/API_Tax/TaxonData?taxId=88899&dataTypes=basic&includeTree=FALSE", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("88899", jsonlite::fromJSON(content(res, "text"))[[1]]$taxid, ignore.case = TRUE))
}

#' @export
#' @rdname ping
ipni_ping <- function(what = "status", ...) {
  res <- GET("http://www.ipni.org/ipni/advPlantNameSearch.do?find_genus=Brintonia&output_format=delimited-minimal", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = {
           txt <- content(res, "text")
           dat <- read.delim(text=txt, sep="%", stringsAsFactors=FALSE)
           grepl("Asteraceae", dat$Family[1], ignore.case = TRUE)
        })
}

#' @export
#' @rdname ping
vascan_ping <- function(what = "status", ...) {
  res <- GET("http://data.canadensys.net/vascan/api/0.1/search.json?q=Crataegus")
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("Crataegus", content(res)$results[[1]]$searchedTerm, ignore.case = TRUE)
  )
}


matchwhat <- function(x){
  x <- as.character(x)
  if( x %in% c("status", "content") ) x else "code"
}

match_status <- function(x){
  stopifnot(is(x, "response"))
  x$status_code == 200
}

match_code <- function(x, y){
  stopifnot(is(x, "response"))
  x$status_code == y
}

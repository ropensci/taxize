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
#'
#' # curl options
#' library("httr")
#' vascan_ping(config=verbose())
#' eol_ping(500, config=verbose())
#' }

#' @export
#' @rdname ping
col_ping <- function(what = "status", ...) {
  res <- GET('http://www.catalogueoflife.org/col/webservice?name=Apis', ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = identical("Apis",
           xml_text(xml_find_one(xml_children(xml2::read_xml(con_utf8(res)))[[1]], "name")))
  )
}

#' @export
#' @rdname ping
eol_ping <- function(what = "status", ...) {
  res <- GET('http://eol.org/api/ping', ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("success", xml2::xml_text(xml2::read_xml(con_utf8(res))),
                         ignore.case = TRUE))
}

#' @export
#' @rdname ping
itis_ping <- function(what = "status", ...) {
  res <- GET(paste0(itbase(), "getDescription"), ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = {
           tt <- xml_text(xml2::xml_children(xml2::read_xml(con_utf8(res)))[[1]])
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
         content = identical("4232",
                             xml_text(xml_find_one(xml_children(read_xml(con_utf8(res)))[[1]], "TaxId")))
  )
}

#' @export
#' @rdname ping
tropicos_ping <- function(what = "status", ...) {
  res <- GET("http://services.tropicos.org/Name/25509881?apikey=f3e499d4-1519-42c9-afd1-685a16882f5a&format=json", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl(25509881, jsonlite::fromJSON(con_utf8(res))$NameId))
}

#' @export
#' @rdname ping
nbn_ping <- function(what = "status", ...) {
  res <- GET("https://data.nbn.org.uk/api/search/taxa?=blackbird", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = jsonlite::fromJSON(con_utf8(res))$header$rows == 25)
}

#' @export
#' @rdname ping
gbif_ping <- function(what = "status", ...) {
  res <- GET("http://api.gbif.org/v1/species/1", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("1", jsonlite::fromJSON(con_utf8(res))$key, ignore.case = TRUE))
}

#' @export
#' @rdname ping
bold_ping <- function(what = "status", ...) {
  res <- GET("http://www.boldsystems.org/index.php/API_Tax/TaxonData?taxId=88899&dataTypes=basic&includeTree=FALSE", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("88899", jsonlite::fromJSON(con_utf8(res))[[1]]$taxid, ignore.case = TRUE))
}

#' @export
#' @rdname ping
ipni_ping <- function(what = "status", ...) {
  res <- GET("http://www.ipni.org/ipni/advPlantNameSearch.do?find_genus=Brintonia&output_format=delimited-minimal", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = {
           txt <- con_utf8(res)
           dat <- read.delim(text = txt, sep = "%", stringsAsFactors = FALSE)
           grepl("Asteraceae", dat$Family[1], ignore.case = TRUE)
        })
}

#' @export
#' @rdname ping
vascan_ping <- function(what = "status", ...) {
  res <- GET("http://data.canadensys.net/vascan/api/0.1/search.json?q=Crataegus", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("Crataegus", jsonlite::fromJSON(con_utf8(res))$results[[1]], ignore.case = TRUE)
  )
}

#' @export
#' @rdname ping
fg_ping <- function(what = "status", ...) {
  res <- GET("http://www.indexfungorum.org/ixfwebservice/fungus.asmx/NameFullByKey?NameLsid=urn:lsid:indexfungorum.org:names:17703", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("Gymnopus", xml_text(read_xml(con_utf8(res))), ignore.case = TRUE)
  )
}


matchwhat <- function(x){
  x <- as.character(x)
  if ( x %in% c("status", "content") ) x else "code"
}

match_status <- function(x){
  stopifnot(is(x, "response"))
  x$status_code == 200
}

match_code <- function(x, y){
  stopifnot(is(x, "response"))
  x$status_code == y
}

#' uBio ping
#'
#' @export
#' @rdname ubio_ping-defunct
ubio_ping <- function() {
  .Defunct(msg = "the uBio API is down, for good as far as we know")
}

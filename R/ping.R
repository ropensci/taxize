#' Ping an API used in taxize to see if it's working.
#'
#' @name ping
#' @param what (character) One of status (default), content, or an HTTP status
#' code. If status, we just check that the HTTP status code is 200, or similar
#' signifying the service is up. If content, we do a simple, quick check to
#' determine if returned content matches what's expected. If an HTTP status
#' code, it must match an appropriate code. See \code{\link{status_codes}}.
#' @param key (character) NCBI Entrez API key. optional. See 
#' \code{\link{get_uid}}
#' @param ... Curl options passed on to \code{\link[crul]{verb-GET}}
#' @return A logical, TRUE or FALSE
#' @details For ITIS, see \code{\link[ritis]{description}}, which provides
#' number of scientific and common names in a character string.
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
#' vascan_ping(verbose = TRUE)
#' eol_ping(500, verbose = TRUE)
#' }

#' @export
#' @rdname ping
col_ping <- function(what = "status", ...) {
  res <- pGET('http://www.catalogueoflife.org/col/webservice?name=Apis', ...)
  switch(
    matchwhat(what),
    status = match_status(res),
    code = match_code(res, what),
    content = {
      identical(
        "Apis",
        xml_text(xml_find_first(xml_children(xml2::read_xml(res$parse("UTF-8")))[[1]], "name")))
    }
  )
}

#' @export
#' @rdname ping
eol_ping <- function(what = "status", ...) {
  res <- pGET('https://eol.org/api/ping/1.0.json', ...)
  switch(
    matchwhat(what),
    status = match_status(res),
    code = match_code(res, what),
    content = grepl("success", 
      jsonlite::fromJSON(res$parse("UTF-8"))$response$message,
      ignore.case = TRUE)
  )
}

#' @export
#' @rdname ping
itis_ping <- function(what = "status", ...) {
  res <- pGET("http://www.itis.gov/ITISWebService/services/ITISService/getDescription", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = {
           tt <- xml_text(xml2::xml_children(xml2::read_xml(res$parse("UTF-8")))[[1]])
           grepl("this is the itis web service", tt, ignore.case = TRUE)
         })
}

#' @export
#' @rdname ping
ncbi_ping <- function(what = "status", key = NULL, ...) {
  key <- getkey(key, "ENTREZ_KEY")
  args <- tc(list(api_key = key))
  res <- pGET(paste0(ncbi_base(), 
    "/entrez/eutils/efetch.fcgi?db=taxonomy&ID=4232"), args = args, ...)
  switch(
    matchwhat(what),
    status = match_status(res),
    code = match_code(res, what),
    content = identical("4232",
      xml_text(xml_find_first(
        xml_children(read_xml(res$parse("UTF-8")))[[1]], "TaxId")))
  )
}

#' @export
#' @rdname ping
tropicos_ping <- function(what = "status", ...) {
  res <- pGET("http://services.tropicos.org/Name/25509881?apikey=f3e499d4-1519-42c9-afd1-685a16882f5a&format=xml", ...)
  error = FALSE
  if (grepl("exception occurred", res$parse("UTF-8"), ignore.case = TRUE)) error = TRUE
  switch(
    matchwhat(what),
    status = if (error) TRUE else match_status(res),
    code = if (error) TRUE else match_code(res, what),
    content = {
      if (error)
        TRUE
      else
        grepl(
          25509881,
          xml2::as_list( xml2::read_xml(res$parse("UTF-8")))$Name$NameId[[1]]
        )
    }
  )
}

#' @export
#' @rdname ping
nbn_ping <- function(what = "status", ...) {
  res <- pGET("https://species-ws.nbnatlas.org/species/NHMSYS0000502940", ...)
  switch(
    matchwhat(what),
    status = match_status(res),
    code = match_code(res, what),
    content = {
      jsonlite::fromJSON(res$parse("UTF-8"))$taxonConcept$guid == 'NHMSYS0000502940'
    }
  )
}

#' @export
#' @rdname ping
gbif_ping <- function(what = "status", ...) {
  res <- pGET("http://api.gbif.org/v1/species/1", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("1", jsonlite::fromJSON(res$parse("UTF-8"))$key, 
          ignore.case = TRUE))
}

#' @export
#' @rdname ping
bold_ping <- function(what = "status", ...) {
  res <- pGET("http://v4.boldsystems.org/index.php/API_Tax/TaxonData?taxId=88899&dataTypes=basic&includeTree=FALSE", ...)
  switch(
    matchwhat(what),
    status = match_status(res),
    code = match_code(res, what),
    content = grepl("88899", jsonlite::fromJSON(res$parse("UTF-8"))$taxid,
                    ignore.case = TRUE)
  )
}

#' @export
#' @rdname ping
ipni_ping <- function(what = "status", ...) {
  res <- pGET("http://www.ipni.org/ipni/advPlantNameSearch.do?find_genus=Brintonia&output_format=delimited-minimal", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = {
           txt <- res$parse("UTF-8")
           dat <- read.delim(text = txt, sep = "%", 
            stringsAsFactors = FALSE)
           grepl("Asteraceae", dat$Family[1], ignore.case = TRUE)
        })
}

#' @export
#' @rdname ping
vascan_ping <- function(what = "status", ...) {
  res <- pGET("http://data.canadensys.net/vascan/api/0.1/search.json?q=Crataegus", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("Crataegus", jsonlite::fromJSON(res$parse("UTF-8"))$results[[1]], 
          ignore.case = TRUE)
  )
}

#' @export
#' @rdname ping
fg_ping <- function(what = "status", ...) {
  res <- pGET("http://www.indexfungorum.org/ixfwebservice/fungus.asmx/NameFullByKey?NameLsid=urn:lsid:indexfungorum.org:names:17703", ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = grepl("Gymnopus", xml_text(read_xml(res$parse("UTF-8"))), 
          ignore.case = TRUE)
  )
}


matchwhat <- function(x){
  x <- as.character(x)
  if ( x %in% c("status", "content") ) x else "code"
}

match_status <- function(x){
  stopifnot(inherits(x, "HttpResponse"))
  x$status_code == 200
}

match_code <- function(x, y){
  stopifnot(inherits(x, "HttpResponse"))
  x$status_code == y
}

pGET <- function(url, args = list(), ...) {
  cli <- crul::HttpClient$new(url, headers = tx_ual, opts = list(...))
  cli$get(query = args)
}

#' uBio ping
#'
#' @export
#' @rdname ubio_ping-defunct
ubio_ping <- function() {
  .Defunct(msg = "the uBio API is down, for good as far as we know")
}

tax_GET <- function(url, path = NULL, query = list(), headers = list(),
  opts = list(), ...) {

  cli <- crul::HttpClient$new(url,
    headers = c(headers, tx_ual), opts = c(opts, list(...)))
  out <- cli$get(path = path, query = query)
  out <- tax_error_handle(out)
  return(out)
}

tax_GET_nocheck <- function(url, path = NULL, query = list(), headers = list(),
  opts = list(), ...) {

  cli <- crul::HttpClient$new(url,
    headers = c(headers, tx_ual), opts = c(opts, list(...)))
  out <- cli$get(path = path, query = query)
  return(out)
}

tax_POST <- function(url, path = NULL, query = list(), body = list(),
  headers = list(), opts = list(), ...) {

  cli <- crul::HttpClient$new(url,
    headers = c(headers, tx_ual), opts = c(opts, list(...)))
  out <- cli$post(path = path, query = query, body = body)
  tax_error_handle(out)
  return(out)
}

tax_error_handle <- function(x) {
  x$raise_for_status()
  txt <- x$parse("utf-8")
  xml <- tryCatch(xml2::read_xml(txt), error = function(e) e)
  json <- tryCatch(jsonlite::fromJSON(txt), error = function(e) e)
  if (x$success() && !inherits(json, "error")) return(json)
  if (inherits(xml, "error")) {
    message("running xml cntrl char replacement")
    strg <- "[[:cntrl:]]" # control characters
    xml <- tryCatch(xml2::read_xml(gsub(strg, "", txt)),
      error = function(e) e)
  }
  if (inherits(xml, "error")) stop("error parsing xml")
  errmssg <- tryCatch(xml2::xml_attr(xml, "error_message"),
    error = function(e) e)
  if (inherits(errmssg, "error")) stop("error parsing xml")
  if (nzchar(errmssg)) stop(errmssg)
  return(xml)
}

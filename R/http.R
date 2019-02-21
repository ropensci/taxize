tax_GET <- function(url, path = NULL, query = list(), headers = list(), 
  opts = list(), ...) {

  cli <- crul::HttpClient$new(url, headers = headers, 
    opts = c(opts, list(...)))
  out <- cli$get(path = path, query = query)
  out$raise_for_status()
  return(out)
}

tax_POST <- function(url, path = NULL, query = list(), body = list(),
  headers = list(), opts = list(), ...) {

  cli <- crul::HttpClient$new(url, headers = headers, 
    opts = c(opts, list(...)))
  out <- cli$post(path = path, query = query, body = body)
  out$raise_for_status()
  return(out)
}

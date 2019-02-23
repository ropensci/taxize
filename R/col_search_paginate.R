#' col_search wrapper to iterate to get more than default results
#' 
#' @keywords internal
#' @inheritParams col_search
#' @examples
#' # col_search_paginate(name = "Poa", response = "full")
col_search_paginate <- function(name = NULL, id = NULL, start = NULL,
  checklist = NULL, response = "terse", ...) {

  res <- col_search(name = name, id = id, start = start, checklist = checklist, 
    response = response, ...)

  lapply(res, function(v) {
    num_found <- attr(v, "total_number_of_results")
    if (num_found <= NROW(v)) return(v)
    num_per <- switch(response, terse = 500, full = 50)
    starts <- seq(NROW(v), round_up(num_found - NROW(v), 50),
      by = 50)
    url <- make_url(checklist)
    args <- tc(list(response = response, format = "json", name = name, id = id))
    reqs <- lapply(starts, function(w) {
      args$start <- w
      crul::HttpRequest$new(
        url = url,
        headers = tx_ual
      )$get(query = args)
    })
    out <- crul::AsyncVaried$new(.list = reqs)
    out$request()
    jsons <- out$parse()
    jsons <- jsons[out$status_code() == 200]
    dt2tibble(lapply(jsons, function(w) {
      tt <- jsonlite::fromJSON(w, FALSE)
      switch(
        response,
        terse = col_meta(parse_terse(tt), tt),
        full = col_meta(parse_full(tt), tt)
      )
    }))
  })

}

# from https://stackoverflow.com/a/6461708/1091766
round_up <- function(x, round = 10) {
  ceiling(max(x + 10^-9)/round + 1/round) * round
}

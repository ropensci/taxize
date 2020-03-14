#' @title ITIS hierarchy
#'
#' @description Get hierarchies from TSN values, full, upstream only, or
#' immediate downstream only
#'
#' @export
#' @param tsn One or more TSN's (taxonomic serial number). Required.
#' @param what One of full (full hierarchy), up (immediate upstream), or down
#'    (immediate downstream)
#' @param ... Further arguments passed on to [ritis::hierarchy_full()]
#' [ritis::hierarchy_up()] or [ritis::hierarchy_down()]
#' @seealso [itis_downstream()]
#' @details Note that [itis_downstream()] gets taxa downstream to a
#' particular rank, while this function only gets immediate names downstream.
#' @examples \dontrun{
#' # Get full hierarchy
#' itis_hierarchy(tsn=180543)
#'
#' # Get hierarchy upstream
#' itis_hierarchy(tsn=180543, "up")
#'
#' # Get hierarchy downstream
#' itis_hierarchy(tsn=180543, "down")
#'
#' # Many tsn's
#' itis_hierarchy(tsn=c(180543,41074,36616))
#' }

itis_hierarchy <- function(tsn, what = "full", ...) {
  temp <- switch(
    what,
    full = lapply(tsn, function(x) ritis::hierarchy_full(x, ...)),
    up = lapply(tsn, function(x) ritis::hierarchy_up(x, ...)),
    down = lapply(tsn, function(x) ritis::hierarchy_down(x, ...))
  )
  if (length(tsn) == 1) {
    tmp <- temp[[1]]
    names(tmp) <- tolower(names(tmp))
    tmp$rankname <- tolower(tmp$rankname)
    tmp
  } else {
    names(temp) <- tsn
    lapply(temp, function(x){
      names(x) <- tolower(names(x))
      x$rankname <- tolower(x$rankname)
      x
    })
  }
}

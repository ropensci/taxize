#' Parse scientific names using EOL's name parser.
#'
#' @export
#' @param names A vector of length 1 or more of taxonomic names
#' @param ... Curl options passed on to [crul::verb-GET]
#' @return A data.frame with results, the submitted names, and the
#' parsed names with additional information.
#' @seealso [gbif_parse()]
#' @references <http://gni.globalnames.org/>
#' @examples \dontrun{
#' gni_parse("Cyanistes caeruleus")
#' gni_parse("Plantago minor")
#' gni_parse("Plantago minor minor")
#' gni_parse(c("Plantago minor minor","Helianthus annuus texanus"))
#'
#' # pass on curl options
#' gni_parse("Cyanistes caeruleus", verbose = TRUE)
#' }
gni_parse <- function(names, ...) {
  names <- paste0(names, collapse = "|")
  cli <- crul::HttpClient$new(paste0(gni_base(), "parsers.json"),
    headers = tx_ual, opts = list(...))
  tt <- cli$get(query = list(names = names))
  tt$raise_for_status()
  out <- jsonlite::fromJSON(tt$parse("UTF-8"), FALSE)
  dt2df(lapply(out, gni_parser), idcol = FALSE)
}

gni_parser <- function(x) {
  positions_names <- vapply(x$scientificName$positions, function(y)
    paste("position_", y[[1]], sep = ""), "", USE.NAMES = FALSE)
  nums <- vapply(x$scientificName$positions, function(y) y[[2]], 1,
    USE.NAMES = FALSE)
  pv <- data.frame(as.list(setNames(nums, positions_names)),
    stringsAsFactors = FALSE)

  singles <- data.frame(x$scientificName[c("verbatim","canonical",
    "normalized","hybrid","parsed")], stringsAsFactors = FALSE)
  details_ <- x$scientificName$details[[1]]
  details_ <- details_[!names(details_) %in% 'status']
  details <- dt2df(Map(function(x, y) data.frame(y, x,
    stringsAsFactors = FALSE), details_, names(details_)),
    idcol = FALSE)[,-3]
  details2 <- as.data.frame(t(data.frame(details[,2])))
  names(details2) <- details[,1]
  row.names(details2) <- NULL
  data.frame(details2, singles, pv, stringsAsFactors = FALSE)
}

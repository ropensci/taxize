#' Parse scientific names using EOL's name parser.
#'
#' @param names A vector of length 1 or more of taxonomic names
#' @return A data.frame with results, the submitted names, and the parsed names
#'    with additional information.
#' @seealso \code{\link{gbif_parse}}
#' @references \url{http://gni.globalnames.org/}
#' @export
#' @examples \dontrun{
#' gni_parse("Cyanistes caeruleus")
#' gni_parse("Plantago minor")
#' gni_parse("Plantago minor minor")
#' gni_parse(c("Plantago minor minor","Helianthus annuus texanus"))
#' }
gni_parse <- function(names)
{
  url <- 'http://gni.globalnames.org/parsers.json'
  names <- paste0(names, collapse="|")
  out <- content(GET(url, query = list(names=names)))

  parser <- function(x){
    positions_names <- laply(x$scientificName$positions, function(y) paste("position_", y[[1]], sep="") )
    positions_values <- data.frame(t(data.frame(laply(x$scientificName$positions, function(y) y[[2]]))))
    row.names(positions_values) <- NULL
    names(positions_values) <- positions_names

    singles <- data.frame(x$scientificName[c("verbatim","canonical","normalized","hybrid","parsed")])
    details_ <- x$scientificName$details[[1]]
    details_ <- details_[!names(details_) %in% 'status']
    details <- ldply(details_, function(x) as.data.frame(x))[,-3]
    details2 <- as.data.frame(t(data.frame(details[,2])))
    names(details2) <- details[,1]
    row.names(details2) <- NULL
    data.frame(details2, singles, positions_values)
  }

  ldply(out, parser)
}

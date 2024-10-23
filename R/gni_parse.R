#' Parse scientific names using EOL's name parser.
#'
#' THIS FUNCTION IS DEFUNCT.
#' 
#' @export
#' @keywords internal
gni_parse <- function(names, ...) {
  .Defunct("ncbi_searcher", "traits",
           msg = "This function is defunct. See gn_parse()")
  # names <- paste0(names, collapse = "|")
  # url <- paste0("https://parser.globalnames.org/api/v1/", names)
  # cli <- crul::HttpClient$new(url, headers = tx_ual, opts = list(...))
  # tt <- cli$get(query = list(cultivars = cultivars, csv = TRUE))
  # tt$raise_for_status()
  # out <- jsonlite::fromJSON(tt$parse("UTF-8"), FALSE)
  # dt2df(lapply(out, gni_parser), idcol = FALSE)
}

# gni_parser <- function(x) {
#   positions_names <- vapply(x$scientificName$positions, function(y)
#     paste("position_", y[[1]], sep = ""), "", USE.NAMES = FALSE)
#   nums <- vapply(x$scientificName$positions, function(y) y[[2]], 1,
#     USE.NAMES = FALSE)
#   pv <- data.frame(as.list(setNames(nums, positions_names)),
#     stringsAsFactors = FALSE)
# 
#   nmz <- c("verbatim","canonical", "normalized","hybrid","parsed")
#   singles <- data.frame(x$scientificName[names(x$scientificName) %in% nmz],
#     stringsAsFactors = FALSE)
#   
#   details2 <- data.frame()
#   if (x$scientificName$parsed) {
#     details_ <- x$scientificName$details[[1]]
#     details_ <- details_[!names(details_) %in% 'status']
#     details <- dt2df(Map(function(x, y) data.frame(y, x,
#       stringsAsFactors = FALSE), details_, names(details_)),
#       idcol = FALSE)[,-3]
#     details2 <- as.data.frame(t(data.frame(details[,2])))
#     names(details2) <- details[,1]
#     row.names(details2) <- NULL
#   }
#   
#   data.frame(Filter(NROW, list(details2, singles, pv)),
#     stringsAsFactors = FALSE)
# }

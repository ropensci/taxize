# get name from TOL ID
# tol_id2name(id = 515698)
tol_id2name <- function(id, ...) {
  cli <- crul::HttpClient$new("https://api.opentreeoflife.org", 
    headers = tx_ual, opts = list(...))
  res <- cli$post(path = "v3/taxonomy/taxon_info", encode = "json",
    body = list(ott_id = id))
  if (res$status_code > 201) {
    warning(res$status_code, ": ", res$status_http()$message, 
      ", returning empty data.frame for ", id, 
      call. = FALSE)
    return(id2name_blanks$tol)
  }
  tmp <- jsonlite::fromJSON(res$parse("UTF-8"))
  tmp[vapply(tmp, length, 1) == 0] <- NULL
  if ("tax_sources" %in% names(tmp)) {
    if (length(tmp$tax_sources) > 1) {
      ids <- lapply(tmp$tax_sources, function(z) {
        m <- strsplit(z, ":")[[1]]
        as.list(stats::setNames(m[2], paste0("tax_sources_", m[1])))
      })
      tmp$tax_sources <- NULL
      tmp <- c(tmp, unlist(ids, FALSE))
    }
  }
  tmp$unique_name <- NULL
  tmp$is_suppressed <- NULL
  names(tmp)[names(tmp) %in% "ott_id"] <- "id"
  tmp <- tmp[c('id', 'name', 'rank', grep("tax_", names(tmp), value = TRUE))]
  data.frame(tmp, stringsAsFactors = FALSE)
}

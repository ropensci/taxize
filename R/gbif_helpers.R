# name backbone
gbif_name_backbone <- function(name, rank = NULL, kingdom = NULL, phylum = NULL,
  class = NULL, order = NULL, family = NULL, genus = NULL, strict = FALSE,
  start = NULL, limit = 500, ...) {


  url = 'https://api.gbif.org/v1/species/match'
  args <- tc(list(name = name, rank = rank, kingdom = kingdom,
                  phylum = phylum, class = class, order = order, family = family,
                  genus = genus, strict = strict, verbose = TRUE, offset = start,
                  limit = limit))
  cli <- crul::HttpClient$new(url = url, headers = tx_ual)
  temp <- cli$get(query = args, ...)
  temp$raise_for_status()
  tt <- jsonlite::fromJSON(temp$parse("UTF-8"), FALSE)

  if (all(names(tt) %in% c('confidence', 'synonym', 'matchType', "note"))) {
    data.frame(NULL)
  } else {
    dd <- data.table::setDF(
      data.table::rbindlist(
        lapply(tt$alternatives,
               function(x) lapply(x, function(x) 
                if (length(x) == 0) NA else x)), use.names = TRUE, fill= TRUE))
    dat <- data.frame(tt[!names(tt) %in% c("alternatives",
                                           "note")], stringsAsFactors = FALSE)
    if (!all(names(dat) %in% c('confidence', 'synonym', 'matchType'))) {
      dd <- dt2df(list(dat, dd), idcol = FALSE)
    }
    if (limit > 0) {
      dd <- cols_move(dd, back_cols_use)
    }
    if (!is.null(dd)) dd$rank <- tolower(dd$rank)
    names(dd) <- tolower(names(dd))
    return(dd)
  }
}

# name lookup
gbif_name_lookup <- function(query = NULL, rank = NULL, 
  higherTaxonKey = NULL, status = NULL, nameType = NULL, 
  datasetKey = 'd7dddbf4-2cf0-4f39-9b2a-bb099caae36c', limit = 500, 
  start = NULL, ...) {

  url = 'https://api.gbif.org/v1/species/search'
  args <- tc(list(q = query, rank = rank, higherTaxonKey = higherTaxonKey,
                  status = status, nameType = nameType, datasetKey = datasetKey,
                  limit = limit, offset = start))
  cli <- crul::HttpClient$new(url = url, headers = tx_ual)
  temp <- cli$get(query = args, ...)
  temp$raise_for_status()
  tt <- jsonlite::fromJSON(temp$parse("UTF-8"), FALSE)
  dd <- data.table::setDF(
    data.table::rbindlist(lapply(
      tt$results,
      nlkupcleaner), use.names = TRUE, fill = TRUE)
  )
  if (limit > 0) {
    dd <- cols_move(dd, gbif_cols_use)
  }
  if (!is.null(dd)) dd$rank <- tolower(dd$rank)
  names(dd) <- tolower(names(dd))
  return(dd)
}

cols_move <- function (x, cols)  {
  other <- names(x)[!names(x) %in% cols]
  x[, c(cols, other)]
}

nlkupcleaner <- function (x) {
  tmp <- x[!names(x) %in% c("descriptions", "vernacularNames", 
    "higherClassificationMap")]
  lapply(tmp, function(x) {
    if (length(x) == 0) {
      NA
    }
    else if (length(x) > 1 || is(x, "list")) {
      paste0(x, collapse = ", ")
    }
    else {
      x
    }
  })
}

gbif_cols_use <- c("key", "canonicalName", "authorship", "rank", 
  "taxonomicStatus", "synonym")
back_cols_use <- c("usageKey", "scientificName", "rank", "status", "matchType")
gbif_cols_show_backbone <- tolower(c("gbifid", "scientificName", "rank", 
  "status", "matchtype"))
gbif_cols_show_lookup <- tolower(c("gbifid", "canonicalName", "authorship", 
  "rank", "taxonomicStatus", "synonym"))

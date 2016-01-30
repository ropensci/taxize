#' Retrieve the taxonomic hierarchy for a given taxon ID.
#'
#' @export
#' @param x Vector of taxa names (character) or IDs (character or numeric) to query.
#' @param db character; database to query. either \code{ncbi}, \code{itis},
#'    \code{eol}, \code{col}, \code{tropicos}, \code{gbif}, or \code{nbn}. Note
#'    that each taxonomic data source has their own identifiers, so that if you
#'    provide the wrong \code{db} value for the identifier you could get a result,
#'    but it will likely be wrong (not what you were expecting).
#' @param id character; identifiers, returned by \code{\link[taxize]{get_tsn}},
#'    \code{\link[taxize]{get_uid}}, \code{\link[taxize]{get_eolid}},
#'    \code{\link[taxize]{get_colid}}, \code{\link[taxize]{get_tpsid}},
#'    \code{\link[taxize]{get_gbifid}}.
#' @param callopts Curl options passed on to \code{\link[httr]{GET}}
#' @param ... Other arguments passed to \code{\link[taxize]{get_tsn}},
#'    \code{\link[taxize]{get_uid}}, \code{\link[taxize]{get_eolid}},
#'    \code{\link[taxize]{get_colid}}, \code{\link[taxize]{get_tpsid}},
#'    \code{\link[taxize]{get_gbifid}}.
#' @param start The first record to return. If omitted, the results are returned
#' 		from the first record (start=0). This is useful if the total number of
#' 		results is larger than the maximum number of results returned by a single
#' 		Web service query (currently the maximum number of results returned by a
#' 		single query is 500 for terse queries and 50 for full queries).
#' @param checklist character; The year of the checklist to query, if you want a specific
#' 		year's checklist instead of the lastest as default (numeric).
#' @param key Your API key; loads from .Rprofile.
#' @param return_id (logical) If TRUE (default), return the taxon id as well as the name
#' and rank of taxa in the lineage returned.
#' @param rows (numeric) Any number from 1 to inifity. If the default NA, all rows are
#' considered. Note that this parameter is ignored if you pass in a taxonomic id instead
#' of a name of class character.
#'
#' @return A named list of data.frames with the taxonomic classification of
#'    every supplied taxa.
#' @details If IDs are supplied directly (not from the \code{get_*} functions) you
#' must specify the type of ID. There is a timeout of 1/3 seconds between
#' querries to NCBI.
#'
#' BEWARE: Right now, NBN doesn't return the queried taxon in the classification. But you can
#' attach it yourself quite easily of course. This behavior is different from the other data
#' sources.
#'
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}},
#'    \code{\link[taxize]{get_eolid}}, \code{\link[taxize]{get_colid}},
#'    \code{\link[taxize]{get_tpsid}}, \code{\link[taxize]{get_gbifid}}
#'
#' @examples \dontrun{
#' # Plug in taxon IDs
#' classification(9606, db = 'ncbi')
#' classification(c(9606, 55062), db = 'ncbi')
#' classification(129313, db = 'itis')
#' classification(57361017, db = 'eol')
#' classification(c(2704179, 2441176), db = 'gbif')
#' classification(25509881, db = 'tropicos')
#' classification("NBNSYS0000004786", db = 'nbn')
#' ## works the same if IDs are in class character
#' classification(c("2704179", "2441176"), db = 'gbif')
#'
#' # Plug in taxon names
#' ## in this case, we use get_*() fxns internally to first get taxon IDs
#' classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi', verbose=FALSE)
#' classification(c("Chironomus riparius", "aaa vva"), db = 'itis')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'itis', verbose=FALSE)
#' classification(c("Chironomus riparius", "aaa vva"), db = 'eol')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'col')
#' classification("Alopias vulpinus", db = 'nbn')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'col', verbose=FALSE)
#' classification(c("Chironomus riparius", "asdfasdfsfdfsd"), db = 'gbif')
#' classification("Poa annua", db = 'tropicos')
#'
#' # Use methods for get_uid, get_tsn, get_eolid, get_colid, get_tpsid
#' classification(get_uid(c("Chironomus riparius", "Puma concolor")))
#'
#' classification(get_uid(c("Chironomus riparius", "aaa vva")))
#' classification(get_tsn(c("Chironomus riparius", "aaa vva")))
#' classification(get_tsn(c("Chironomus riparius", "aaa vva"), verbose = FALSE))
#' classification(get_eolid(c("Chironomus riparius", "aaa vva")))
#' classification(get_colid(c("Chironomus riparius", "aaa vva")))
#' classification(get_tpsid(c("Poa annua", "aaa vva")))
#' classification(get_gbifid(c("Poa annua", "Bison bison")))
#'
#' # Pass many ids from class "ids"
#' (out <- get_ids(names="Puma concolor", db = c('ncbi','gbif')))
#' (cl <- classification(out))
#'
#' # Bind width-wise from class classification_ids
#' cbind(cl)
#'
#' # Bind length-wise
#' rbind(cl)
#'
#' # Many names to get_ids
#' (out <- get_ids(names=c("Puma concolor","Accipiter striatus"), db = c('ncbi','itis','col')))
#' (cl <- classification(out))
#' rbind(cl)
#' ## cbind with so many names results in some messy data
#' cbind(cl)
#' ## so you can turn off return_id
#' cbind( classification(out, return_id=FALSE) )
#'
#' # rbind and cbind on class classification (from a call to get_colid, get_tsn, etc.
#' # - other than get_ids)
#' (cl_col <- classification(get_colid(c("Puma concolor","Accipiter striatus"))))
#' rbind(cl_col)
#' cbind(cl_col)
#'
#' (cl_uid <- classification(get_uid(c("Puma concolor","Accipiter striatus")), return_id=FALSE))
#' rbind(cl_uid)
#' cbind(cl_uid)
#' ## cbind works a bit odd when there are lots of ranks without names
#' (cl_uid <- classification(get_uid(c("Puma concolor","Accipiter striatus")), return_id=TRUE))
#' cbind(cl_uid)
#'
#' (cl_tsn <- classification(get_tsn(c("Puma concolor","Accipiter striatus"))))
#' rbind(cl_tsn)
#' cbind(cl_tsn)
#'
#' (tsns <- get_tsn(c("Puma concolor","Accipiter striatus")))
#' (cl_tsns <- classification(tsns))
#' cbind(cl_tsns)
#'
#' # NBN data
#' (res <- classification(c("Alopias vulpinus","Pinus sylvestris"), db = 'nbn'))
#' rbind(res)
#' cbind(res)
#'
#' # Return taxonomic IDs
#' ## the return_id parameter is logical, and you can turn it on or off. It's TRUE by default
#' classification(c("Alopias vulpinus","Pinus sylvestris"), db = 'ncbi', return_id = TRUE)
#' classification(c("Alopias vulpinus","Pinus sylvestris"), db = 'ncbi', return_id = FALSE)
#'
#' # Use rows parameter to select certain
#' classification('Poa annua', db = 'tropicos')
#' classification('Poa annua', db = 'tropicos', rows=1:4)
#' classification('Poa annua', db = 'tropicos', rows=1)
#' classification('Poa annua', db = 'tropicos', rows=6)
#' }
#'
#' @examples \dontrun{
#' # Fails
#' classification(315576)
#' }
classification <- function(...){
  UseMethod("classification")
}

#' @export
#' @rdname classification
classification.default <- function(x, db = NULL, callopts=list(), return_id = TRUE, rows = NA, ...){
  nstop(db)
  switch(db,
         itis = {
           id <- process_ids(x, db, get_tsn, rows = rows, ...)
           setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
         },
         ncbi = {
           id <- process_ids(x, db, get_uid, rows = rows, ...)
           setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
         },
         eol = {
           id <- process_ids(x, db, get_eolid, rows = rows, ...)
           setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
         },
         col = {
           id <- process_ids(x, db, get_colid, rows = rows, ...)
           setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
         },
         tropicos = {
           id <- process_ids(x, db, get_tpsid, rows = rows, ...)
           setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
         },
         gbif = {
           id <- process_ids(x, db, get_gbifid, rows = rows, ...)
           setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
         },
         nbn = {
           id <- process_ids(x, db, get_nbnid, rows = rows, ...)
           setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
         },
         stop("the provided db value was not recognised", call. = FALSE)
  )
}

process_ids <- function(input, db, fxn, ...){
  g <- tryCatch(as.numeric(as.character(input)), warning = function(e) e)
  if (is(g,"numeric") || is.character(input) && grepl("N[HB]", input)) {
    as_fxn <- switch(db,
           itis = as.tsn,
           ncbi = as.uid,
           eol = as.eolid,
           col = as.colid,
           tropicos = as.tpsid,
           gbif = as.gbifid,
           nbn = as.nbnid)
    as_fxn(input, check = FALSE)
  } else {
    eval(fxn)(input, ...)
  }
}

#' @export
#' @rdname classification
classification.tsn <- function(id, callopts = list(), return_id = TRUE, ...) {
  fun <- function(x, callopts){
    # return NA if NA is supplied
    if (is.na(x)) {
      out <- NA
    } else {
      out <- getfullhierarchyfromtsn(x, callopts, ...)
      if (NROW(out) < 1) return(NA)
      # remove overhang
      out <- out[1:which(out$tsn == x), c('taxonname', 'rankname', 'tsn')]
      names(out) <- c('name', 'rank', 'id')
      # Optionally return tsn of lineage
      if (!return_id) out <- out[, c('name', 'rank')]
      return(out)
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'itis')
}

#' @export
#' @rdname classification
classification.uid <- function(id, callopts = list(), return_id = TRUE, ...) {
  fun <- function(x, callopts){
    # return NA if NA is supplied
    if (is.na(x)) {
      out <- NA
    } else {
      baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy"
      ID <- paste("ID=", x, sep = "")
      searchurl <- paste(baseurl, ID, sep = "&")
      res <- GET(searchurl, callopts)
      stop_for_status(res)
      tt <- con_utf8(res)
      ttp <- xml2::read_xml(tt)
      out <- data.frame(name = xml2::xml_text(xml2::xml_find_all(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/ScientificName")),
                        rank = xml2::xml_text(xml2::xml_find_all(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/Rank")),
                        id = xml2::xml_text(xml2::xml_find_all(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/TaxId")),
                        stringsAsFactors = FALSE)
      out <- rbind(out, c(xml2::xml_text(xml2::xml_find_all(ttp, "//TaxaSet/Taxon/ScientificName")),
                          xml2::xml_text(xml2::xml_find_all(ttp, "//TaxaSet/Taxon/Rank")),
                          xml2::xml_text(xml2::xml_find_all(ttp, "//TaxaSet/Taxon/TaxId")))
      )
      # Optionally return tsn of lineage
      if (!return_id) out <- out[, c('name', 'rank')]
      return(out)
    }
    # NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(out)
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'ncbi')
}

#' @export
#' @rdname classification
classification.eolid <- function(id, key = NULL, callopts = list(), return_id = TRUE, ...) {
  common_names = synonyms = NULL
  fun <- function(x){
    if (is.na(x)) {
      out <- NA
    } else {
      url = 'http://eol.org/api/hierarchy_entries/1.0/'
      key <- getkey(key, "eolApiKey")
      urlget <- paste(url, x, '.json', sep = "")
      args <- tc(list(common_names = common_names, synonyms = synonyms))
      tt <- GET(urlget, query = args, callopts)
      stop_for_status(tt)
      res <- con_utf8(tt)
      if (length(res$ancestors) == 0) {
        return(sprintf("No hierarchy information for %s", x))
      } else {
        out <- do.call(rbind.fill, lapply(res$ancestors, data.frame, stringsAsFactors = FALSE))[,c('scientificName','taxonRank', 'taxonID')]
        # add querried taxon
        tr <- res$taxonRank
        out <- rbind(out, c(res$scientificName, if ( is.null(tr) ) NA else tr, x))
        names(out) <- c('name', 'rank', 'id')
        # Optionally return id of lineage
        if (!return_id) out <- out[, c('name', 'rank')]
        return(out)
      }
    }
  }
  out <- lapply(id, fun)
  names(out) <- id
  structure(out, class = 'classification', db = 'eol')
}

#' @export
#' @rdname classification
classification.colid <- function(id, start = NULL, checklist = NULL,
                                 callopts = list(), return_id = TRUE, ...) {
  fun <- function(x, checklist, start, callopts){
    # return NA if NA is supplied
    if (is.na(x)) {
      out <- NA
    } else {
      url <- make_url(checklist)
      args <- tc(list(id = x, response = "full", start = start))
      out <- GET(url, query = args, callopts)
      stop_for_status(out)
      tt <- xml2::read_xml(con_utf8(out))
      out <- search_col_classification_df(tt)
      # add query-ied taxon
      out <- rbind(out, c(xml2::xml_text(xml2::xml_find_one(tt, "//result/name")),
                          xml2::xml_text(xml2::xml_find_one(tt, "//result/rank")),
                          xml2::xml_text(xml2::xml_find_one(tt, "//result/id"))))
      # Optionally return id of lineage
      if (!return_id) out <- out[, c('name', 'rank')]
    }
    return(out)
  }
  out <- lapply(id, fun, checklist = checklist, start = start, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'col')
}

search_col_classification_df <- function(x) {
  name <- xml2::xml_text(xml2::xml_find_all(x, "//classification//name"))
  rank <- xml2::xml_text(xml2::xml_find_all(x, "//classification//rank"))
  ids <- xml2::xml_text(xml2::xml_find_all(x, "//classification//id"))
  data.frame(name, rank, ids, stringsAsFactors = FALSE)
}


#' @export
#' @rdname classification
classification.tpsid <- function(id, key = NULL, callopts = list(), return_id = TRUE, ...) {
  fun <- function(x, callopts){
    if (is.na(x)) {
      out <- NA
    } else {
      url <- sprintf('http://services.tropicos.org/Name/%s/HigherTaxa', x)
      key <- getkey(key, "tropicosApiKey")
      args <- tc(list(format = 'json', apikey = key))
      tt <- GET(url, query = args, callopts)
      stop_for_status(tt)
      out <- jsonlite::fromJSON(con_utf8(tt), FALSE)
      if (names(out[[1]])[[1]] == "Error") {
        out <- data.frame(ScientificName = NA, Rank = NA)
      } else {
        out <- do.call(rbind.fill, lapply(out, data.frame))[,c('ScientificName','Rank', 'NameId')]
      }
      names(out) <- c('name', 'rank', 'id')
      # Optionally return id of lineage
      if (!return_id) out <- out[, c('name', 'rank')]
    }
    return(out)
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'tropicos')
}

#' @export
#' @rdname classification
classification.gbifid <- function(id, callopts = list(), return_id = TRUE, ...) {
  fun <- function(x, callopts){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- suppressWarnings(tryCatch(gbif_name_usage(key = x, callopts = callopts), error = function(e) e))
      if (is(out, "simpleError")) {
        NA
      } else {
        nms <- ldply(out[c('kingdom','phylum','class','order','family','genus','species')])
        keys <- unname(unlist(out[paste0(c('kingdom','phylum','class','order','family','genus','species'), "Key")]))
        df <- data.frame(name = nms$V1, rank = nms$.id, id = keys)

        # Optionally return id of lineage
        if (!return_id) df[, c('name', 'rank')] else df
      }
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'gbif')
}

#' @export
#' @rdname classification
classification.nbnid <- function(id, callopts = list(), return_id = TRUE, ...) {
  fun <- function(x, callopts){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- suppressWarnings(tryCatch(nbn_classification(id = x, callopts), error = function(e) e))
      if (is(out, "simpleError")) {
        NA
      } else {
        out <- out[ , c('name','rank', 'taxonversionkey')]
        names(out) <- c('name', 'rank', 'id')
        # Optionally return id of lineage
        if (!return_id) out <- out[, c('name', 'rank')]
        return(out)
      }
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'nbn')
}

#' @export
#' @rdname classification
classification.ids <- function(id, ...) {
  fun <- function(x, ...){
    # return NA if NA is supplied
    if (all(is.na(x))) {
      out <- NA
    } else {
      out <- classification(x, ...)
    }
    return(out)
  }
  structure(lapply(id, fun, ...), class = 'classification_ids')
}

#' @export
#' @rdname classification
cbind.classification <- function(x) {
  gethiernames <- function(x) {
    x$name <- as.character(x$name)
    x$rank <- as.character(x$rank)
    values <- setNames(data.frame(t(x[,'name']), stringsAsFactors = FALSE), tolower(x[,'rank']))
    if ("id" %in% names(x)) {
      x$id <- as.character(x$id)
      ids <- setNames(data.frame(t(x[,'id']), stringsAsFactors = FALSE), paste0(tolower(x[,'rank']),"_id") )
      data.frame(values, ids)
    } else {
      values
    }
  }
  input <- x
  input <- input[sapply(input, class) %in% "data.frame"]
  tmp <- do.call(rbind.fill, lapply(input, gethiernames))
  tmp$query <- names(x)
  tmp$db <- attr(x, "db")
  tmp
}

#' @export
#' @rdname classification
rbind.classification <- function(x) {
  input <- x
  db <- attr(input, "db")
  x <- input[vapply(x, class, "") %in% "data.frame"]
  for (i in seq_along(x)) {
    x[[i]]$query <- names(x[i])
  }
  df <- do.call(rbind.fill, x)
  df$db <- db
  return( df )
}

#' @export
#' @rdname classification
cbind.classification_ids <- function(...) {
  input <- c(...)
  # remove non-data.frames
  input <- input[vapply(input, function(x) class(x[[1]]), "") %in% "data.frame"]

  gethiernames <- function(x){
    x$name <- as.character(x$name)
    x$rank <- as.character(x$rank)
    values <- setNames(data.frame(t(x[,'name']), stringsAsFactors = FALSE), tolower(x[,'rank']))
    if ("id" %in% names(x)) {
      x$id <- as.character(x$id)
      ids <- setNames(data.frame(t(x[,'id']), stringsAsFactors = FALSE), paste0(tolower(x[,'rank']),"_id") )
      data.frame(values, ids)
    } else {
      values
    }
  }
  dat <- do.call(rbind.fill, lapply(input, function(h){
      tmp <- lapply(h, gethiernames)
      tmp <- do.call(rbind.fill, tmp)
      tmp$query <- names(h)
      tmp$db <- attr(h, "db")
      tmp
    })
  )
  move_col(tt = dat, y = c('query','db'))
}

#' @export
#' @rdname classification
rbind.classification_ids <- function(...) {
  input <- c(...)
  # remove non-data.frames
  input <- input[vapply(input, function(x) class(x[[1]]), "") %in% "data.frame"]

  df <- lapply(input, function(x){
    coll <- list()
    for (i in seq_along(x)) {
      coll[[i]] <- data.frame(names(x[i]), x[i][[1]], stringsAsFactors = FALSE)
    }
    coll
  })

  get <- list()
  for (i in seq_along(df[[1]])) {
    tmp <- do.call(rbind, lapply(df, "[[", i))
    source2 <- gsub("\\.[0-9]+", "", row.names(tmp))
    row.names(tmp) <- NULL
    names(tmp)[1] <- "query"
    tmp <- data.frame(db = source2, tmp, stringsAsFactors = FALSE)
    get[[i]] <- tmp
  }

  tt <- if (length(get) == 1) get[[1]] else do.call(rbind.fill, get)
  move_col(tt, c('query', 'db'))
}

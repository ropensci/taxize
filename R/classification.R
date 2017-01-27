#' Retrieve the taxonomic hierarchy for a given taxon ID.
#'
#' @export
#' @param x Vector of taxa names (character) or IDs (character or numeric)
#' to query.
#' @param db character; database to query. either \code{ncbi}, \code{itis},
#' \code{eol}, \code{col}, \code{tropicos}, \code{gbif}, \code{nbn},
#' \code{worms}, \code{natserv}, or \code{bold}. Note that each taxonomic
#' data source has, their own identifiers, so that if you provide the wrong
#' \code{db} value for the identifier you could get a result, but it will
#' likely be wrong (not what you were expecting).
#' @param id character; identifiers, returned by \code{\link{get_tsn}},
#' \code{\link{get_uid}}, \code{\link{get_eolid}},
#' \code{\link{get_colid}}, \code{\link{get_tpsid}},
#' \code{\link{get_gbifid}}, \code{\link{get_tolid}},
#' \code{\link{get_wormsid}}, \code{\link{get_natservid}},
#' \code{\link{get_wormsid}}
#' @param callopts Curl options passed on to \code{\link[httr]{GET}}
#' @param ... Other arguments passed to \code{\link{get_tsn}},
#' \code{\link{get_uid}}, \code{\link{get_eolid}},
#' \code{\link{get_colid}}, \code{\link{get_tpsid}},
#' \code{\link{get_gbifid}}, \code{\link{get_wormsid}},
#' \code{\link{get_natservid}}, \code{\link{get_wormsid}}
#' @param start The first record to return. If omitted, the results are returned
#' 		from the first record (start=0). This is useful if the total number of
#' 		results is larger than the maximum number of results returned by a single
#' 		Web service query (currently the maximum number of results returned by a
#' 		single query is 500 for terse queries and 50 for full queries).
#' @param checklist character; The year of the checklist to query, if you want
#' a specific year's checklist instead of the lastest as default (numeric).
#' @param key Your API key; loads from .Rprofile.
#' @param return_id (logical) If \code{TRUE} (default), return the taxon id
#' as well as the name and rank of taxa in the lineage returned.
#' Ignored for natserv as they don't return IDs in their taxonomic
#' classification data.
#' @param rows (numeric) Any number from 1 to infinity. If the default NA,
#' all rows are considered. Note that this parameter is ignored if you pass
#' in a taxonomic id instead of a name of class character.
#'
#' @return A named list of data.frames with the taxonomic classification of
#'    every supplied taxa.
#' @details If IDs are supplied directly (not from the \code{get_*} functions)
#' you must specify the type of ID. There is a timeout of 1/3 seconds between
#' querries to NCBI.
#'
#' BEWARE: Right now, NBN doesn't return the queried taxon in the
#' classification. But you can attach it yourself quite easily of course.
#' This behavior is different from the other data sources.
#'
#' @seealso \code{\link{get_tsn}}, \code{\link{get_uid}},
#'    \code{\link{get_eolid}}, \code{\link{get_colid}},
#'    \code{\link{get_tpsid}}, \code{\link{get_gbifid}}
#'    \code{\link{get_wormsid}}, \code{\link{get_natservid}},
#'    \code{\link{get_boldid}}
#'
#' @examples \dontrun{
#' # Plug in taxon IDs
#' classification(9606, db = 'ncbi')
#' classification(c(9606, 55062), db = 'ncbi')
#' classification(129313, db = 'itis')
#' classification(57361017, db = 'eol')
#' classification(126436, db = 'worms')
#' classification("ELEMENT_GLOBAL.2.134717", db = 'natserv')
#' classification(c(2704179, 2441176), db = 'gbif')
#' classification(25509881, db = 'tropicos')
#' classification("NBNSYS0000004786", db = 'nbn')
#' classification(3930798, db = 'tol')
#' ## works the same if IDs are in class character
#' classification(c("2704179", "2441176"), db = 'gbif')
#' classification("Agapostemon", db = "bold")
#'
#' # Plug in taxon names
#' ## in this case, we use get_*() fxns internally to first get taxon IDs
#' classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi',
#'   verbose=FALSE)
#' classification(c("Chironomus riparius", "aaa vva"), db = 'itis')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'itis',
#'   verbose=FALSE)
#' classification(c("Chironomus riparius", "aaa vva"), db = 'eol')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'col')
#' classification("Alopias vulpinus", db = 'nbn')
#' classification('Gadus morhua', db = 'worms')
#' classification('Aquila chrysaetos', db = 'natserv')
#' classification('Gadus morhua', db = 'natserv')
#' classification('Pomatomus saltatrix', db = 'natserv')
#' classification('Aquila chrysaetos', db = 'natserv')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'col',
#'   verbose=FALSE)
#' classification(c("Chironomus riparius", "asdfasdfsfdfsd"), db = 'gbif')
#' classification("Chironomus", db = 'tol')
#' classification("Poa annua", db = 'tropicos')
#'
#' # Use methods for get_uid, get_tsn, get_eolid, get_colid, get_tpsid
#' classification(get_uid(c("Chironomus riparius", "Puma concolor")))
#'
#' classification(get_uid(c("Chironomus riparius", "aaa vva")))
#' classification(get_tsn(c("Chironomus riparius", "aaa vva")))
#' classification(get_tsn(c("Chironomus riparius", "aaa vva"),
#'   verbose = FALSE))
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
#' (out <- get_ids(names=c("Puma concolor","Accipiter striatus"),
#'   db = c('ncbi','itis','col')))
#' (cl <- classification(out))
#' rbind(cl)
#' ## cbind with so many names results in some messy data
#' cbind(cl)
#' ## so you can turn off return_id
#' cbind( classification(out, return_id=FALSE) )
#'
#' # rbind and cbind on class classification (from a
#' # call to get_colid, get_tsn, etc. other than get_ids)
#' (cl_col <- classification(
#'   get_colid(c("Puma concolor","Accipiter striatus"))))
#' rbind(cl_col)
#' cbind(cl_col)
#'
#' (cl_uid <- classification(get_uid(c("Puma concolor","Accipiter striatus")),
#'   return_id=FALSE))
#' rbind(cl_uid)
#' cbind(cl_uid)
#' ## cbind works a bit odd when there are lots of ranks without names
#' (cl_uid <- classification(get_uid(c("Puma concolor","Accipiter striatus")),
#'   return_id=TRUE))
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
#' (res <- classification(c("Alopias vulpinus","Pinus sylvestris"),
#'   db = 'nbn'))
#' rbind(res)
#' cbind(res)
#'
#' # Return taxonomic IDs
#' ## the return_id parameter is logical, and you can turn it on or off.
#' ## It's TRUE by default
#' classification(c("Alopias vulpinus","Pinus sylvestris"), db = 'ncbi',
#'   return_id = TRUE)
#' classification(c("Alopias vulpinus","Pinus sylvestris"), db = 'ncbi',
#'   return_id = FALSE)
#'
#' # Use rows parameter to select certain
#' classification('Poa annua', db = 'tropicos')
#' classification('Poa annua', db = 'tropicos', rows=1:4)
#' classification('Poa annua', db = 'tropicos', rows=1)
#' classification('Poa annua', db = 'tropicos', rows=6)
#' }
#'
#' @examples \dontrun{
#' # Fails without db param set
#' classification(315576)
#' }
classification <- function(...){
  UseMethod("classification")
}

#' @export
#' @rdname classification
classification.default <- function(x, db = NULL, callopts = list(),
                                   return_id = TRUE, rows = NaN, ...) {
  nstop(db)
  switch(
    db,
    itis = {
      id <- process_ids(x, db, get_tsn, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
    },
    ncbi = {
      id <- process_ids(x, db, get_uid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
    },
    eol = {
      id <- process_ids(x, db, get_eolid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
    },
    col = {
      id <- process_ids(x, db, get_colid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
    },
    tropicos = {
      id <- process_ids(x, db, get_tpsid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
    },
    gbif = {
      id <- process_ids(x, db, get_gbifid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
    },
    nbn = {
      id <- process_ids(x, db, get_nbnid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
    },
    tol = {
      id <- process_ids(x, db, get_tolid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
    },
    worms = {
      id <- process_ids(x, db, get_wormsid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
    },
    natserv = {
      id <- process_ids(x, db, get_natservid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
    },
    bold = {
      id <- process_ids(x, db, get_boldid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts, return_id = return_id, ...), x)
    },
    stop("the provided db value was not recognised", call. = FALSE)
  )
}

process_ids <- function(input, db, fxn, ...){
  g <- tryCatch(as.numeric(as.character(input)), warning = function(e) e)
  if (
    inherits(g, "numeric") ||
    is.character(input) && grepl("N[HB]", input) ||
    is.character(input) && grepl("ELEMENT_GLOBAL", input)
  ) {
    as_fxn <- switch(db,
           itis = as.tsn,
           ncbi = as.uid,
           eol = as.eolid,
           col = as.colid,
           tropicos = as.tpsid,
           gbif = as.gbifid,
           nbn = as.nbnid,
           tol = as.tolid,
           worms = as.wormsid,
           natserv = as.natservid,
           bold = as.boldid)
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
      out <- ritis::hierarchy_full(x, wt = "json", raw = FALSE, callopts)
      if (NROW(out) < 1) return(NA)
      # remove overhang
      out <- out[1:which(out$tsn == x), c('taxonname', 'rankname', 'tsn')]
      names(out) <- c('name', 'rank', 'id')
      # Optionally return tsn of lineage
      if (!return_id) out <- out[, c('name', 'rank')]
      out$rank <- tolower(out$rank)
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
      baseurl <- paste0(ncbi_base(), "/entrez/eutils/efetch.fcgi?db=taxonomy")
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
      if (NROW(out) == 0) {
        out <- NA
      } else {
        out <- rbind(out, c(xml2::xml_text(xml2::xml_find_all(ttp, "//TaxaSet/Taxon/ScientificName")),
                            xml2::xml_text(xml2::xml_find_all(ttp, "//TaxaSet/Taxon/Rank")),
                            xml2::xml_text(xml2::xml_find_all(ttp, "//TaxaSet/Taxon/TaxId")))
        )
        # Optionally return tsn of lineage
        if (!return_id) out <- out[, c('name', 'rank')]
        out$rank <- tolower(out$rank)
        return(out)
      }
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
      res <- jsonlite::fromJSON(con_utf8(tt), FALSE)
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
        out$rank <- tolower(out$rank)
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
      out <- rbind(out, c(xml2::xml_text(xml2::xml_find_first(tt, "//result/name")),
                          xml2::xml_text(xml2::xml_find_first(tt, "//result/rank")),
                          xml2::xml_text(xml2::xml_find_first(tt, "//result/id"))))
      # Optionally return id of lineage
      if (!return_id) out <- out[, c('name', 'rank')]
      out$rank <- tolower(out$rank)
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
  id <- xml2::xml_text(xml2::xml_find_all(x, "//classification//id"))
  if (any(grepl("species", rank, ignore.case = TRUE))) {
    name[which(rank %in% "Species")] <- paste(name[which(rank %in% "Genus")], name[which(rank %in% "Species")], collapse = " ")
  }
  data.frame(name, rank, id, stringsAsFactors = FALSE)
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
      out$rank <- tolower(out$rank)
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
        df <- data.frame(name = nms$V1, rank = nms$.id, id = keys, stringsAsFactors = FALSE)
        df$rank <- tolower(df$rank)
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
        out$rank <- tolower(out$rank)
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
classification.tolid <- function(id, callopts = list(), return_id = TRUE, ...) {
  fun <- function(x, callopts){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- tryCatch(rotl::taxonomy_taxon_info(x, include_lineage = TRUE), error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        outdf <- rotl::tax_lineage(out)[[1]]
        # we have to reverse row order
        outdf <- outdf[NROW(outdf):1, ]
        # tack on species searched for
        orig <- c(out[[1]]$rank, out[[1]]$name, out[[1]]$unique_name, out[[1]]$ott_id)
        outdf <- rbind(outdf, orig)
        row.names(outdf) <- NULL
        outdf <- outdf[ , c('name','rank', 'ott_id')]
        names(outdf) <- c('name', 'rank', 'id')
        if (!return_id) outdf <- outdf[, c('name', 'rank')]
        return(outdf)
      }
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'tol')
}

#' @export
#' @rdname classification
classification.wormsid <- function(id, callopts = list(), return_id = TRUE, ...) {
  fun <- function(x, callopts){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- tryCatch(worrms::wm_classification(as.numeric(x)), error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        row.names(out) <- NULL
        out <- out[ , c('scientificname','rank', 'AphiaID')]
        names(out) <- c('name', 'rank', 'id')
        if (!return_id) out <- out[, c('name', 'rank')]
        return(out)
      }
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'worms')
}

#' @export
#' @rdname classification
classification.natservid <- function(id, callopts = list(), return_id = TRUE, ...) {
  fun <- function(x, callopts) {
    if (is.na(x)) {
      out <- NA
    } else {
      out <- tryCatch(natserv::ns_data(x), error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        tmp <- out[[1]]$classification
        if (is.null(tmp)) return(NA)
        tmp <- tmp$taxonomy$formalTaxonomy
        if (is.null(tmp)) return(NA)
        tmp <- tmp[names(tmp) %in% c('kingdom', 'phylum', 'class', 'order', 'family', 'genus')]
        df <- data.frame(scientificname = unname(unlist(tmp)), rank = names(tmp),
                   stringsAsFactors = FALSE)
        rks <- c('kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species')
        targ_taxon <- c(
          out[[1]]$classification$names$scientificName$unformattedName[[1]],
          rks[which(df$rank[length(df$rank)] == rks) + 1]
        )
        df <- rbind(df, targ_taxon)
        return(df)
      }
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'natserv')
}

#' @export
#' @rdname classification
classification.boldid <- function(id, callopts = list(), return_id = TRUE, ...) {
  fun <- function(x, callopts) {
    if (is.na(x)) {
      out <- NA
    } else {
      out <- tryCatch(bold_search(id = x, includeTree = TRUE), error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        if (is.null(out)) return(NA)
        tmp <- out[names(out) %in% c('taxid', 'taxon', 'tax_rank')]
        df <- data.frame(name = tmp$taxon, rank = tmp$tax_rank,
                          id = tmp$taxid, stringsAsFactors = FALSE)
        return(df)
      }
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'bold')
}

# ---------
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
    x <- data.frame(x)
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
  #input <- input[sapply(input, class) %in% "data.frame"]
  input <- input[vapply(x, function(z) inherits(z, "data.frame"), logical(1))]
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
  #x <- input[vapply(x, class, "") %in% "data.frame"]
  x <- input[vapply(x, function(z) inherits(z, "data.frame"), logical(1))]
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
  input <- input[vapply(input, function(x) class(x[[1]])[1], "") %in% c("data.frame", "tbl_df")]

  gethiernames <- function(x){
    x$name <- as.character(x$name)
    x$rank <- as.character(x$rank)
    values <- setNames(data.frame(t(x$name), stringsAsFactors = FALSE), tolower(x$rank))
    if ("id" %in% names(x)) {
      x$id <- as.character(x$id)
      ids <- setNames(data.frame(t(x$id), stringsAsFactors = FALSE), paste0(tolower(x$rank), "_id") )
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
  input <- input[vapply(input, function(x) class(x[[1]])[1], "") %in% c("data.frame", "tbl_df")]

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

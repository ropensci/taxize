#' Retrieve the taxonomic hierarchy for a given taxon ID.
#'
#' @export
#' @param sci_id Vector of taxa names (character) or IDs (character or numeric)
#' to query. For `db = "eol"`, EOL expects you to pass it a taxon id, called
#' `eolid` in the output of [get_eolid()]. 
#' @param db character; database to query. either `ncbi`, `itis`, `eol`,
#' `tropicos`, `gbif`, `nbn`, `worms`, `natserv`, `bold`, `wiki`, or `pow`.
#' Note that each taxonomic data source has, their own identifiers, so that
#' if you provide the wrong `db` value for the identifier you could get a
#' result, but it will likely be wrong (not what you were expecting). If using
#' ncbi, and/or tropicos, we recommend getting an API key; see
#' [taxize-authentication]
#' @param id character; identifiers, returned by [get_tsn()], [get_uid()],
#' [get_eolid()], [get_tpsid()], [get_gbifid()], [get_tolid()],
#' [get_wormsid()], [get_natservid()], [get_wormsid()], [get_wiki()],
#' [get_pow()]
#' @param callopts Curl options passed on to [crul::verb-GET]
#' @param ... For `classification`: other arguments passed to [get_tsn()],
#' [get_uid()], [get_eolid()], [get_tpsid()], [get_gbifid()],
#' [get_wormsid()], [get_natservid()], [get_wormsid()], [get_wiki()],
#' [get_pow()]. For `rbind.classification` and `cbind.classification`: one or
#' more objects of class `classification`
#' @param return_id (logical) If `TRUE` (default), return the taxon id
#' as well as the name and rank of taxa in the lineage returned.
#' Ignored for natserv as they don't return IDs in their taxonomic
#' classification data.
#' @param rows (numeric) Any number from 1 to infinity. If the default NA,
#' all rows are considered. Note that this parameter is ignored if you pass
#' in a taxonomic id instead of a name of class character.
#' @param batch_size (numeric) For NCBI queries, specify the number of IDs to
#'   lookup for each query.
#' @param max_tries (numeric) For NCBI queries, the number of times a particular
#'   query will be attempted, assuming the first does not work.
#' @param x Deprecated, see `sci_id`
#'
#' @return A named list of data.frames with the taxonomic classification of
#'    every supplied taxa.
#' @details If IDs are supplied directly (not from the `get_*` functions)
#' you must specify the type of ID. There is a timeout of 1/3 seconds between
#' queries to NCBI.
#'
#' BEWARE: Right now, NBN doesn't return the queried taxon in the
#' classification. But you can attach it yourself quite easily of course.
#' This behavior is different from the other data sources.
#'
#' @seealso [get_tsn()], [get_uid()], [get_eolid()],
#'    [get_tpsid()], [get_gbifid()], [get_wormsid()], [get_natservid()],
#'    [get_boldid()], [get_wiki()], [get_pow()]
#'
#' @section Lots of results:
#' It may happen sometimes that you get more results back from your query
#' than will show in the data.frame on screen. Our advice is to refine your
#' query in those cases. On a data source basis we can attempt to help
#' make it easier to refine queries, whether it be with the data provider
#' (unlikely to happen), or in the code in this package (more likely) -
#' let us know if you run into too many results problem and we'll see what
#' we can do.
#'
#' @section Authentication:
#' See [taxize-authentication]
#' 
#' @section EOL:
#' EOL does not have very good failure behavior. For example, if you submit
#' an ID that does not exist they'll return a 500 HTTP error, which is
#' not an appropriate error; it's probably that that ID does not exist 
#' in their database, but we can't know for sure. Isn't that fun?
#' 
#' @section HTTP version for NCBI requests:
#' We hard code `http_version = 2L` to use HTTP/1.1 in HTTP requests to
#' the Entrez API. See `curl::curl_symbols('CURL_HTTP_VERSION')` 
#'
#' @examples \dontrun{
#' # Plug in taxon IDs
#' classification(9606, db = 'ncbi')
#' classification(c(9606, 55062), db = 'ncbi')
#' classification(129313, db = 'itis')
#' classification(6985636, db = 'eol')
#' classification(126436, db = 'worms')
#' classification('Helianthus annuus', db = 'pow')
#' classification('Helianthus', db = 'pow')
#' classification('Asteraceae', db = 'pow')
#' classification("134717", db = 'natserv')
#' classification(c(2704179, 6162875, 8286319), db = 'gbif')
#' classification(25509881, db = 'tropicos')
#' classification("NBNSYS0000004786", db = 'nbn')
#' classification(as.nbnid("NBNSYS0000004786"), db = 'nbn')
#' classification(3930798, db = 'tol')
#'
#' ## works the same if IDs are in class character
#' classification(c("2704179", "2441176"), db = 'gbif')
#' classification("Agapostemon", db = "bold")
#'
#' # wikispecies
#' classification("Malus domestica", db = "wiki")
#' classification("Pinus contorta", db = "wiki")
#' classification("Pinus contorta", db = "wiki", wiki_site = "commons")
#' classification("Pinus contorta", db = "wiki", wiki_site = "pedia")
#' classification("Pinus contorta", db = "wiki", wiki_site = "pedia",
#'   wiki = "fr")
#'
#' classification(get_wiki("Malus domestica", "commons"))
#' classification(get_wiki("Malus domestica", "species"))
#' classification(c("Pinus contorta", "Malus domestica"), db = "wiki")
#'
#' # Plug in taxon names
#' ## in this case, we use get_*() fxns internally to first get taxon IDs
#' classification("Oncorhynchus mykiss", db = "eol")
#' classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi',
#'   messages=FALSE)
#' classification(c("Chironomus riparius", "aaa vva"), db = 'itis')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'itis',
#'   messages=FALSE)
#' classification(c("Chironomus riparius", "aaa vva"), db = 'eol')
#' classification("Alopias vulpinus", db = 'nbn')
#' classification('Gadus morhua', db = 'worms')
#' classification('Aquila chrysaetos', db = 'natserv')
#' classification('Gadus morhua', db = 'natserv')
#' classification('Pomatomus saltatrix', db = 'natserv')
#' classification('Aquila chrysaetos', db = 'natserv')
#' classification(c("Chironomus riparius", "asdfasdfsfdfsd"), db = 'gbif')
#' classification("Chironomus", db = 'tol')
#' classification("Poa annua", db = 'tropicos')
#'
#' # Use methods for get_uid, get_tsn, get_eolid, get_tpsid
#' classification(get_uid(c("Chironomus riparius", "Puma concolor")))
#'
#' classification(get_uid(c("Chironomus riparius", "aaa vva")))
#' classification(get_tsn(c("Chironomus riparius", "aaa vva")))
#' classification(get_tsn(c("Chironomus riparius", "aaa vva"),
#'   messages = FALSE))
#' classification(get_eolid(c("Chironomus riparius", "aaa vva")))
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
#'   db = c('ncbi','itis')))
#' (cl <- classification(out))
#' rbind(cl)
#' ## cbind with so many names results in some messy data
#' cbind(cl)
#' ## so you can turn off return_id
#' cbind( classification(out, return_id=FALSE) )
#'
#' (cl_uid <- classification(get_uid(c("Puma concolor",
#'   "Accipiter striatus")), return_id=FALSE))
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
#' 
#' # Queries of many IDs are processed in batches for NCBI
#' ids <- c("13083", "2650392", "1547764", "230054", "353934", "656984", 
#' "271789", "126272", "184644", "73213", "662816", "1161803", "1239353", 
#' "59420", "665675", "866969", "1091219", "1431218", "1471898", 
#' "864321", "251768", "2486276", "2068772", "1825808", "2006532", 
#' "128287", "1195738", "1084683", "1886461", "508296", "377247", 
#' "1489665", "329325", "219243", "1176946", "339893", "197933", 
#' "174510", "1704048", "212897", "154842", "1239280", "260135", 
#' "405735", "1566412", "2083462", "651348", "983204", "165380", 
#' "2338856", "2068760", "167262", "34229", "1213340", "478939", 
#' "1933585", "49951", "1277794", "1671089", "1502538", "362355", 
#' "746473", "242879", "158219", "313664", "2093188", "1541232", 
#' "584742", "1331091", "147639", "284492", "75642", "1412882", 
#' "391782", "1406855", "434506", "2053357", "217315", "1444328", 
#' "329249", "2294004", "84942", "324458", "538247", "69452", "49170", 
#' "1993845", "261646", "127633", "228146", "1420004", "1629772", 
#' "577055", "697062", "231660", "648380", "554953", "746496", "2602969")
#' result <- classification(ids, db = 'ncbi')
#' }
#'
#' @examples \dontrun{
#' # Fails without db param set
#' # classification(315576)
#' }
classification <- function(...){
  UseMethod("classification")
}

#' @export
#' @rdname classification
classification.default <- function(sci_id, db = NULL, callopts = list(),
                                   return_id = TRUE, rows = NA, x = NULL, ...) {
  nstop(db)
  pchk(x, "sci_id")
  switch(
    db,
    itis = {
      id <- process_ids(sci_id, db, get_tsn, rows = rows, ...)
      stats::setNames(classification(id, return_id = return_id, ...), sci_id)
    },
    ncbi = {
      id <- process_ids(sci_id, db, get_uid, rows = rows)
      stats::setNames(classification(id, callopts = callopts,
        return_id = return_id), sci_id)
    },
    eol = {
      id <- process_ids(sci_id, db, get_eolid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
        return_id = return_id, ...), sci_id)
    },
    tropicos = {
      id <- process_ids(sci_id, db, get_tpsid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
        return_id = return_id, ...), sci_id)
    },
    gbif = {
      id <- process_ids(sci_id, db, get_gbifid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
        return_id = return_id, ...), sci_id)
    },
    nbn = {
      id <- process_ids(sci_id, db, get_nbnid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
        return_id = return_id, ...), sci_id)
    },
    tol = {
      id <- process_ids(sci_id, db, get_tolid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
        return_id = return_id, ...), sci_id)
    },
    worms = {
      id <- process_ids(sci_id, db, get_wormsid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
        return_id = return_id, ...), sci_id)
    },
    natserv = {
      id <- process_ids(sci_id, db, get_natservid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
        return_id = return_id, ...), sci_id)
    },
    bold = {
      id <- process_ids(sci_id, db, get_boldid, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
        return_id = return_id, ...), sci_id)
    },
    wiki = {
      id <- process_ids(sci_id, db, get_wiki, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
        return_id = return_id, ...), sci_id)
    },
    pow = {
      id <- process_ids(sci_id, db, get_pow, rows = rows, ...)
      stats::setNames(classification(id, callopts = callopts,
        return_id = return_id, ...), sci_id)
    },
    stop("the provided db value was not recognised", call. = FALSE)
  )
}

process_ids <- function(input, db, fxn, ...){
  g <- tryCatch(as.numeric(as.character(input)), warning = function(e) e)
  if (
    inherits(g, "numeric") || # all others
    is.character(input) && all(grepl("N[HB]", input)) || # NBN
    is.character(input) && all(grepl("urn:lsid", input)) # POW
  ) {
    as_fxn <- switch(db,
           itis = as.tsn,
           ncbi = as.uid,
           eol = as.eolid,
           tropicos = as.tpsid,
           gbif = as.gbifid,
           nbn = as.nbnid,
           tol = as.tolid,
           worms = as.wormsid,
           natserv = as.natservid,
           bold = as.boldid,
           wiki = as.wiki,
           pow = as.pow)
    as_fxn(input, check = FALSE)
  } else {
    eval(fxn)(input, ...)
  }
}

#' @export
#' @rdname classification
classification.tsn <- function(id, return_id = TRUE, ...) {
  warn_db(list(...), "itis")
  fun <- function(x) {
    # return NA if NA is supplied
    if (is.na(x)) {
      out <- NA
    } else {
      out <- ritis::hierarchy_full(as.character(x), wt = "json", raw = FALSE)
      if (NROW(out) < 1) return(NA)
      # make normal data.frame
      out <- data.frame(out, stringsAsFactors = FALSE)
      # remove overhang
      out <- out[1:which(out$tsn == x), c('taxonname', 'rankname', 'tsn')]
      names(out) <- c('name', 'rank', 'id')
      # Optionally return tsn of lineage
      if (!return_id) out <- out[, c('name', 'rank')]
      out$rank <- tolower(out$rank)
    }
    return(out)
  }
  out <- lapply(id, fun)
  names(out) <- id
  structure(out, class = 'classification', db = 'itis')
}

#' @export
#' @rdname classification
classification.uid <- function(id, callopts = list(), return_id = TRUE,
  batch_size = 50, max_tries = 3, ...) {

  warn_db(list(...), "ncbi")
  fun <- function(x, callopts) {
    key <- getkey(NULL, service="entrez")
    
    query_ncbi <- function(ids) {
      query <- tc(list(db = "taxonomy", ID = paste0(ids, collapse = ','),
        api_key = key))
      cli <- crul::HttpClient$new(url = ncbi_base(),
        opts = c(http_version = 2L, callopts))
      success <- FALSE
      tries <- 1
      while (success == FALSE && tries <= max_tries) {
        res <- cli$get("entrez/eutils/efetch.fcgi", query = query)
        res$raise_for_status()
        tt <- res$parse("UTF-8")
        ttp <- xml2::read_xml(tt)
        out <- lapply(xml2::xml_find_all(ttp, '//TaxaSet/Taxon'),
          function(tax_node) {
          lin <- data.frame(
            name = xml_text_all(tax_node,
              ".//LineageEx/Taxon/ScientificName"),
            rank = xml_text_all(tax_node, ".//LineageEx/Taxon/Rank"),
            id = xml_text_all(tax_node, ".//LineageEx/Taxon/TaxId"),
            stringsAsFactors = FALSE)
          targ_tax <- data.frame(
            name = xml_text_all(tax_node, "./ScientificName"),
            rank = xml_text_all(tax_node, "./Rank"),
            id = xml_text_all(tax_node, "./TaxId"),
            stringsAsFactors = FALSE)
          rbind(lin, targ_tax)
        })
        # Is not directly below root and no lineage info
        parent_id <- xml_text_all(ttp, "//TaxaSet/Taxon/ParentTaxId") %||% ""
        out[vapply(out, NROW, numeric(1)) == 0 & parent_id != "1"] <- NA
        # Add NA where the taxon ID was not found
        names(out) <- xml_text(xml2::xml_find_all(ttp,
          '//TaxaSet/Taxon/TaxId'))
        out <- unname(out[ids])
        success <- ! grepl(tt, pattern = 'error', ignore.case = TRUE)
        tries <- tries + 1
        # NCBI limits requests to three per second without key or 10 per
        # second with key
        ncbi_rate_limit_pause(key)
        # Wait longer if query failed
        if (success == FALSE) {
          Sys.sleep(1)
        }
      }
      # Return NA if cannot get information
      if (!success) {
        out <- rep(list(NA), length(ids))
        warning(call. = FALSE, 'Giving up on query after ',
          max_tries, ' tries. NAs will be returned.')
      }
      return(out)
    }
    # return NA if NA is supplied
    out <- rep(list(NA), length(x))
    out[! is.na(x)] <- query_ncbi(x[! is.na(x)])
    # Optionally return taxon id of lineage taxa
    if (!return_id) {
      out[! is.na(out)] <- lapply(out[! is.na(out)],
        function(o) o[, c('name', 'rank')])
    }
    # Return ranks in all lower case
    out[! is.na(out)] <- lapply(out[! is.na(out)], function(o) {
      o$rank <- tolower(o$rank)
      return(o)
    })
    return(out)
  }
  id <- as.character(id) # force to character
  id_chunks <- split(id, ceiling(seq_along(id)/batch_size))
  out <- lapply(id_chunks, fun, callopts = callopts)
  out <- unlist(out, recursive = FALSE)
  names(out) <- id
  structure(out, class = 'classification', db = 'ncbi')
}

#' @export
#' @rdname classification
classification.eolid <- function(id, callopts = list(), return_id = TRUE, ...) {
  warn_db(list(...), "eol")
  common_names = synonyms = NULL
  fun <- function(x){
    if (is.na(x)) {
      out <- NA
    } else {
      args <- tc(list(common_names = common_names, synonyms = synonyms))
      cli <- crul::HttpClient$new(url = 'https://eol.org', opts = callopts)
      tt <- cli$get(file.path('api/hierarchy_entries/1.0', paste0(x, ".json")),
        query = args)
      tt$raise_for_status()
      res <- jsonlite::fromJSON(tt$parse("UTF-8"), FALSE)
      if (length(res$ancestors) == 0) {
        return(sprintf("No hierarchy information for %s", x))
      } else {
        fff <- lapply(res$ancestors, function(z) {
          z[sapply(z, is.null)] <- NA_character_
          data.frame(z, stringsAsFactors = FALSE)
        })
        out <- dt2df(fff, idcol = FALSE)[,c('scientificName',
          'taxonRank', 'taxonID')]
        # add querried taxon
        tr <- res$taxonRank
        out <- rbind(out,
          c(res$scientificName, if ( is.null(tr) ) NA else tr, x))
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
classification.tpsid <- function(id, callopts = list(), return_id = TRUE, ...) {
  warn_db(list(...), "tropicos")
  fun <- function(x, callopts){
    if (is.na(x)) {
      out <- NA
    } else {
      url <- sprintf('http://services.tropicos.org/Name/%s/HigherTaxa', x)
      key <- getkey(NULL, "TROPICOS_KEY")
      args <- tc(list(format = 'json', apikey = key))
      tt <- tax_GET(url, query = args, opts = callopts)
      if ("Error" %in% names(tt)) {
        out <- data.frame(ScientificName = NA, Rank = NA)
      } else {
        out <- tt[,c('ScientificName','Rank', 'NameId')]
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
classification.gbifid <- function(id, callopts = list(),
  return_id = TRUE, ...) {

  warn_db(list(...), "gbif")
  fun <- function(x, callopts){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- suppressWarnings(tryCatch(
        gbif_name_usage(key = x, callopts = callopts),
        error = function(e) e))
      if (inherits(out, "simpleError")) {
        NA
      } else {
        cls = c('kingdom','phylum','class', 'order','family','genus','species')
        df1 <- stats::setNames(nmdlst2df(out[names(out) %in% cls]),
          c("rank", "name"))
        df2 <- stats::setNames(
          nmdlst2df(out[names(out) %in% paste0(cls, "Key")]),
          c(".id", "id"))
        df2$rank <- sub("Key", "", df2$.id)
        df2$.id <- NULL
        df <- merge(df1, df2, by = "rank")
        df$rank <- tolower(df$rank)
        # sort to make sure ranks are in correct descending order
        df <- df[order(vapply(df$rank, which_rank, 1)), ]
        # column order
        df <- data.frame(name = df$name, rank = df$rank, id = df$id,
          stringsAsFactors = FALSE)
        # check if target taxon is sub-specific
        if (which_rank(tolower(out$rank)) > 34) {
          df <- rbind(df,
            c(out$canonicalName, tolower(out$rank), out$key))
        }
        df$id <- as.integer(df$id)
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
classification.nbnid <- function(id, callopts = list(),
  return_id = TRUE, ...) {

  warn_db(list(...), "nbn")
  fun <- function(x, callopts){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- suppressWarnings(tryCatch(nbn_classification(id = x, callopts),
                                       error = function(e) e))
      if (inherits(out, "simpleError")) {
        NA
      } else {
        out <- out[ , c('scientificname', 'rank', 'guid')]
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
classification.tolid <- function(id, callopts = list(),
  return_id = TRUE, ...) {

  warn_db(list(...), "tol")
  fun <- function(x, callopts) {
    if (is.na(x)) {
      out <- NA
    } else {
      x <- as.numeric(x)
      out <- tryCatch(rotl::taxonomy_taxon_info(x,
        include_lineage = TRUE), error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        outdf <- rotl::tax_lineage(out)[[1]]
        # we have to reverse row order
        outdf <- outdf[NROW(outdf):1, ]
        # tack on species searched for
        orig <- c(out[[1]]$rank, out[[1]]$name, out[[1]]$unique_name,
          out[[1]]$ott_id)
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
classification.wormsid <- function(id, callopts = list(),
  return_id = TRUE, ...) {

  warn_db(list(...), "worms")
  fun <- function(x, ...){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- tryCatch(worrms::wm_classification(as.numeric(x), ...),
        error = function(e) e)
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
  out <- lapply(id, fun, ...)
  names(out) <- id
  structure(out, class = 'classification', db = 'worms')
}

#' @export
#' @rdname classification
classification.natservid <- function(id, callopts = list(),
  return_id = TRUE, ...) {

  warn_db(list(...), "natserv")
  fun <- function(x, callopts) {
    if (is.na(x)) {
      out <- NA
    } else {
      out <- tryCatch(natserv::ns_id(paste0("ELEMENT_GLOBAL.2.", x)), error = function(e) e)
      # out <- tryCatch(natserv::ns_data(x), error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        zz <- c("kingdom", "phylum", "taxclass", "taxorder", "family", "genus")
        if (is.null(out$speciesGlobal)) return(NA)
        res <- tc(out$speciesGlobal[zz])
        if (length(res) == 0) return(NA)
        df <- data.frame(scientificname = unname(unlist(res)),
          rank = gsub("tax", "", names(res)), stringsAsFactors = FALSE)
        df <- rbind(df, c(out$scientificName, "species"))
        # tmp <- out[[1]]$classification
        # if (is.null(tmp)) return(NA)
        # tmp <- tmp$taxonomy$formalTaxonomy
        # if (is.null(tmp)) return(NA)
        # tmp <- tmp[names(tmp) %in% c('kingdom', 'phylum',
        #   'class', 'order', 'family', 'genus')]
        # df <- data.frame(scientificname = unname(unlist(tmp)),
        #   rank = names(tmp), stringsAsFactors = FALSE)
        # rks <- c('kingdom', 'phylum', 'class', 'order',
        #   'family', 'genus', 'species')
        # targ_taxon <- c(
        #   out[[1]]$classification$names$scientificName$unformattedName[[1]],
        #   rks[which(df$rank[length(df$rank)] == rks) + 1]
        # )
        # df <- rbind(df, targ_taxon)
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
classification.boldid <- function(id, callopts = list(),
  return_id = TRUE, ...) {

  warn_db(list(...), "bold")
  fun <- function(x, callopts) {
    if (is.na(x)) {
      out <- NA
    } else {
      out <- tryCatch(bold_search(id = x, includeTree = TRUE),
        error = function(e) e)
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

#' @export
#' @rdname classification
classification.wiki <- function(id, callopts = list(), return_id = TRUE, ...) {
  warn_db(list(...), "wiki")
  fun <- function(x, wiki_site = "species", wiki = "en", callopts) {
    if (is.na(x)) {
      out <- NA
    } else {
      fxn <- switch(
        wiki_site,
        species = wikitaxa::wt_wikispecies,
        commons = wikitaxa::wt_wikicommons,
        pedia = wikitaxa::wt_wikipedia
      )
      out <- tryCatch(fxn(x)$classification, error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        if (is.null(out) || NROW(out) == 0) return(NA)
        df <- data.frame(name = out$name, rank = out$rank,
                         stringsAsFactors = FALSE)
        return(df)
      }
    }
  }
  out <- list()
  for (i in seq_along(id)) {
    out[[i]] <-
      fun(id[i], attr(id, "wiki_site"), attr(id, "wiki_lang"))
  }
  names(out) <- id
  structure(out, class = 'classification', db = 'wiki',
            wiki_site = attr(id, "wiki_site"), wiki = attr(id, "wiki_lang"))
}

#' @export
#' @rdname classification
classification.pow <- function(id, callopts = list(), return_id = TRUE, ...) {
  warn_db(list(...), "pow")
  fun <- function(x, callopts) {
    if (is.na(x)) {
      out <- NA
    } else {
      out <- tryCatch(pow_lookup(x), error = function(e) e)
      if (inherits(out, "error")) {
        NA
      } else {
        if (is.null(out)) return(NA)
        tmp <- out$meta$classification[,c('name', 'rank', 'fqId')]
        df <- data.frame(name = tmp$name, rank = tolower(tmp$rank),
          id = tmp$fqId, stringsAsFactors = FALSE)
        return(df)
      }
    }
  }
  out <- lapply(id, fun, callopts = callopts)
  names(out) <- id
  structure(out, class = 'classification', db = 'pow')
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
cbind.classification <- function(...) {
  gethiernames <- function(x) {
    x <- data.frame(x)
    x$name <- as.character(x$name)
    x$rank <- as.character(x$rank)
    values <- setNames(data.frame(t(x[,'name']), stringsAsFactors = FALSE),
      tolower(x[,'rank']))
    if ("id" %in% names(x)) {
      x$id <- as.character(x$id)
      ids <- setNames(data.frame(t(x[,'id']), stringsAsFactors = FALSE),
        paste0(tolower(x[,'rank']),"_id") )
      data.frame(values, ids)
    } else {
      values
    }
  }
  input <- x <- c(...)
  input <- input[vapply(x, function(z) inherits(z, "data.frame"), logical(1))]
  tmp <- dt2df(lapply(input, gethiernames), idcol = FALSE)
  tmp$query <- names(x)
  tmp$db <- attr(x, "db")
  tmp
}

#' @export
#' @rdname classification
rbind.classification <- function(...) {
  input <- x <- c(...)
  db <- attr(input, "db")
  x <- input[vapply(x, function(z) inherits(z, "data.frame"), logical(1))]
  for (i in seq_along(x)) {
    x[[i]]$query <- names(x[i])
  }
  df <- dt2df(x, idcol = FALSE)
  df$db <- db
  return( df )
}

#' @export
#' @rdname classification
cbind.classification_ids <- function(...) {
  input <- c(...)

  # remove non-data.frames
  input <- input[vapply(input, function(x)
    class(x[[1]])[1], "") %in% c("data.frame", "tbl_df")]

  gethiernames <- function(x){
    x$name <- as.character(x$name)
    x$rank <- as.character(x$rank)
    values <- setNames(data.frame(t(x$name), stringsAsFactors = FALSE),
      tolower(x$rank))
    if ("id" %in% names(x)) {
      x$id <- as.character(x$id)
      ids <- setNames(data.frame(t(x$id), stringsAsFactors = FALSE),
        paste0(tolower(x$rank), "_id") )
      data.frame(values, ids)
    } else {
      values
    }
  }
  dat <- dt2df(lapply(input, function(h){
    tmp <- lapply(h, gethiernames)
    tmp <- dt2df(tmp, idcol = FALSE)
    tmp$query <- names(h)
    tmp$db <- attr(h, "db")
    tmp
  }), idcol = FALSE)
  move_col(tt = dat, y = c('query','db'))
}

#' @export
#' @rdname classification
rbind.classification_ids <- function(...) {
  input <- c(...)

  # remove non-data.frames
  input <- input[vapply(input, function(x)
    class(x[[1]])[1], "") %in% c("data.frame", "tbl_df")]

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

  tt <- if (length(get) == 1) get[[1]] else dt2df(get, idcol = FALSE)
  move_col(tt, c('query', 'db'))
}

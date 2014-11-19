#' Retrieve the taxonomic hierarchy for a given taxon ID.
#' 
#' @import XML RCurl plyr
#' 
#' @param x character; taxons to query.
#' @param db character; database to query. either \code{ncbi}, \code{itis}, 
#'    \code{eol}, \code{col}, \code{tropicos}, \code{gbif}, or \code{nbn}.
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
#' @param return_id If TRUE, return the taxon id as well as the name and rank of taxa
#' in the lineage returned.
#' 
#' @return A named list of data.frames with the taxonomic classifcation of 
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
#' @export
#' @examples \donttest{
#' # Plug in taxon names directly
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
#' out <- get_ids(names="Puma concolor", db = c('ncbi','gbif'))
#' cl <- classification(out)
#' 
#' # Bind width-wise from class classification_ids
#' cbind(cl)
#' 
#' # Bind length-wise
#' rbind(cl)
#' 
#' # Many names to get_ids
#' out <- get_ids(names=c("Puma concolor","Accipiter striatus"), db = c('ncbi','itis','col'))
#' cl <- classification(out)
#' rbind(cl)
#' cbind(cl)
#' 
#' # rbind and cbind on class classification (from a call to get_colid, get_tsn, etc. 
#' # - other than get_ids)
#' cl_col <- classification(get_colid(c("Puma concolor","Accipiter striatus")))
#' rbind(cl_col)
#' cbind(cl_col)
#' 
#' cl_uid <- classification(get_uid(c("Puma concolor","Accipiter striatus")))
#' rbind(cl_uid)
#' cbind(cl_uid)
#' 
#' cl_tsn <- classification(get_tsn(c("Puma concolor","Accipiter striatus")))
#' rbind(cl_tsn)
#' cbind(cl_tsn)
#' 
#' tsns <- get_tsn(c("Puma concolor","Accipiter striatus"))
#' cl_tsns <- classification(tsns)
#' cbind(cl_tsns)
#' 
#' # NBN data
#' res <- classification(c("Alopias vulpinus","Pinus sylvestris"), db = 'nbn')
#' rbind(res)
#' cbind(res)
#' }
#' 
#' @examples \donttest{
#' # Fails
#' classification(315576)
#' }
classification <- function(...){
  UseMethod("classification")
}

#' @method classification default
#' @export
#' @rdname classification
classification.default <- function(x, db = NULL, callopts=list(), return_id = FALSE, ...){
  if (is.null(db))
    stop("Must specify db!")
  if (db == 'itis') {
    id <- process_ids(x, get_tsn, ...)
    out <- classification(id, callopts=callopts, return_id=return_id, ...)
    names(out) <- x
  }
  if (db == 'ncbi') {
    id <- process_ids(x, get_uid, ...)
#     id <- get_uid(x, ...)
    out <- classification(id, return_id=return_id, ...)
    names(out) <- x
  }
  if (db == 'eol') {
    id <- process_ids(x, get_eolid, ...)
#     id <- get_eolid(x, ...)
    out <- classification(id, callopts=callopts, return_id=return_id, ...)
    names(out) <- x
  }
  if (db == 'col') {
    id <- process_ids(x, get_colid, ...)
#     id <- get_colid(x, ...)
    out <- classification(id, return_id=return_id, ...)
    names(out) <- x
  }
  if (db == 'tropicos') {
    id <- process_ids(x, get_tpsid, ...)
#     id <- get_tpsid(x, ...)
    out <- classification(id, callopts=callopts, return_id=return_id, ...)
    names(out) <- x
  }
  if (db == 'gbif') {
    id <- process_ids(x, get_gbifid, ...)
#     id <- get_gbifid(x, ...)
    out <- classification(id, callopts=callopts, ...)
    names(out) <- x
  }
  if (db == 'nbn') {
    id <- process_ids(x, get_nbnid, ...)
    out <- classification(id, callopts=callopts, return_id=return_id, ...)
    names(out) <- x
  }
  return(out)
}

process_ids <- function(input, fxn, ...){
  g <- tryCatch(as.numeric(as.character(input)), warning=function(e) e)
  if(is(g,"numeric")){
    id <- input
    class(id) <- "tsn"
  } else {
    id <- eval(fxn)(input, ...)
  }
  id
}

#' @method classification tsn
#' @export
#' @rdname classification
classification.tsn <- function(id, callopts = list(), return_id = FALSE, ...) 
{
  fun <- function(x){
    # return NA if NA is supplied
    if (is.na(x)) {
      out <- NA
    } else {
      out <- getfullhierarchyfromtsn(x, curlopts = callopts, ...)
      # remove overhang
      out <- out[1:which(out$tsn == x), c('taxonName', 'rankName', 'tsn')]
      names(out) <- c('name', 'rank', 'id')
      # Optionally return tsn of lineage
      if (!return_id) out <- out[, c('name', 'rank')]
      return(out)
    }
  }
  out <- lapply(id, fun)
  names(out) <- id
  structure(out, class='classification', db='itis')
}


#' @method classification uid
#' @export
#' @rdname classification
classification.uid <- function(id, return_id = FALSE, ...) {
  fun <- function(x){
    # return NA if NA is supplied
    if(is.na(x)){
      out <- NA
    } else {
      baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy"
      ID <- paste("ID=", x, sep = "")
      searchurl <- paste(baseurl, ID, sep = "&")
      tt <- getURL(searchurl)
      ttp <- xmlTreeParse(tt, useInternalNodes = TRUE)
      out <- data.frame(name = xpathSApply(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/ScientificName", xmlValue), 
                        rank = xpathSApply(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/Rank", xmlValue),
                        id = xpathSApply(ttp, "//TaxaSet/Taxon/LineageEx/Taxon/TaxId", xmlValue),
                        stringsAsFactors = FALSE)
      out <- rbind(out, c(xpathSApply(ttp, "//TaxaSet/Taxon/ScientificName", xmlValue),
                          xpathSApply(ttp, "//TaxaSet/Taxon/Rank", xmlValue),
                          xpathSApply(ttp, "//TaxaSet/Taxon/TaxId", xmlValue)))
      # Optionally return tsn of lineage
      if (!return_id) out <- out[, c('name', 'rank')]
      return(out)
    }
    # NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(out)
  }
  out <- lapply(id, fun)
  names(out) <- id
  structure(out, class='classification', db='ncbi')
}


#' @method classification eolid
#' @export
#' @rdname classification
classification.eolid <- function(id, key = NULL, callopts = list(), return_id = FALSE, ...) {
  common_names = NULL
  fun <- function(x){
    if(is.na(x)){ 
      out <- NA
    } else {
      url = 'http://www.eol.org/api/hierarchy_entries/1.0/'
      key <- getkey(key, "eolApiKey")
      urlget <- paste(url, x, '.json', sep="")
      args <- compact(list(common_names=common_names, synonyms=synonyms))
      tt <- GET(urlget, query=args, callopts)
      stop_for_status(tt)
      res <- content(tt)
      if(length(res$ancestors)==0){
        return(sprintf("No hierarchy information for %s", x))
      } else {
        out <- do.call(rbind.fill, lapply(res$ancestors, data.frame, stringsAsFactors = FALSE))[,c('scientificName','taxonRank', 'taxonID')]
        # add querried taxon
        out <- rbind(out, c(res$scientificName, res$taxonRank, x))
        names(out) <- c('name', 'rank', 'id')
        # Optionally return id of lineage
        if (!return_id) out <- out[, c('name', 'rank')]
        return(out)
      }
    }
  }
  out <- lapply(id, fun)
  names(out) <- id
  structure(out, class='classification', db='eol')
}

#' @method classification colid
#' @export
#' @rdname classification
classification.colid <- function(id, start = NULL, checklist = NULL, return_id = FALSE, ...) {
  fun <- function(x){
    # return NA if NA is supplied
    if(is.na(x)){
      out <- NA
    } else {
      url <- "http://www.catalogueoflife.org/col/webservice"
      if(!is.null(checklist)){
        cc <- match.arg(checklist, choices = c(2012, 2011, 2010, 2009, 2008, 2007))
        if (cc %in% c(2012, 2011, 2010)) {
          url <- gsub("col", paste("annual-checklist/", cc, sep = ""), url)
        } else {
          url <- "http://webservice.catalogueoflife.org/annual-checklist/year/search.php"
          url <- gsub("year", cc, url)
        }
      }
      
      args <- compact(list(id = x, response = "full", start = start))
      out <- getForm(url, .params = args)
      tt <- xmlParse(out)
      
      out <- data.frame(name = xpathSApply(tt, "//classification//name", xmlValue),
                        rank = xpathSApply(tt, "//classification//rank", xmlValue),
                        id  = xpathSApply(tt, "//classification//id", xmlValue),
                        stringsAsFactors = FALSE)
      # add querried taxon
      out <- rbind(out, c(xpathSApply(tt, "//result/name", xmlValue), 
                          xpathSApply(tt, "//result/rank", xmlValue),
                          xpathSApply(tt, "//result/id", xmlValue)))
      # Optionally return id of lineage
      if (!return_id) out <- out[, c('name', 'rank')]
    }
    return(out)
  }
  out <- lapply(id, fun)
  names(out) <- id
  structure(out, class='classification', db='col')
}


#' @method classification tpsid
#' @export
#' @rdname classification
classification.tpsid <- function(id, key = NULL, callopts = list(), return_id = FALSE, ...) {
  fun <- function(x){
    if(is.na(x)) {
      out <- NA
    } else {
      url <- sprintf('http://services.tropicos.org/Name/%s/HigherTaxa', x)
      key <- getkey(key, "tropicosApiKey")
      args <- taxize_compact(list(format='json', apikey=key))
      tt <- GET(url, query = args, callopts)
      stop_for_status(tt)
      out <- content(tt)
      if(names(out[[1]])[[1]] == "Error"){ 
        out <- data.frame(ScientificName=NA, Rank=NA) 
      } else {
        out <- do.call(rbind.fill, lapply(out, data.frame))[,c('ScientificName','Rank', 'NameId')]
      }
      names(out) <- c('name', 'rank', 'id')
      # Optionally return id of lineage
      if (!return_id) out <- out[, c('name', 'rank')]
    }
    return(out)
  }
  out <- lapply(id, fun)
  names(out) <- id
  structure(out, class='classification', db='tropicos')
}

#' @method classification gbifid
#' @export
#' @rdname classification
classification.gbifid <- function(id, callopts = list(), ...) {
  fun <- function(x){
    if(is.na(x)) {
      out <- NA
    } else {
      out <- suppressWarnings(tryCatch(gbif_name_usage(key = x), error=function(e) e))
      if(is(out, "simpleError")){ 
        out <- NA
      } else {
        #         out <- do.call(rbind.fill, lapply(out, data.frame))[,c('ScientificName','Rank')]
        out <- ldply(out[c('kingdom','phylum','clazz','order','family','genus','species')])
        out <- data.frame(name=out$V1, rank=out$.id)
      }
    }
    return(out)
  }
  out <- lapply(id, fun)
  names(out) <- id
  structure(out, class='classification', db='gbif')
}


#' @method classification nbnid
#' @export
#' @rdname classification
classification.nbnid <- function(id, callopts = list(), return_id = FALSE, ...) {
  fun <- function(x){
    if(is.na(x)) {
      out <- NA
    } else {
      out <- suppressWarnings(tryCatch(nbn_classifcation(id=x, ...), error=function(e) e))
      if(is(out, "simpleError")){ NA } else {
        out <- out[ , c('name','rank', 'taxonVersionKey')]
        names(out) <- c('name', 'rank', 'id')
        # Optionally return id of lineage
        if (!return_id) out <- out[, c('name', 'rank')]
        return(out)
      }
    }
  }
  out <- lapply(id, fun)
  names(out) <- id
  structure(out, class='classification', db='nbn')
}

#' @method classification ids
#' @export
#' @rdname classification
classification.ids <- function(id, ...) 
{
  fun <- function(x, ...){
    # return NA if NA is supplied
    if (all(is.na(x))) {
      out <- NA
    } else {
      out <- classification(x, ...)
    }
    return(out)
  }
  out <- lapply(id, fun)
  class(out) <- 'classification_ids'
  return(out)
}

#' @method cbind classification
#' @export
#' @rdname classification
cbind.classification <- function(x)
{
  gethiernames <- function(x){
    x$name <- as.character(x$name)
    x$rank <- as.character(x$rank)
    values <- data.frame(t(x[,'name']))
    names(values) <- tolower(x[,'rank'])
    return( values )
  }
  input <- x
  input <- input[sapply(input, class) %in% "data.frame"]
  tmp <- do.call(rbind.fill, lapply(input, gethiernames))
  tmp$query <- names(x)
  tmp$db <- attr(x, "db")
  tmp
}

#' @method rbind classification
#' @export
#' @rdname classification
rbind.classification <- function(x)
{
  input <- x
  db <- attr(input, "db")
  x <- input[vapply(x, class, "") %in% "data.frame"]
  for(i in seq_along(x)){
    x[[i]]$query <- names(x[i])
  }
  df <- do.call(rbind.fill, x)
  df$db <- db
  return( df )
}

#' @method cbind classification_ids
#' @export
#' @rdname classification
cbind.classification_ids <- function(...)
{
  input <- c(...)
  # remove non-data.frames
  input <- input[vapply(input, function(x) class(x[[1]]), "") %in% "data.frame"]
  
  gethiernames <- function(x){
    x$name <- as.character(x$name)
    x$rank <- as.character(x$rank)
    values <- data.frame(t(x[,'name']))
    names(values) <- tolower(x[,'rank'])
    return( values )
  }
  dat <- do.call(rbind.fill, lapply(input, function(h){ 
      tmp <- lapply(h, gethiernames)
      tmp <- do.call(rbind.fill, tmp)
      tmp$query <- names(h)
      tmp$db <- attr(h, "db")
      tmp
    })
  )
  move_col(tt=dat, y=c('query','db'))
}

move_col <- function(tt, y){
  tt[ c(names(tt)[ - sapply(y, function(m) grep(m, names(tt))) ], y) ]
}

#' @method rbind classification_ids
#' @export
#' @rdname classification
rbind.classification_ids <- function(...)
{
  input <- c(...)
  # remove non-data.frames
  input <- input[vapply(input, function(x) class(x[[1]]), "") %in% "data.frame"]

  df <- lapply(input, function(x){
    coll <- list()
    for(i in seq_along(x)){
      coll[[i]] <- data.frame(names(x[i]), x[i][[1]], stringsAsFactors = FALSE)
    }
    coll
  })
  
  get <- list()
  for(i in seq_along(df[[1]])){
    tmp <- do.call(rbind, lapply(df, "[[", i))
    source2 <- gsub("\\.[0-9]+", "", row.names(tmp))
    row.names(tmp) <- NULL
    names(tmp)[1] <- "query"
    tmp <- data.frame(db = source2, tmp, stringsAsFactors = FALSE)
    get[[i]] <- tmp
  }
  
  tt <- if(length(get) == 1) get[[1]] else do.call(rbind.fill, get)
  move_col(tt, c('query','db'))
}

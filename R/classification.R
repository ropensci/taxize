#' Retrieve the taxonomic hierarchy for a given taxon ID.
#' 
#' @import XML RCurl plyr
#' 
#' @param x character; taxons to query.
#' @param db character; database to query. either \code{ncbi}, \code{itis}, 
#'    \code{eol}, \code{col}, \code{tropicos}, \code{gbif}.
#' @param id character; identifiers, returned by \code{\link[taxize]{get_tsn}}, 
#'    \code{\link[taxize]{get_uid}}, \code{\link[taxize]{get_eolid}}, 
#'    \code{\link[taxize]{get_colid}}, \code{\link[taxize]{get_tpsid}}, 
#'    \code{\link[taxize]{get_gbifid}}.
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
#' @param callopts Further args passed on to httr::GET.
#' 
#' @return A named list of data.frames with the taxonomic classifcation of 
#'    every supplied taxa.
#' @note If IDs are supplied directly (not from the \code{get_*} functions) you 
#'    must specify the type of ID. There is a timeout of 1/3 seconds between 
#'    querries to NCBI.
#' 
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}}, 
#'    \code{\link[taxize]{get_eolid}}, \code{\link[taxize]{get_colid}}, 
#'    \code{\link[taxize]{get_tpsid}}, \code{\link[taxize]{get_gbifid}}
#' 
#' @export
#' @examples \dontrun{
#' # Plug in taxon names directly
#' classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'itis')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'eol')
#' classification(c("Chironomus riparius", "aaa vva"), db = 'col')
#' classification(c("Chironomus riparius", "asdfasdfsfdfsd"), db = 'gbif')
#' classification(c("Poa annua", "aaa vva"), db = 'tropicos')
#' 
#' # Use methods for get_uid, get_tsn, get_eolid, get_colid, get_tpsid
#' classification(get_uid(c("Chironomus riparius", "Puma concolor")))
#' 
#' classification(get_uid(c("Chironomus riparius", "aaa vva")))
#' classification(get_tsn(c("Chironomus riparius", "aaa vva")))
#' classification(get_eolid(c("Chironomus riparius", "aaa vva")))
#' classification(get_colid(c("Chironomus riparius", "aaa vva")))
#' classification(get_tpsid(c("Poa annua", "aaa vva")))
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
#' cl_uid <- classification(get_uid(c("Puma concolor","Accipiter striatus")))
#' cl_tsn <- classification(get_tsn(c("Puma concolor","Accipiter striatus")))
#' rbind(cl_col)
#' rbind(cl_uid)
#' rbind(cl_tsn)
#' cbind(cl_col)
#' cbind(cl_uid)
#' cbind(cl_tsn)
#' 
#' tsns <- get_tsn(c("Puma concolor","Accipiter striatus"))
#' cl_tsns <- classification(tsns)
#' cbind(cl_tsns)
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
classification.default <- function(x, db = NULL, ...){
  if (is.null(db))
    stop("Must specify db!")
  if (db == 'itis') {
    id <- get_tsn(x, ...)
    out <- classification(id, ...)
    names(out) <- x
  }
  if (db == 'ncbi') {
    id <- get_uid(x, ...)
    out <- classification(id, ...)
    names(out) <- x
  }
  if (db == 'eol') {
    id <- get_eolid(x, ...)
    out <- classification(id, ...)
    names(out) <- x
  }
  if (db == 'col') {
    id <- get_colid(x, ...)
    out <- classification(id, ...)
    names(out) <- x
  }
  if (db == 'tropicos') {
    id <- get_tpsid(x, ...)
    out <- classification(id, ...)
    names(out) <- x
  }
  if (db == 'gbif') {
    id <- get_gbifid(x, ...)
    out <- classification(id, ...)
    names(out) <- x
  }
  return(out)
}

#' @method classification tsn
#' @export
#' @rdname classification
classification.tsn <- function(id, ...) 
{
  fun <- function(x){
    # return NA if NA is supplied
    if (is.na(x)) {
      out <- NA
    } else {
    	out <- getfullhierarchyfromtsn(x, ...)
    	# remove overhang
    	out <- out[1:which(out$tsn == x), c('taxonName', 'rankName')]
      names(out) <- c('name', 'rank')
    	return(out)
    }
  }
  out <- lapply(id, fun)
  names(out) <- id
  class(out) <- 'classification'
  attr(out, 'db') <- 'itis'
  return(out)
}


#' @method classification uid
#' @export
#' @rdname classification
classification.uid <- function(id, ...) {
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
                        stringsAsFactors = FALSE)
      out <- rbind(out, c(xpathSApply(ttp, "//TaxaSet/Taxon/ScientificName", xmlValue),
        xpathSApply(ttp, "//TaxaSet/Taxon/Rank", xmlValue), 
        xpathSApply(ttp, "//TaxaSet/Taxon/TaxId", xmlValue)))
      return(out)
    }
    # NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(out)
  }
  out <- lapply(id, fun)
  names(out) <- id
  class(out) <- 'classification'
  attr(out, 'db') <- 'ncbi'
  return(out)
}


#' @method classification eolid
#' @export
#' @rdname classification
classification.eolid <- function(id, key = NULL, callopts = list(), ...) {
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
        out <- do.call(rbind.fill, lapply(res$ancestors, data.frame))[,c('scientificName','taxonRank')]
        names(out) <- c('name', 'rank')
        return(out)
      }
    }
  }
  out <- lapply(id, fun)
  names(out) <- id
  class(out) <- 'classification'
  attr(out, 'db') <- 'eol'
  return(out)
}

#' @method classification colid
#' @export
#' @rdname classification
classification.colid <- function(id, start = NULL, checklist = NULL, ...) {
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
                        stringsAsFactors = FALSE)
    }
    return(out)
  }
  out <- lapply(id, fun)
  names(out) <- id
  class(out) <- 'classification'
  attr(out, 'db') <- 'col'
  return(out)
}


#' @method classification tpsid
#' @export
#' @rdname classification
classification.tpsid <- function(id, key = NULL, callopts = list(), ...) {
  fun <- function(x){
    if(is.na(x)) {
      out <- NA
    } else {
      url <- sprintf('http://services.tropicos.org/Name/%s/HigherTaxa', x)
      key <- getkey(key, "tropicosApiKey")
      args <- compact(list(format='json', apikey=key))
      tt <- GET(url, query = args, callopts)
      stop_for_status(tt)
      out <- content(tt)
      if(names(out[[1]])[[1]] == "Error"){ 
        out <- data.frame(ScientificName=NA, Rank=NA) 
      } else {
        out <- do.call(rbind.fill, lapply(out, data.frame))[,c('ScientificName','Rank')]
      }
      names(out) <- c('name', 'rank')
    }
    return(out)
  }
  out <- lapply(id, fun)
  names(out) <- id
  class(out) <- 'classification'
  return(out)
}

#' @method classification gbifid
#' @export
#' @rdname classification
classification.gbifid <- function(id, callopts = list(), ...) {
  fun <- function(x){
    if(is.na(x)) {
      out <- NA
    } else {
      out <- suppressWarnings(tryCatch(name_usage(key = x, ...), error=function(e) e))
      if(is(out, "simpleError")){ 
        df <- NA
      } else {
#         out <- do.call(rbind.fill, lapply(out, data.frame))[,c('ScientificName','Rank')]
        df <- ldply(out[c('kingdom','phylum','clazz','order','family','genus','species')])
        df <- data.frame(name=df$V1, rank=df$.id)
      }
    }
    return( df )
  }
  out <- lapply(id, fun)
  names(out) <- id
  class(out) <- 'classification'
  return(out)
}

#' @method classification ids
#' @export
#' @rdname classification
classification.ids <- function(id, ...) 
{
  fun <- function(x, ...){
    # return NA if NA is supplied
    if (is.na(x)) {
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
  do.call(rbind.fill, lapply(input, gethiernames))
}

#' @method rbind classification
#' @export
#' @rdname classification
rbind.classification <- function(x)
{
  input <- x
  db <- attr(input, "db")
  x <- input[vapply(x, class, "") %in% "data.frame"]
  df <- do.call(rbind, x)
  df <- data.frame(source = db, taxonid = gsub("\\.[0-9]+", "", row.names(df)), df)
  row.names(df) <- NULL
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
  do.call(rbind.fill, lapply(input, function(x){ 
      tmp <- lapply(x, gethiernames)
      do.call(rbind.fill, tmp)
    })
  )
#   # sort columns by rank order
#   rank_ref$ranks[names(df) %in% tolower(rank_ref$ranks)]
#   grep(names(values)[[2]], tolower(rank_ref$ranks))
#   torank <- sapply(rank_ref[grep(downto, rank_ref$ranks):nrow(rank_ref),"ranks"], function(x) strsplit(x, ",")[[1]][[1]], USE.NAMES=F)
}

#' @method rbind classification_ids
#' @export
#' @rdname classification
rbind.classification_ids <- function(...)
{
  input <- c(...)
  # remove non-data.frames
  input <- input[vapply(input, function(x) class(x[[1]]), "") %in% "data.frame"]
  #   df <- do.call(rbind, lapply(input, function(x){
  # #             lapply(x, function(y) data.frame(names(y), y[[1]]))
  #           coll <- list()
  #           for(i in seq_along(x)){
  #             coll[[i]] <- data.frame(names(x[i]), x[i][[1]])
  #           }
  #     do.call(rbind, coll)
  #   }))
  
  df <- lapply(input, function(x){
    coll <- list()
    for(i in seq_along(x)){
      coll[[i]] <- data.frame(names(x[i]), x[i][[1]])
    }
    coll
  })
  
  get <- list()
  for(i in seq_along(df[[1]])){
    tmp <- do.call(rbind, lapply(df, "[[", i))
    source2 <- gsub("\\.[0-9]+", "", row.names(tmp))
    row.names(tmp) <- NULL
    names(tmp)[1] <- "taxonid"
    tmp <- data.frame(source = source2, tmp)
    get[[i]] <- tmp
  }
  
  if(length(get) == 1) 
    get[[1]]
  else
    get
  #   source2 <- gsub("\\.[0-9]+", "", row.names(df))
  #   row.names(df) <- NULL
  #   names(df)[1] <- "taxonid"
  #   df <- data.frame(source = source2, df)
#   return( res )
}
#' Retrieve synonyms from various sources given input taxonomic names or identifiers.
#' 
#' @param x character; taxons to query.
#' @param db character; database to query. either \code{itis}, \code{tropicos}, \code{worms}
#' or \code{ubio}.
#' @param id character; identifiers, returned by \code{\link[taxize]{get_tsn}}, 
#'    \code{\link[taxize]{get_tpsid}}, \code{\link[taxize]{get_wormsid}},
#'    or \code{\link[taxize]{get_ubioid}}
#' @param ... Other passed arguments.
#' 
#' @return A named list of data.frames with the synonyms of every supplied taxa.
#' @note If IDs are supplied directly (not from the \code{get_*} functions) you 
#'    must specify the type of ID.
#' 
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_tpsid}}, or 
#' \code{\link[taxize]{get_wormsid}}
#' 
#' @export
#' @examples \dontrun{
#' # Plug in taxon names directly
#' synonyms("Poa annua", db="itis")
#' synonyms(c("Poa annua",'Pinus contorta','Puma concolor'), db="itis")
#' synonyms("Poa annua", db="tropicos")
#' synonyms("Pinus contorta", db="tropicos")
#' synonyms(c("Poa annua",'Pinus contorta'), db="tropicos")
#' synonyms("Salvelinus fontinalis", db='worms')
#' synonyms("Salmo friderici", db='ubio')
#' synonyms(c("Salmo friderici",'Carcharodon carcharias','Puma concolor'), db="ubio")
#' 
#' # Use methods for get_tsn, get_tpsid, or get_wormsid
#' synonyms(get_tsn("Poa annua"))
#' synonyms(get_tpsid("Poa annua"))
#' synonyms(get_wormsid("Salvelinus fontinalis"))
#' synonyms(get_ubioid("Carcharodon carcharias"))
#' 
#' # Pass many ids from class "ids"
#' out <- get_ids(names="Poa annua", db = c('itis','tropicos'))
#' synonyms(out)
#' out <- get_ids(names="Salvelinus fontinalis", db = c('itis','worms'))
#' synonyms(out)
#' }

synonyms <- function(...){
  UseMethod("synonyms")
}

#' @method synonyms default
#' @export
#' @rdname synonyms
synonyms.default <- function(x, db = NULL, ...){
  if (is.null(db))
    stop("Must specify Identifier!")
  if (db == 'itis') {
    id <- get_tsn(x, ...)
    out <- synonyms(id, ...)
    names(out) <- x
  }
  if (db == 'tropicos') {
    id <- get_tpsid(x, ...)
    out <- synonyms(id, ...)
    names(out) <- x
  }
  if (db == 'worms') {
    id <- get_wormsid(x, ...)
    out <- synonyms(id, ...)
    names(out) <- x
  }
  if (db == 'ubio') {
    id <- get_ubioid(x, searchtype = 'scientific', ...)
    out <- synonyms(id, ...)
    names(out) <- x
  }
  return(out)
}

#' @method synonyms tsn
#' @export
#' @rdname synonyms
synonyms.tsn <- function(id, ...) 
{
  fun <- function(x){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- getsynonymnamesfromtsn(x, ...)
      if(as.character(out[1,1]) == 'nomatch')
        names(out) <- c('name','tsn')
    }
    out
  }
  tmp <- lapply(id, fun)
  names(tmp) <- id
  return(tmp)
#   return( lapply(id, fun) )
}

#' @method synonyms tpsid
#' @export
#' @rdname synonyms
synonyms.tpsid <- function(id, ...) 
{
  fun <- function(x){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- tp_synonyms(x, ...)$synonyms
    }
    out
  }
  tmp <- lapply(id, fun)
  names(tmp) <- id
  return(tmp)
#   return( lapply(id, fun) )
}

#' @method synonyms wormsid
#' @export
#' @rdname synonyms
synonyms.wormsid <- function(id, ...) 
{
  fun <- function(x){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- worms_synonyms(x, ...)[,c("AphiaID","scientificname","status","rank","valid_AphiaID","valid_name")]
    }
    out
  }
  tmp <- lapply(id, fun)
  names(tmp) <- id
  return(tmp)
#   return( lapply(id, fun) )
}

#' @method synonyms ubioid
#' @export
#' @rdname synonyms
synonyms.ubioid <- function(id, ...) 
{
  fun <- function(x){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- ubio_id(namebankID = x, ...)[['synonyms']]
    }
    out
  }
  tmp <- lapply(id, fun)
  names(tmp) <- id
  return(tmp)
}


#' @method synonyms ids
#' @export
#' @rdname synonyms
synonyms.ids <- function(id, ...) 
{
  fun <- function(x){
    if (is.na(x)) {
      out <- NA
    } else {
      out <- synonyms(x, ...)
    }
    return( out )
  }
  return( lapply(id, fun) )
}

# x <- "Poa annua"
# # ubioout <- ubio_search(searchName=x, sci = 1) 
# # let's use 5408026
# # ubiodat <- ubio_synonyms(hierarchiesID = 5408026)
# 
# searchbyscientificname(srchkey=x) # let's use 41107
# getsynonymnamesfromtsn(tsn = 41107)
# 
# tpout <- tp_search(name = 'Poa annua')
# tp_search('Pinus contorta')
# head(tpout) # use 25509881
# tp_synonyms(id = 25509881)
# tp_synonyms(id = 24900183)

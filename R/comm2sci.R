#' Get scientific names from common names.
#' 
#' @param commnames One or more common names or partial names.
#' @param db Data source, one of eol (default), itis, or tropicos
#' @param itisby Search for common names across entire names (search, default),
#'    at beginning of names (begin), or at end of names (end). 
#' @param ... Further arguments passed on to functions...
#' @return List of data.frame's.
#' @seealso \code{\link[taxize]{searchbycommonname}}, 
#' \code{\link[taxize]{searchbycommonnamebeginswith}}, 
#' \code{\link[taxize]{searchbycommonnameendswith}}, \code{\link[taxize]{eol_search}},
#' \code{\link[taxize]{tp_search}}
#' @export
#' @seealso \code{\link[taxize]{sci2comm}}
#' @author Scott Chamberlain (myrmecocystus@@gmail.com)
#' @examples \dontrun{
#' comm2sci(commnames='black bear')
#' comm2sci(commnames='black bear', db='itis')
#' comm2sci(commnames='inch', db='itis', itisby='begin')
#' comm2sci(commnames='snake', db='itis', itisby='end')
#' comm2sci(commnames='annual blue grass', db='tropicos')
#' comm2sci(commnames=c('annual blue grass','tree of heaven'), db='tropicos')
#' do.call(rbind.fill, comm2sci(commnames=c('annual blue grass','tree of heaven'), db='tropicos'))
#' comm2sci(commnames=c('black bear', 'roe deer'))
#' }
comm2sci <- function(commnames, db='eol', itisby='search', ...)
{  
  getsci <- function(nn, ...){
    switch(db, 
           eol = eol_search(terms=nn, ...),
           itis = itiscommnamesearch(nn, itisby, ...),
           tropicos = tp_search(commonname = nn, ...))
  }
  temp <- lapply(commnames, function(x) getsci(x, ...))
  names(temp) <- commnames
  temp
}

#' @export
#' @keywords internal
itiscommnamesearch <- function(x, by='search', ...){
  tmp <- switch(by, 
                search = searchbycommonname(x, ...),
                begin = searchbycommonnamebeginswith(x, ...),
                end = searchbycommonnameendswith(x, ...))
  # remove empty tsn slots
  tsns <- as.character(tmp$tsn)
  tsns <- tsns[!sapply(tsns, nchar)==0]
  # get scientific names
  do.call(rbind, lapply(tsns, getscientificnamefromtsn))
}
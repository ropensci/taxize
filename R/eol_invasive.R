#' Search for presence of taxonomic names in EOL invasive species databases.
#' 
#' If a two-column data.frame is returned with the species you searched for, your 
#' species is listead in the database you searched. If your searched taxon is not found,
#' we return a string "<your species name> was not found".
#'
#' Beware that some datasets are quite large, and may take 30 sec to a minute to 
#' pull down all data before we can search for your species. Note there is no
#' parameter in this API method for searching by taxon name.
#' 
#' This function is vectorized, so you can pass a single name or a vector of names.
#'   
#' @import httr plyr
#' @param name A taxonomic name, or a vector of names.
#' @param dataset One of all, gisd100, gisd, gris, isc, daisie, i3n, or mineps.
#'    See the Details for what each dataset ID. 
#' @param page A maximum of 30 results are returned per page. This parameter allows 
#'    you to fetch more pages of results if there are more than 30 matches (Default 1)
#' @param per_page Results to get per page
#' @param key Your EOL API key; loads from .Rprofile.
#' @param callopts Further args passed on to GET.
#' @details It's possible to return JSON or XML with the EOL API. However, 
#'   	this function only returns JSON for now. 
#'     
#' Options for the dataset parameter are 
#' \itemize{
#'  \item all - All datasets
#'  \item gisd100 - 100 of the World's Worst Invasive Alien Species (Global Invasive Species Database)
#'  \item gisd - Global Invasive Species Database 2013
#'  \item gris - Global Register of Invasive Species (GRIS) Taxa
#'  \item isc - Centre for Agriculture and Biosciences International Invasive Species Compendium (ISC)
#'  \item daisie - Delivering Alien Invasive Species Inventories for Europe (DAISIE) Species List
#'  \item i3n - IABIN Invasives Information Network (I3N) Species
#'  \item mineps - Marine Invaders of the NE Pacific Species
#' }
#' @return A list of data.frame's/strings with results, with each element named by
#' the input elements to the name parameter.
#' @references See info for each data source at \url{http://eol.org/collections/55367/taxa}
#' @examples \dontrun{
#' eol_invasive(name='Brassica oleracea', dataset='gisd')
#' eol_invasive(name='Sargassum', dataset='gisd')
#' eol_invasive(name='Ciona intestinalis', dataset='mineps')
#' eol_invasive(name='Rhinella marina', dataset='gris')
#' eol_invasive(name=c('Rhinella marina','Sturnus vulgaris','Cygnus olor','Pinus concolor'), dataset='gris')
#' eol_invasive(name=c('Lymantria dispar','Cygnus olor','Hydrilla verticillata','Pinus concolor'), dataset='i3n')
#' eol_invasive(name=c('Branta canadensis','Gallus gallus','Myiopsitta monachus'), dataset='daisie')
#' eol_invasive(name=c('Branta canadensis','Gallus gallus','Myiopsitta monachus'), dataset='isc')
#' }
#' @export
eol_invasive <- function(name = NULL, dataset="all", searchby = grep, page=NULL, 
  per_page=NULL, key = NULL, callopts=list())
{     
  if(is.null(name)) stop("please provide a taxonomic name")
  if(is.null(dataset)) stop("please provide a dataset name")
  datasetid <- switch(dataset,
           all = 55367,
           gisd100 = 54500,
           gisd = 54983,
           gris = 55288,
           isc = 55180,
           daisie = 55179,
           i3n = 55176,
           mineps = 55331)
  url = 'http://eol.org/api/collections/1.0.json'
  key <- getkey(key, "eolApiKey")

  args <- compact(list(id=datasetid,page=page,per_page=500,filter='taxa'))
  tt <- GET(url, query=args, callopts)
  stop_for_status(tt)
  res <- content(tt) 
  data_init <- res$collection_items
  message(sprintf("Getting data for %s names...", res$total_items))
  
  pages_left <- function(){
    tot <- res$total_items
    got <- length(res$collection_items)
    if(got < tot){
      seq(1, ceiling((tot-got)/500), 1)+1
    }
  }
  pages_get <- pages_left()
  
  if(!is.null(pages_get)){  
    out <- list()
    for(i in seq_along(pages_get)){
      args <- compact(list(id=datasetid,page=pages_get[i],per_page=500,filter='taxa'))
      tt <- GET(url, query=args, callopts)
      stop_for_status(tt)
      res <- content(tt)
      out[[i]] <- res$collection_items
    }
    res2 <- compact(out)
    dat_all <- do.call(c, list(data_init, do.call(c, res2)))
    dat_all <- lapply(dat_all, "[", c("name","object_id"))
    dat <- do.call(rbind, lapply(dat_all, data.frame, stringsAsFactors=FALSE))
  } else
  {
    dat_all <- lapply(data_init, "[", c("name","object_id"))
    dat <- do.call(rbind, lapply(dat_all, data.frame, stringsAsFactors=FALSE))
  }
  
  # search by name
  getmatches <- function(x){
    matched <- searchby(x, dat$name)
    if(identical(matched, integer(0))){
      data.frame(name = x, object_id = NA)
    } else
    {
      dat[matched,]
    }
  }
  tmp <- lapply(name, getmatches)
  names(tmp) <- name
  df <- ldply(tmp)
  df$db <- dataset
  names(df)[c(1,3)] <- c("searched_name","eol_object_id")
  df
}
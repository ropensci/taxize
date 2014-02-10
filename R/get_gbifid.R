#' Get the GBIF backbone taxon ID from taxonomic names.
#' 
#' @import plyr RCurl
#' @param sciname character; scientific name.
#' @param ask logical; should get_colid be run in interactive mode? 
#' If TRUE and more than one ID is found for the species, the user is asked for 
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the 
#'    console.
#' 
#' @return A vector of unique identifiers. If a taxon is not found NA. 
#' If more than one ID is found the function asks for user input. 
#' 
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}},
#' \code{\link[taxize]{get_tpsid}}, \code{\link[taxize]{get_eolid}},
#' \code{\link[taxize]{get_colid}}
#' 
#' @export
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#' 
#' @details Internally in this function we use a function to search GBIF's taxonomy, 
#' and if we find an exact match we return the ID for that match. If there isn't an 
#' exact match we return the options to you to pick from. 
#' 
#' @examples \dontrun{
#' get_gbifid(sciname='Poa annua')
#' get_gbifid(sciname='Pinus contorta')
#' get_gbifid(sciname='Puma concolor')
#' 
#' get_gbifid(c("Poa annua", "Pinus contorta"))
#' 
#' # When not found
#' get_gbifid(sciname="uaudnadndj")
#' get_gbifid(c("Chironomus riparius", "uaudnadndj"))
#' }

get_gbifid <- function(sciname, ask = TRUE, verbose = TRUE){
  fun <- function(sciname, ask, verbose) {
    mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
    df <- gbif_name_suggest(q=sciname, fields = c("key","canonicalName","rank"))
    
    if(is.null(df))
      df <- data.frame(NULL)
    
    if(nrow(df)==0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
    } else
    {
      names(df)[1] <- 'gbifid'
      id <- df$gbifid
    }
    
    # not found
    if(length(id) == 0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
    }
  
    # more than one found -> user input
    if(length(id) > 1){
      # check for exact match
      matchtmp <- df[df$canonicalName %in% sciname, "gbifid"]
      if(length(matchtmp) == 1){
        id <- as.character(matchtmp)
      } else 
      {
        if(ask){
          rownames(df) <- 1:nrow(df)
          # prompt
          message("\n\n")
          message("\nMore than one eolid found for taxon '", sciname, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")      
          print(df)
          take <- scan(n = 1, quiet = TRUE, what = 'raw')
          
          if(length(take) == 0)
            take <- 'notake'
          if(take %in% seq_len(nrow(df))){
            take <- as.numeric(take)
            message("Input accepted, took gbifid '", as.character(df$gbifid[take]), "'.\n")
            id <- as.character(df$gbifid[take])
          } else {
            id <- NA
            mssg(verbose, "\nReturned 'NA'!\n\n")
          }
        } else{
          id <- NA
        }
      }
    }  
    return(id)
  }
  out <- laply(sciname, fun, ask, verbose)
  class(out) <- "gbifid"
  return(out)
}

gbif_name_suggest <- function(q=NULL, datasetKey=NULL, rank=NULL, fields=NULL, start=NULL, 
                         limit=20, callopts=list())
{
  url = 'http://api.gbif.org/v0.9/species/suggest'
  args <- compact(list(q=q, rank=rank, offset=start, limit=limit))
  temp <- GET(url, query=args, callopts)
  stop_for_status(temp)
  tt <- content(temp)
  if(is.null(fields)){
    toget <- c("key","scientificName","rank")
  } else { toget <- fields }
  matched <- sapply(toget, function(x) x %in% gbif_suggestfields())
  if(!any(matched))
    stop(sprintf("the fields %s are not valid", paste0(names(matched[matched == FALSE]),collapse=",")))
  out <- lapply(tt, function(x) x[names(x) %in% toget])
  do.call(rbind.fill, lapply(out,data.frame))
}

#' Fields available in gbif_suggest function
#' @export
#' @keywords internal
gbif_suggestfields <- function(){  
  c("key","datasetTitle","datasetKey","nubKey","parentKey","parent",
    "kingdom","phylum","clazz","order","family","genus","species",
    "kingdomKey","phylumKey","classKey","orderKey","familyKey","genusKey",
    "speciesKey","scientificName","canonicalName","authorship",
    "accordingTo","nameType","taxonomicStatus","rank","numDescendants",
    "numOccurrences","sourceId","nomenclaturalStatus","threatStatuses",
    "synonym")
}
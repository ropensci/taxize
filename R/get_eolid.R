#' Get the EOL ID from Encyclopedia of Life from taxonomic names.
#' 
#' Note that EOL doesn't expose an API endpointn for directly querying for EOL
#' taxon ID's, so we first use the function \code{\link[taxize]{eol_search}} to find pages
#' that deal with the species of interest, then use \code{\link[taxize]{eol_pages}}
#' to find the actual taxon IDs. 
#' 
#' @import plyr RCurl
#' @importFrom reshape sort_df
#' @param sciname character; scientific name.
#' @param ask logical; should get_eolid be run in interactive mode? 
#' If TRUE and more than one ID is found for the species, the user is asked for 
#' input. If FALSE NA is returned for multiple matches.
#' @param key API key
#' @param ... Further args passed on to eol_search()
#' @param verbose logical; If TRUE the actual taxon queried is printed on the 
#'    console.
#' 
#' @return A vector of unique identifiers (EOL). If a taxon is not found NA. 
#' If more than one ID is found the function asks for user input. 
#' 
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{get_uid}},
#' \code{\link[taxize]{get_tpsid}}
#' 
#' @export
#' @author Scott Chamberlain, \email{myrmecocystus@@gmail.com}
#' 
#' @examples \dontrun{
#' get_eolid(sciname='Poa annua')
#' get_eolid(sciname='Pinus contorta')
#' get_eolid(sciname='Puma concolor')
#' 
#' get_eolid(c("Poa annua", "Pinus contorta"))
#' 
#' # When not found
#' get_eolid(sciname="uaudnadndj")
#' get_eolid(c("Chironomus riparius", "uaudnadndj"))
#' }
get_eolid <- function(sciname, ask = TRUE, verbose = TRUE, key = NULL, ...){
  fun <- function(sciname, ask, verbose) {
    mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
    tmp <- eol_search(terms = sciname, key, ...)
    
    ms="Not found. Consider checking the spelling or alternate classification"
    
    if(all(is.na(tmp))){
      mssg(verbose, ms)
      id <- NA
    } else {   
      pageids <- tmp[grep(tolower(sciname), tolower(tmp$name)), "pageid"]
      
      if(length(pageids) == 0){
        if(nrow(tmp)>0)
        mssg(verbose, paste(ms, sprintf('\nDid find: %s', paste(tmp$name, collapse = "; "))))
        id <- NA
      } else
      {
        dfs <- compact(lapply(pageids, function(x) eol_pages(x)$scinames))
        dfs <- ldply(dfs[!sapply(dfs, nrow)==0])
        df <- dfs[,c('identifier','scientificname','nameaccordingto')]
        names(df) <- c('eolid','name','source')
        df <- getsourceshortnames(df)
        
        if(nrow(df) == 0){
          mssg(verbose, ms)
          id <- NA
        } else{ 
          id <- df$eolid 
        }
      }
    }

    # not found on eol
    if(length(id) == 0){
      mssg(verbose, ms)
      id <- NA
    }
    # more than one found on eol -> user input
    if(length(id) > 1){
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
          message("Input accepted, took eolid '", as.character(df$eolid[take]), "'.\n")
          id <- as.character(df$eolid[take])
        } else {
          id <- NA
          mssg(verbose, "\nReturned 'NA'!\n\n")
        }
      } else{
        id <- NA
      }
    }  
    return(id)
  }
  out <- laply(sciname, fun, ask, verbose)
  class(out) <- "eolid"
  return(out)
}

getsourceshortnames <- function(input){  
  lookup <- data.frame(z=c('COL','ITIS','GBIF','NCBI','IUCN'),
                       b=c('Species 2000 & ITIS Catalogue of Life: April 2013',
                           'Integrated Taxonomic Information System (ITIS)',
                           'GBIF Nub Taxonomy',
                           'NCBI Taxonomy',
                           'IUCN Red List (Species Assessed for Global Conservation)'))
  bb <- merge(input, lookup, by.x="source", by.y="b")[,-1]
  names(bb)[3] <- "source"
  sort_df(bb, "name")
}
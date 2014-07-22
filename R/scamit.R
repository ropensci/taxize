#' Search Catalogue of Life for taxonomic IDs
#' 
#' @export
#' @param name The string to search for. Only exact matches found the name given 
#'     will be returned, unless one or wildcards are included in the search 
#'   	string. An * (asterisk) character denotes a wildcard; a % (percentage) 
#'    character may also be used. The name must be at least 3 characters long, 
#'    not counting wildcard characters.
#' @param rank Taxonomic rank to search against. One of phylum, subphylum, class, subclass, 
#'    infraclass, superorder, order, suborder, infraorder, superfamily, family, subfamily, 
#'    tribe, genus, subgenus, or species
#' @param ... Further args passed on to \code{agrep()}. See \code{?agrep}
#' 
#' @note Note that we set \code{ignore.case=TRUE} by default.
#'    
#' @examples
#' # Compare 
#' scamit_search(name="Cliona", rank="genus")
#' scamit_search(name="Cliona", rank="genus", max.distance = 0)
#' scamit_search(name="Silicea", rank="phylum")

scamit_search <- function(name, rank='genus', ...)
{
  searchscamit <- function(x){
    if(!exists('scamit')){
      scamit <- readRDS("SCAMIT_ed8.rds")
      names(scamit) <- tolower(names(scamit))
    }
    matches <- scamit[ agrep(name, scamit[[rank]], ignore.case = TRUE, ...), ]
    if(NROW(matches) == 0){ NULL } else {
      matches
    }
  }
  temp <- lapply(name, searchscamit)
  names(temp) <- name
  temp
}

#' Get the SCAMIT ID from taxonomic names.
#' 
#' @param sciname character; scientific name.
#' @param ask logical; should get_scamitid be run in interactive mode? 
#' If TRUE and more than one ID is found for the species, the user is asked for 
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the console.
#' @examples
#' get_scamitid(name='Cliona', rank='genus')

get_scamitid <- function(name, rank = 'genus', ask = TRUE, verbose = TRUE){
  fun <- function(name, rank, ask, verbose) {
    mssg(verbose, "\nRetrieving data for taxon '", name, "'\n")
    df <- scamit_search(name=name, rank=rank)[[1]]
    
    rank_taken <- NA
    if(nrow(df)==0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
    } else
    {
      df <- df[,c('speciesid','family','genus','species')]
      names(df)[1] <- 'scamitid'
      id <- df$scamitid
      if(!is.null(rank)){
        rank_taken <- rank
        df$rank <- rank
      }
      else{
        # FIXME, need to assign rank to each match when rank not given
        rank_taken <- as.character(df$rank)
        df$rank <- "fixme"
      }
    }
    
    # not found on col
    if(length(id) == 0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      id <- NA
    }
    # more than one found -> user input
    if(length(id) > 1){
      if(ask){
        rownames(df) <- 1:nrow(df)
        # prompt
        message("\n\n")
        message("\nMore than one colid found for taxon '", name, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")      
        print(df)
        take <- scan(n = 1, quiet = TRUE, what = 'raw')
        
        if(length(take) == 0)
          take <- 'notake'
        if(take %in% seq_len(nrow(df))){
          take <- as.numeric(take)
          message("Input accepted, took scamitid '", as.character(df$scamit[take]), "'.\n")
          id <- as.character(df$scamitid[take])
          rank_taken <- as.character(df$rank[take])
        } else {
          id <- NA
          mssg(verbose, "\nReturned 'NA'!\n\n")
        }
      } else{
        id <- NA
      }
    }  
    #     return(id)
    return( c(id=id, rank=rank_taken) )
  }
  name <- as.character(name)
  out <- lapply(name, fun, rank=rank, ask=ask, verbose=verbose)
  ids <- sapply(out, "[[", "id")
  class(ids) <- "scamitid"
  attr(ids, 'uri') <- "local dataset: scamit v8"
  return(ids)
}

#' @method classification sacmitid
#' @export
#' @rdname classification
classification.scamitid <- function(id, ...) 
{
  fun <- function(x){
    # return NA if NA is supplied
    if (is.na(x)) {
      out <- NA
    } else {
      out <- getscamithierarchy(x)
      return(out)
    }
  }
  out <- lapply(id, fun)
  names(out) <- id
  class(out) <- 'classification'
  attr(out, 'db') <- 'scamit'
  return(out)
}

#' Get scamit hierarchy
#' @param id Scamit identifier
getscamithierarchy <- function(id){
  if(!exists('scamit')){
    scamit <- readRDS("SCAMIT_ed8.rds")
    names(scamit) <- tolower(names(scamit))
  }
  getranks <- c('phylum','subphylum','class','subclass','infraclass','superorder','order','suborder','infraorder','superfamily','family','subfamily','tribe','genus','subgenus','species','describer')
  dat <- scamit[ scamit$speciesid %in% id, getranks ]
  data.frame(rank=names(dat), name=unlist(unname(dat)), stringsAsFactors = FALSE)
}
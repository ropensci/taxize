#' Get the UID codes from NCBI for taxonomic names.
#' 
#' Retrieve the Unique Identifier (UID) of a taxon from NCBI taxonomy browser.
#' 
#' @import plyr RCurl
#' @param sciname character; scientific name.
#' @param ask logical; should get_tsn be run in interactive mode? 
#' If TRUE and more than one TSN is found for the species, the user is asked for 
#' input. If FALSE NA is returned for multiple matches.
#' @param verbose logical; If TRUE the actual taxon queried is printed on the 
#'    console.
#' 
#' @return A vector of unique identifiers (UID). If a taxon is not found NA. 
#' If more than one UID is found the function asks for user input. 
#' 
#' @seealso \code{\link[taxize]{get_tsn}}, \code{\link[taxize]{classification}}
#' 
#' @export
#' @author Eduard Szoecs, \email{szoe8822@@uni-landau.de}
#' 
#' @examples \dontrun{
#' get_uid(c("Chironomus riparius", "Chaetopteryx"))
#' get_uid(c("Chironomus riparius", "aaa vva"))
#' 
#' # When not found
#' get_uid("howdy")
#' get_uid(c("Chironomus riparius", "howdy"))
#' 
#' # multiple matches
#' get_uid('Dugesia')  # user prompt needed
#' get_uid('Dugesia', ask = FALSE) # returns NA for multiple matches
#' 
#' }
get_uid <- function(sciname, ask = TRUE, verbose = TRUE){
  fun <- function(sciname, ask, verbose) {
    mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
    sciname <- gsub(" ", "+", sciname)
    searchurl <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=taxonomy&term=", 
                       sciname, sep = "")
    # NCBI limits requests to three per second
    xml_result <- xmlParse(getURL(searchurl))
    Sys.sleep(0.33)
    uid <- xpathSApply(xml_result, "//IdList/Id", xmlValue)    
    # not found on ncbi
    if (length(uid) == 0){
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      uid <- NA
    }
    # more than one found on ncbi -> user input
    if(length(uid) > 1){
      if(ask){
        baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=taxonomy"
        ID <- paste("ID=", paste(uid, collapse= ","), sep = "")
        searchurl <- paste(baseurl, ID, sep = "&")
        tt <- getURL(searchurl)
        ttp <- xmlTreeParse(tt, useInternalNodes = TRUE)
        df <- ldply(xmlToList(ttp), data.frame)
        df <- df[df$Item..attrs != 'String', c(2,5, 7)]
        names(df) <- c("UID", "Rank", "Division")
        rownames(df) <- 1:nrow(df)
        
        # prompt
        message("\n\n")
        message("\nMore than one UID found for taxon '", sciname, "'!\n
            Enter rownumber of taxon (other inputs will return 'NA'):\n")      
        print(df)
        take <- scan(n = 1, quiet = TRUE, what = 'raw')
        
        if(length(take) == 0)
          take <- 'notake'
        if(take %in% seq_len(nrow(df))){
          take <- as.numeric(take)
          message("Input accepted, took UID '", as.character(df$UID[take]), "'.\n")
          uid <- as.character(df$UID[take])
        } else {
          uid <- NA
          mssg(verbose, "\nReturned 'NA'!\n\n")
        }
      } else {
        uid <- NA
      }
    }  
    return(uid)
  }
  out <- laply(sciname, fun, ask, verbose)
  class(out) <- "uid"
  return(out)
}
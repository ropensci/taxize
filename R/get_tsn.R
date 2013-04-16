#' Get the TSN code for a search term.
#' 
#' \code{get_tsn} uses a variety of functions from the \code{itis} function.
#' 
#' @import plyr RSQLite
#' @param searchterm A vector of common or scientific names.
#' @param searchtype One of 'sciname', 'anymatch', 'comnamebeg', 'comname', 
#'    'comnameend'.
#' @param verbose should progress be printed?
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#'  queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @return A vector of taxonomic serial numbers (TSN). If a species is not found NA. 
#' 		If more than one TSN is found the function asks for user input.
#' 		See functions in the \code{itis} function.
#' @export
#' @examples \dontrun{
#' get_tsn(searchterm="Quercus douglasii", searchtype="sciname", locally=FALSE)
#' 
#' taxize:::sqlite_init(path="~/github/ropensci/sql/itis2.sqlite")
#' get_tsn(searchterm="Quercus douglasii", searchtype="sciname", locally=TRUE)
#' get_tsn(searchterm=c("Chironomus riparius","Quercus douglasii"), searchtype="sciname", locally=TRUE)
#' splist <- c("annona cherimola", 'annona muricata', "quercus robur", 
#' 		"shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus", "Tamandua tetradactyla",
#' 		"Tarsius syric", "Sylvilagus dicei", "Galeopterus variegatus")
#' get_tsn(splist, "sciname", locally=TRUE)
#' get_tsn(splist, "sciname")
#' 
#' # By common names
#' get_tsn(searchterm=c("polar bear", "ferret-badger", "american bullfrog"), searchtype = "comname", locally=TRUE)
#' }
get_tsn <- function(searchterm, searchtype = "sciname", verbose = TRUE, locally=FALSE) 
{
	if(locally)
	{
		if(searchtype == "sciname"){ tsn_df <- searchbyscientificname(searchterm, locally=locally, returnindex=TRUE) } else
			if(searchtype == "anymatch") { tsn_df <- searchforanymatch(searchterm, locally=locally, returnindex=TRUE) } else
				if(searchtype == "comnamebeg") { tsn_df <- searchbycommonnamebeginswith(searchterm, locally=locally, returnindex=TRUE) } else
					if(searchtype == "comname") { tsn_df <- searchbycommonname(searchterm, locally=locally, returnindex=TRUE) } else
						if(searchtype == "comnameend") { tsn_df <- searchbycommonnameendswith(searchterm, locally=locally, returnindex=TRUE) } else
							stop("searchtype not valid!")
		
		dframes_split <- split(tsn_df, f=tsn_df$querystring)
		
		fun <- function(x, verbose)
		{
			if(verbose)
				cat("\nRetrieving data for species '", x, "'\n")
			
			dframe <- dframes_split[[x]]
			
			# should return NA if spec not found
			if (nrow(dframe) == 0)
				tsn <- NA
			# take the one tsn from data.frame
			if (nrow(dframe) == 1)
				tsn <- dframe$tsn
			# check for direct match
			if (nrow(dframe) > 1){
				direct <- match(tolower(x), tolower(dframe$combinedName))
				if(!is.na(direct))
					tsn <- dframe$tsn[direct]
			} else {
				direct <- NA
			}
			# user prompt
			if (nrow(dframe) > 1 & is.na(direct)){
				# sort alphabetically
				dframe <- dframe[order(dframe$combinedname), ]
				rownames(dframe) <- 1:nrow(dframe)
				
				# prompt
				cat("\n\n")
				print(dframe)
				cat("\nMore than one TSN found for species '", x, "'!\n
          Enter rownumber of species (other inputs will return 'NA'):\n") # prompt
				take <- scan(n = 1, quiet = TRUE, what = 'raw')
				
				if(length(take) == 0)
					take <- 'notake'
				if(take %in% seq_len(nrow(dframe))){
					take <- as.numeric(take)
					cat("Input accepted, took species '", as.character(dframe$combinedname[take]), "'.\n")
					tsn <-  dframe$tsn[take]
				} else {
					tsn <- NA
					cat("\nReturned 'NA'!\n\n")
				}
			}
			return(as.character(tsn))
		}
		out <- laply(searchterm, fun, verbose)
		if(nchar(out)==0){out <- "notsn"} else {NULL}
		class(out) <- "tsn"
		return(out)
	} else
	{
		fun <- function(x, verbose)
		{
			if(verbose)
				cat("\nRetrieving data for species '", x, "'\n")
			
			if(searchtype == "sciname"){ tsn_df <- searchbyscientificname(x) } else
				if(searchtype == "anymatch") { tsn_df <- searchforanymatch(x) } else
					if(searchtype == "comnamebeg") { tsn_df <- searchbycommonnamebeginswith(x) } else
						if(searchtype == "comname") { tsn_df <- searchbycommonname(x) } else
							if(searchtype == "comnameend") { tsn_df <- searchbycommonnameendswith(x) } else
								stop("searchtype not valid!")
			
			# should return NA if spec not found
			if (nrow(tsn_df) == 0)
				tsn <- NA
			# take the one tsn from data.frame
			if (nrow(tsn_df) == 1)
				tsn <- tsn_df$tsn
			# check for direct match
			if (nrow(tsn_df) > 1){
				direct <- match(tolower(x), tolower(tsn_df$combinedname))
				if(!is.na(direct))
					tsn <- tsn_df$tsn[direct]
			} else {
				direct <- NA
			}
			# user prompt
			if (nrow(tsn_df) > 1 & is.na(direct)){
				# sort alphabetically
				tsn_df <- tsn_df[order(tsn_df$combinedname), ]
				rownames(tsn_df) <- 1:nrow(tsn_df)
				
				# prompt
				cat("\n\n")
				print(tsn_df)
				cat("\nMore than one TSN found for species '", x, "'!\n
          Enter rownumber of species (other inputs will return 'NA'):\n") # prompt
				take <- scan(n = 1, quiet = TRUE, what = 'raw')
				
				if(length(take) == 0)
					take <- 'notake'
				if(take %in% seq_len(nrow(tsn_df))){
					take <- as.numeric(take)
					cat("Input accepted, took species '", as.character(tsn_df$combinedname[take]), "'.\n")
					tsn <-  tsn_df$tsn[take]
				} else {
					tsn <- NA
					cat("\nReturned 'NA'!\n\n")
				}
			}
			return(as.character(tsn))
		}
		out <- laply(searchterm, fun, verbose)
		if(nchar(out)==0){out <- "notsn"} else {NULL}
		class(out) <- "tsn"
		return(out)
	}
}
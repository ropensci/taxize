#' Search the Phylotastic Taxonomic Name Resolution Service.
#'
#' Match taxonomic names using the Taxonomic Name Resolution Service (TNRS). 
#'  Returns score of the matched name, and whether it was accepted or not.
#'  
#' @import RCurl XML plyr stringr RJSONIO
#' @param query Quoted taxonomic names to search in a vector (character).
#' @param source_ Specify the source you want to match names against. Defaults 
#' 		to just retrieve data from all sources. Options: NCBI, iPlant_TNRS, or MSW3.
#' @param getpost Use GET or POST method to send the query. If you have more than 
#' 		say 50 species or so in your query, you should probably use POST.
#' @param sleep Numer of seconds by which to pause between calls. Defaults to 0 
#' 		seconds. Use when doing many calls in a for loop ar lapply type call.
#' @param splitby Number by which to split species list for querying the TNRS.
#' @return data.frame of results from TNRS plus the name submitted.
#' @details If there is no match in the Taxosaurus database, nothing is returned, so you
#' 		will not get anything back for non matches. 
#' @examples \dontrun{
#' # Default, uses GET curl method
#' mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica", "Humbert humbert")
#' tnrs(query = mynames, source_ = "NCBI")
#' 
#' # Specifying the source to match against
#' mynames <- c("Helianthus annuus", "Poa annua")
#' tnrs(query = mynames, source_ = "iPlant_TNRS")
#' 
#' # You can specify multiple sources, by comma-separating them
#' mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica", "Humbert humbert")
#' tnrs(query = mynames, source_ = "NCBI,MSW3")
#' 
#' # Using POST method, especially useful when you have a lot of species
#' mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica", "Humbert humbert", 
#' 		"Helianthus annuus", "Pinus contorta", "Poa annua", "Abies magnifica", 
#'		"Rosa california", "Festuca arundinace", "Mimulus bicolor", "Sorbus occidentalis",
#'		"Madia sativa", "Thymopsis thymodes", "Bartlettia scaposa")
#' tnrs(mynames, getpost="POST", source_ = "NCBI")
#' }
#' @export
tnrs <- function(query = NA, source_ = NULL, getpost = "GET", sleep = 0, splitby = NULL)
{
	url = "http://taxosaurus.org/submit"
  
	mainfunc <- function(x){
		Sys.sleep(time = sleep) # set amount of sleep to pause by
		if(getpost=="GET"){
			if(!any(is.na(x))){
				query2 <- paste(str_replace_all(x, ' ', '+'), collapse='%0A')
# 				tt <- getURL(paste0(url, "?query=", query2))
				args <- compact(list(query = query2, source = source_))
				tt <- getForm(url, .params=args)
			} else
			{
				stop("some problems...")
			}
		} else
		{
			splist <- paste(x, collapse="\n")
			args <- compact(list(query = splist, source = source_))
			tt <- postForm(url, .params=args)
# 				postForm(url, query=splist, source = source_)
		}
		message <- fromJSON(tt)["message"]
		retrieve <- str_replace_all(str_extract(message, "http.+"), "\\.$", "")
		
		message(paste("Calling ", retrieve, sep=""))
		
		iter <- 0
		output <- list()
		timeout <- "wait"
		while(timeout == "wait"){
			iter <- iter + 1
			temp <- fromJSON(getURL(retrieve))
			if(grepl("is still being processed", temp["message"])==TRUE){timeout <- "wait"} else {
				output[[iter]] <- temp
				timeout <- "done"
			}
		}
		out <- compact(output)[[1]]
		
		parseres <- function(w){ # function to parse results
			matches <- w$matches
			foome <- function(z) { 
				z[sapply(z, length)==0] <- "none" 
				data.frame(z)
			}
			matches2 <- ldply(matches, foome)
			df <- data.frame(submittedName=w$submittedName, matches2)
			df$score <- round(as.numeric(as.character(df$score)), 2)
			df
		}
		
		# Parse results into data.frame
		df <- ldply(out$names, parseres)
		order_ <- unlist(sapply(x, function(y) grep(y, df$submittedName)))
# 		order_ <- do.call(c, sapply(x, function(y) grep(y, df$submittedName)))
		df2 <- df[order_,]
		
		return(df2)
	}
	mainfunc_safe <- plyr::failwith(NULL, mainfunc)
	
	if(is.null(splitby)){
		mainfunc_safe(query)
	} else
	{
		## Define function to split up your species list into useable chuncks
		slice <- function(input, by = 2) {
			starts <- seq(1, length(input), by)
			tt <- lapply(starts, function(y) input[y:(y + (by - 1))])
			llply(tt, function(x) x[!is.na(x)])
		}
		species_split <- slice(query, by = splitby)	
		
		out <- llply(species_split, function(x) mainfunc_safe(x))
		return(ldply(out))
	}
}
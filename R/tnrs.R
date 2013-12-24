#' Search the Phylotastic Taxonomic Name Resolution Service.
#'
#' Match taxonomic names using the Taxonomic Name Resolution Service (TNRS). 
#'  Returns score of the matched name, and whether it was accepted or not.
#'  
#' @import RCurl XML plyr stringr RJSONIO data.table
#' @param query Quoted taxonomic names to search in a vector (character).
#' @param source_ Specify the source you want to match names against. Defaults 
#' 		to just retrieve data from all sources. Options: NCBI, iPlant_TNRS, 
#'   	or MSW3.
#' @param code Nomenclatural code. One of: ICZN (zoological), ICN (algae, fungi, 
#'    and plants), ICNB (bacteria), ICBN (botanical), ICNCP (cultivated plants), 
#'    ICTV (viruses)
#' @param getpost Use GET or POST method to send the query. If you have more 
#'    than say 50 species or so in your query, you should probably use POST.
#' @param sleep Numer of seconds by which to pause between calls. Defaults to 0 
#' 		seconds. Use when doing many calls in a for loop ar lapply type call.
#' @param splitby Number by which to split species list for querying the TNRS.
#' @param verbose Verbosity or not (default TRUE)
#' @return data.frame of results from TNRS plus the name submitted.
#' @details If there is no match in the Taxosaurus database, nothing is 
#'    returned, so youwill not get anything back for non matches. 
#' @examples \dontrun{
#' # Default, uses GET curl method, you can't specify any other parameters when 
#' using GET
#' mynames <- c("Panthera tigris", "Neotamias minimus", "Magnifera indica")
#' tnrs(query = mynames, source_="NCBI")
#' 
#' # Specifying the source to match against
#' mynames <- c("Helianthus annuus", "Poa annua")
#' tnrs(query = mynames, source_ = "iPlant_TNRS")
#' 
#' # Specifying the nomenclatural code to match against
#' mynames <- c("Helianthus annuus", "Poa annua")
#' tnrs(query = mynames, code = "ICBN")
#' 
#' # You can specify multiple sources, by comma-separating them
#' mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica", 
#' "Humbert humbert")
#' tnrs(query = mynames, source_ = "NCBI,MSW3")
#' 
#' # Using POST method, especially useful when you have a lot of species
#' mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica", 
#'    "Humbert humbert", "Helianthus annuus", "Pinus contorta", "Poa annua", 
#'    "Abies magnifica", "Rosa california", "Festuca arundinace", 
#'    "Mimulus bicolor", "Sorbus occidentalis","Madia sativa", "Thymopsis 
#'    thymodes", "Bartlettia scaposa")
#' tnrs(mynames, getpost="POST", source_ = "NCBI")
#' 
#' # And even more names
mynames <- names_list(rank="species", size=75)
tnrs(query=mynames, getpost="POST", source_ = "NCBI")
## Or use splitby
tnrs(mynames, getpost="POST", source_ = "NCBI", splitby=50)
#' }
#' @export
tnrs <- function(query = NA, source_ = NULL, code = NULL, getpost = "POST", 
                 sleep = 0, splitby = 30, verbose=TRUE)
{
	url = "http://taxosaurus.org/submit"
  
	mainfunc <- function(x){
		Sys.sleep(time = sleep) # set amount of sleep to pause by
		if(getpost=="GET"){
			if(!any(is.na(x))){
				query2 <- paste(str_replace_all(x, ' ', '+'), collapse='%0A')
				args <- compact(list(query = query2))
				tt <- getForm(url, .params=args)
			} else
			{
				stop("some problems...")
			}
		} else
		{
			splist <- paste(x, collapse="\n")
			args <- compact(list(query = splist, source = source_, code = code))
# 			tt <- postForm(url, .params=args, curl=getCurlHandle(), style="POST")
# 			tt <- postForm(url, source="NCBI", file=fileUpload(loc, contentType="text/plain"), style="HTTPPOST", .checkParams=FALSE)
# 			tt <- postForm(url, source="NCBI", query=splist, style="POST", .checkParams=FALSE)
# 			tt <- POST(url, body=toJSON(list(file = upload_file(path=loc))))
			body = toJSON(list(query = splist))
			(tt <- POST(url, upload_file(loc)))
		}
		message <- fromJSON(tt)[["message"]]
		retrieve <- str_replace_all(str_extract(message, "http.+"), "\\.$", "")
		
		mssg(verbose, paste("Calling ", retrieve, sep=""))
		
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
    length(out$names)
		
		parseres <- function(w){ # function to parse results
			matches <- w$matches
			foome <- function(z) { 
				z[sapply(z, length)==0] <- "none" 
				data.frame(z)
			}
			matches2 <- data.frame(rbindlist(lapply(matches, foome)))
			df <- data.frame(submittedName=w$submittedName, matches2)
			df$score <- round(as.numeric(as.character(df$score)), 2)
			df
		}
		
		# Parse results into data.frame
    df <- data.frame(rbindlist(lapply(out$names, parseres)))
    f <- function(x) str_replace_all(x, pattern="\\+", replacement=" ")
    df <- colwise(f)(df)
		order_ <- unlist(sapply(x, function(y) grep(y, df$submittedName)))
		df2 <- df[order_,]
		
		return(df2)
	}
	mainfunc_safe <- plyr::failwith(NULL, mainfunc)
	
	if(getpost == "GET" && length(query) > 75 | length(query) > 30 && getpost == "POST"){
	  ## Define function to split up the species list into more manageable chunks
	  slice <- function(input, by = 2) {
	    starts <- seq(1, length(input), by)
	    tt <- lapply(starts, function(y) input[y:(y + (by - 1))])
	    lapply(tt, function(x) x[!is.na(x)])
	  }
	  species_split <- slice(query, by = splitby)	
	  
	  out <- lapply(species_split, function(x) mainfunc_safe(x))
	  tmp <- data.frame(rbindlist(out))
	  names(tmp) <- tolower(names(tmp))
	  return( tmp )
	} else
	{
	  tmp <- mainfunc_safe(query)
	  names(tmp) <- tolower(names(tmp))
	  return( tmp )
	}
}
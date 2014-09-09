#' Search the Phylotastic Taxonomic Name Resolution Service.
#'
#' Match taxonomic names using the Taxonomic Name Resolution Service (TNRS).
#' Returns score of the matched name, and whether it was accepted or not.
#'
#' @import RCurl XML plyr stringr jsonlite data.table
#' @export
#'
#' @param query Quoted taxonomic names to search in a vector (character).
#' @param source Specify the source you want to match names against. Defaults
#' 		to just retrieve data from all sources. Options: NCBI, iPlant_TNRS,
#'   	or MSW3. Only available when using getpost="POST".
#' @param code Nomenclatural code. One of: ICZN (zoological), ICN (algae, fungi,
#'    and plants), ICNB (bacteria), ICBN (botanical), ICNCP (cultivated plants),
#'    ICTV (viruses). Only available when using getpost="POST".
#' @param getpost Use GET or POST method to send the query. If you have more
#'    than say 50 species or so in your query, you should probably use POST.
#'    IMPORTANT!!!!! ->
#'        POST is the only option for this parameter if you want to
#'        use source or code parameters.
#' @param sleep Numer of seconds by which to pause between calls. Defaults to 0
#' 		seconds. Use when doing many calls in a for loop ar lapply type call.
#' @param splitby Number by which to split species list for querying the TNRS.
#' @param verbose Verbosity or not (default TRUE)
#' @param callopts Curl debugging options to pass in httr::GET or POST
#' @return data.frame of results from TNRS plus the name submitted.
#' @details If there is no match in the Taxosaurus database, nothing is
#'    returned, so youwill not get anything back for non matches.
#' @examples \donttest{
#' # Default, uses GET curl method, you can't specify any other parameters when
#' using GET
#' mynames <- c("Panthera tigris", "Neotamias minimus", "Magnifera indica")
#' tnrs(query = mynames)
#'
#' # Specifying the source to match against
#' mynames <- c("Helianthus annuus", "Poa annua")
#' tnrs(query = mynames, source = "iPlant_TNRS")
#'
#' # Specifying the nomenclatural code to match against
#' mynames <- c("Helianthus annuus", "Poa annua")
#' tnrs(query = mynames, code = "ICBN")
#'
#' # You can specify multiple sources, by comma-separating them
#' mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica",
#' "Humbert humbert")
#' tnrs(query = mynames, source = "NCBI,MSW3")
#'
#' # Using POST method, especially useful when you have a lot of species
#' mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica",
#'    "Humbert humbert", "Helianthus annuus", "Pinus contorta", "Poa annua",
#'    "Abies magnifica", "Rosa california", "Festuca arundinace",
#'    "Mimulus bicolor", "Sorbus occidentalis","Madia sativa", "Thymopsis thymodes",
#'    "Bartlettia scaposa")
#' tnrs(mynames, source = "NCBI")
#'
#' # And even more names
#' mynames <- names_list(rank="species", size=75)
#' tnrs(query=mynames, source = "NCBI")
#' ## Or use splitby
#' tnrs(mynames, source = "NCBI", splitby=50)
#' }

tnrs <- function(query = NA, source = NULL, code = NULL, getpost = "POST",
                 sleep = 0, splitby = 30, verbose = TRUE, callopts = list())
{
	mainfunc <- function(x){
	  url = "http://taxosaurus.org/submit"

    Sys.sleep(time = sleep) # set amount of sleep to pause by

		if(getpost=="GET"){
			if(!any(is.na(x))){
				query2 <- paste(str_replace_all(x, ' ', '+'), collapse='%0A')
				args <- compact(list(query = query2))
				out <- GET(url, query=args, config = callopts)
				error_handle(out)
				retrieve <- out$url
			} else
			{
				stop("some problems...")
			}
		} else
		{
      loc <- tempfile(fileext=".txt")
      write.table(data.frame(x), file=loc, col.names=FALSE, row.names=FALSE)
      args <- compact(list(file = upload_file(loc), source = source, code = code))
			out <- POST(url, body = args, config=compact(c(followlocation = 0L, callopts)))
			error_handle(out)
      tt <- content(out, as="text")
			message <- jsonlite::fromJSON(tt, FALSE)[["message"]]
			retrieve <- str_replace_all(str_extract(message, "http.+"), "\\.$", "")
		}

		mssg(verbose, sprintf("Calling %s", retrieve))

		iter <- 0
		output <- list()
		timeout <- "wait"
		while(timeout == "wait"){
			iter <- iter + 1
      ss <- GET(retrieve, callopts)
			error_handle(ss, TRUE)
      temp <- jsonlite::fromJSON(content(ss, as="text"), FALSE)
			if(grepl("is still being processed", temp["message"])==TRUE){timeout <- "wait"} else {
				output[[iter]] <- temp
				timeout <- "done"
			}
		}
		out <- compact(output)[[1]]

		# Parse results into data.frame
    df <- data.frame(rbindlist(lapply(out$names, parseres)))
    f <- function(x) str_replace_all(x, pattern="\\+", replacement=" ")
    df2 <- colwise(f)(df)

    # replace quotes
    df2 <- data.frame(apply(df2, c(1,2), function(x){
      if(grepl('\"', x)) gsub('\"', "", x) else x
    }), stringsAsFactors=FALSE)

		return(df2)
	}
# 	mainfunc_safe <- plyr::failwith(NULL, mainfunc)

  if(length(query) < 1 || is.na(query)) stop("Please supply at least one name", call. = FALSE)

	if(getpost == "GET" && length(query) > 75 | length(query) > 30 && getpost == "POST"){
	  species_split <- slice(query, by = splitby)

	  out <- lapply(species_split, function(x) mainfunc(x))
	  tmp <- data.frame(rbindlist(out))
	  names(tmp) <- tolower(names(tmp))
	  return( tmp )
	} else
	{
	  tmp <- mainfunc(query)
	  names(tmp) <- tolower(names(tmp))
	  return( tmp )
	}
}

# Function to parse results
parseres <- function(w){
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

# Function to split up the species list into more manageable chunks
slice <- function(input, by = 2) {
  starts <- seq(1, length(input), by)
  tt <- lapply(starts, function(y) input[y:(y + (by - 1))])
  lapply(tt, function(x) x[!is.na(x)])
}

error_handle <- function(x, checkcontent=FALSE){
  tocheck <- x$status_code
  if(checkcontent){
    if( "metadata" %in% names(jsonlite::fromJSON(content(x), FALSE)) ){
      codes <- sapply(jsonlite::fromJSON(content(x), FALSE)$metadata$sources, "[[", "status")
      codes <- as.numeric(str_extract(codes, "[0-9]+"))
      tocheck <- c(tocheck, codes)
    }
  }
  if(any(tocheck >= 400)){
    it <- tocheck[ tocheck >= 400 ]
    mssg <- switch(as.character(it),
       '400' = "Bad Request. The request could not be understood by the server due to malformed syntax. The client SHOULD NOT repeat the request without modifications.",
       '401' = 'Unauthorized',
       '403' = 'Forbidden',
       '404' = 'Not Found',
       '500' = 'Internal Server Error. The server encountered an unexpected condition which prevented it from fulfilling the request.',
       '501' = 'Not Implemented',
       '502' = 'Bad Gateway',
       '503' = 'Service Unavailable',
       '504' = 'Gateway Timeout'
    )
    stop(sprintf("HTTP status %s - %s", it, mssg), call. = FALSE)
  }
}

#' Function to get API key. 
#' 
#' Checks first to get key from your .Rprofile file for an API key with the name
#' 		'tropicoskey'. If it is not found, the default key is used. 
#' 
#' @param x An API key, defaults to NULL.
#' @examples \dontrun{
#' getkey()
#' } 
#' @export
getkey <- function(x = NULL) {	
	
	if(is.null(x)){
		
		key <- getOption("tropicoskey")
		
		if(is.null(key)){
			key <- "00ca3d6a-cbcc-4924-b882-c26b16d54446"
			message("Using default key: Please go get your own API key at http://services.tropicos.org/help?requestkey")
		} else 
			
			if(class(key)=="character"){key <- key} else 
				{ stop("check your key input - it should be a character string") }
		
	} else 
		
		{ key <- x }
	key
}
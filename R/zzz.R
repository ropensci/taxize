#' Function to get API key. 
#' 
#' Checks first to get key from your .Rprofile file for an API key with the 
#' 		name 'tropicoskey'. If it is not found, the default key is used. 
#' 
#' @param x An API key, defaults to NULL.
#' @param service The API data provider, used to match to default guest key.
#' @examples \dontrun{
#' getkey(service="tropicos")
#' getkey(service="eol")
#' } 
#' @keywords internal
#' @export
getkey <- function(x = NULL, service) {	
	if(is.null(x)){
	  keynames <- c("tropicosApiKey","eolApiKey","ubioApiKey","pmApiKey")
		service <- match.arg(service, keynames, several.ok=F)
		key <- getOption(service)
		if(is.null(key)){
			keys <- c("00ca3d6a-cbcc-4924-b882-c26b16d54446",
								"44f1a53227f1c0b6238a997fcfe7513415f948d2",
								"750bc6b8a550f2b9af1e8aaa34651b4c1111862a",
								"530763730")
			names(keys) <- keynames
			key <- keys[[service]]
			urls <- c("http://services.tropicos.org/help?requestkey",
								"http://eol.org/users/register",
								"http://www.ubio.org/index.php?pagename=form",
								"http://www.plantminer.com/")
			names(urls) <- keynames
			message(paste("Using default key: Please get your own API key at ", 
										urls[service], sep=""))
		} else 
			if(class(key)=="character"){key <- key} else 
				{ stop("check your key input - it should be a character string") }
	} else 
		{ key <- x }
	key
}

#' Replacement function for ldply that should be faster in all cases. 
#' 
#' @import plyr
#' @param x A list.
#' @param convertvec Convert a vector to a data.frame before rbind is called.
#' @export
#' @keywords internal
taxize_ldfast <- function(x, convertvec=FALSE){
  convert2df <- function(x){
    if(!inherits(x, "data.frame")) 
      data.frame(rbind(x))
    else
      x
  }
  
  if(convertvec)
    do.call(rbind.fill, lapply(x, convert2df))
  else
    do.call(rbind.fill, x)
}

mssg <- function(v, ...) if(v) message(...)

taxize_compact <- function (l) Filter(Negate(is.null), l)


#' Function to allow user to set timeout on a function call.
#' 
#' This is separate from the curl timeout.ms parameter
#'
#' @importFrom R.utils evalWithTimeout
#' @param test_fn The function call to test
#' @param tlimit = 120 A timeout in seconds
#' @param defaultvalue Default value
#' @return If in a loop, passes on to next thing, or if not, then gives back message
#' @examples \dontrun{
#' library("taxize")
#' try_with_timeout(eol_search('Salix'))
#' try_with_timeout(eol_search('Salix'), tlimit=10)
#' }

try_with_timeout <- function(test_fn, tlimit = 30, defaultvalue = "TimedOut") {
  results <- tryCatch(expr = evalWithTimeout(test_fn, timeout = tlimit), 
                      TimeoutException = function(ex) defaultvalue)
  if(is(results, "TimedOut")){
    return( defaultvalue )
  } else { results } 
}
#' Capitalize the first letter of a character string.
#' 
#' @param s A character string
#' @param strict Should the algorithm be strict about capitalizing. Defaults to FALSE.
#' @param onlyfirst Capitalize only first word, lowercase all others. Useful for 
#' 		taxonomic names.
#' @examples 
#' capwords(c("using AIC for model selection"))
#' capwords(c("using AIC for model selection"), strict=TRUE)
#' @export
capwords <- function(s, strict = FALSE, onlyfirst = FALSE) {
	cap <- function(s) paste(toupper(substring(s,1,1)),
		{s <- substring(s,2); if(strict) tolower(s) else s}, sep = "", collapse = " " )
	if(!onlyfirst){
		sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
	} else
		{
			sapply(s, function(x) 
				paste(toupper(substring(x,1,1)), 
							tolower(substring(x,2)), 
							sep="", collapse=" "), USE.NAMES=F)
		}
}
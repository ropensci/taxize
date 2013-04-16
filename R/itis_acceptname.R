#' Retrieve accepted TSN (with accepted name).
#' 
#' @param searchtsn Quoted TSN for a taxonomic group (character).
#' @param locally If TRUE, queries are run locally in sqlite3; if FALSE (the default), 
#' 		queries are run against the ITIS web API. locally=TRUE should be faster in almost all cases.
#' @details You can print informative messages by setting supmess=FALSE.
#' @return Names or TSNs of all downstream taxa.
#' @export
#' @examples \dontrun{
#' itis_acceptname(searchtsn='208527')  # TSN accepted - good name
#' itis_acceptname(searchtsn='504239')  # TSN not accepted - input TSN is old name
#' 
#' itis_acceptname(searchtsn='208527', locally=TRUE)  # searching locally
#' }
itis_acceptname <- function(searchtsn = NA, locally = FALSE)
{
	tt <- taxize:::getacceptednamesfromtsn(searchtsn, locally = locally)
	if(length(tt)==1){tt} else {data.frame(tt)}
}
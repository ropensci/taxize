#' Initiate connection to sqlite3 database
#' @param path Path to the sqlite database
#' @examples \dontrun{
#' taxize:::sqlite_init()
#' }
sqlite_init <- function(path){
	require(RSQLite)
	path = "/Users/scottmac2/github/ropensci/taxize_/inst/sql/itis2.sqlite"
	m <- dbDriver("SQLite")
	con <- dbConnect(m, dbname = path)
	return( con )
}
#' Initiate connection to sqlite3 database
#' @param path Path to the sqlite database
#' @examples \dontrun{
#' taxize:::sqlite_init()
#' }
sqlite_init <- function(path = "/Users/scottmac2/github/ropensci/taxize_/inst/sql/itis2.sqlite")
{
# 	if(!inherits(conn, what="SQLiteConnection"))
	m <- dbDriver("SQLite")
	conn <- dbConnect(m, dbname = path)
	return( conn )
}
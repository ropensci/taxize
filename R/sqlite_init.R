#' Initiate connection to sqlite3 database
#' 
#' @import RSQLite DBI
#' @param path Path to the sqlite database
#' @examples \dontrun{
#' taxize:::sqlite_init()
#' }
sqlite_init <- function(path = "~/github/ropensci/sql/itis2.sqlite")
{
# 	if(!inherits(conn, what="SQLiteConnection"))
	m <- dbDriver("SQLite")
	conn <- dbConnect(m, dbname = path)
  options(conn = conn)
	return( conn )
}
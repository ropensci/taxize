#' Get The Plant List csv files. 
#' 
#' The Plant List \link{http://www.theplantlist.org/}. Note that there is now a 
#' 		package on CRAN (taxonstand - \link{http://cran.r-project.org/web/packages/Taxonstand/}) 
#' 		that uses only theplantlist.org to search plant names.
#' 
#' @import RCurl plyr XML
#' @param dir_ Directory to write csv files to.
#' @param family If you want just one, or >1 family, but not all, list them in a vector.
#' @details Throws a warning if you already have a directory of the one provided, but still works.
#' @return Returns nothing to console, except a message and progress bar. Writes csv files to dir_.
#' @examples \donttest{
#' tpl_get(dir_ = "~/foo") # writes to your home directory, change to where you want
#' tpl_get(dir_ = "~/foo2", family = c("Platanaceae","Winteraceae")) # just a few families
#' @export
tpl_get <- function(dir_, family = NULL)
{
	temp <- getURL("http://www.theplantlist.org/browse/A/") # get the html source for the families listing page
	families <- xpathSApply(htmlParse(temp), "//i[@class]", xmlValue, "class") # get the family names
	families <- families[!families %in% c("Angiosperms","angiosperms")] # remove "Angiosperms"

	# if family does not = NULL, get just those families
	if(!is.null(family)) {
		families <- families[families %in% family]
	} else 
		{
			families <- families
		}
	csvlinks <- sapply(families, function(x)  # makes csv URL's
		paste("http://www.theplantlist.org/browse/A/", x, "/", x, ".csv", sep=""),
		USE.NAMES=FALSE)
	readwrite <- function(x, dir_){ # function to read, then write the csv file to directory
		write.csv(
			read.csv(x), 
				paste(dir_, "/", strsplit(x[[1]], "/")[[1]][[6]], ".csv", sep=""), 
					row.names=FALSE)
	}
	# reads each csv file and writes to dir w/o saving in workspace
	message(paste("Reading and writing csv files to ", dir_, "...", sep=""))
	
	dir.create(dir_) # create directory if it doesn't exist, will not overwrite if the dir_ already exists

	l_ply(csvlinks, readwrite, dir_ = dir_, .progress="text") # do readwrite in a loop for each family
	message("...el fin") # Done!
}
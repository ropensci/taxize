#' Check invasive species status for a set of species from GISD database
#' 
#' @import plyr XML RCurl
#' 
#' @param x character; a vector of scientific species names in the form of 
#'    c("Genus species").
#' @param simplify logical; returns a data.frame with the species name and the 
#'    values "Invasive", "Not in GISD". I recomend to check first the not 
#'    simplified version (default), which contains raw information about the 
#'    level of invasiveness.
#' 
#' @return A data.frame with species names and invasiveness.
#' 
#' @description This function check which species (both plants and animals) are 
#' considered "invaders" somewhere in the world.  
#' 
#' For that end, it checks GISD (http://www.issg.org/database/welcome/) and 
#' returns a value, either "Not in GISD" or the brief description presented in 
#' GISD. 
#' 
#' Note that the webpage contains more information. Also note that the function 
#' won't tell you if it's exotic in your area, a lot of exotic species are not 
#' considered invaders (yet). 
#' 
#' As expected, the function is as good as the database is, which I find quite 
#' reliable and well maintained. 
#' The database is also able to recognize a lot (but not all) of the species 
#' synonyms.
#' 
#' @author Ignasi Bartomeus \email{nacho.bartomeus@@gmail.com}
#' @examples \dontrun{
#' sp <- c("Carpobrotus edulis", "Rosmarinus officinalis")
#' ## first species is invasive, second one is not. 
#' gisd_isinvasive(sp)
#' gisd_isinvasive(sp, simplify = TRUE)
#' }
#' 
#' @export
gisd_isinvasive <- function(x, simplify = FALSE){ 
	# reformat sp list
	species <- gsub(" ", "+", x)
	# create urls to parse
	urls <- paste("http://www.issg.org/database/species/search.asp?sts=sss&st=sss&fr=1&x=13&y=9&sn=",
								species, "&rn=&hci=-1&ei=-1&lang=EN", sep = "")
	# create a data.frame to store the Output
	out <- data.frame(species = x, status = c(1:length(urls)))
	#loop through all species
	for(i in 1:length(urls)){
		#Parse url and extract table
		doc <- htmlTreeParse(urls[i], useInternalNodes = TRUE)
		tables <- getNodeSet(doc, "//table")
		t <- readHTMLTable(tables[[4]])
		tt <- as.matrix(t)
		if(length(grep("No invasive species currently recorded", tt, value = TRUE)) > 0){
			out[i, 2] <- "Not in GISD"
		}
		else{
			if(simplify == FALSE){
        out[i, 2] <- tt[12, 1]
			} else {
        out[i, 2] <- "Invasive"
			}
		}
		message(paste("Checking species", i))	
	}
	message("Done")
	return(out)
}
is.invasive <- function(sp, simplified.df = FALSE){ 
	require(plyr)
	require(XML)
	require(RCurl)
	#reformat sp list
	species <- gsub(" ","+",sp)
	#create urls to parse
	urls <- paste("http://www.issg.org/database/species/search.asp?sts=sss&st=sss&fr=1&x=13&y=9&sn=",
								species,"&rn=&hci=-1&ei=-1&lang=EN", sep = "")
	#create a data.frame to store the Output
	Out <- data.frame(species = sp, status = c(1:length(urls)))
	#loop through all species
	for(i in 1:length(urls)){
		#Parse url and extract table
		doc <- htmlTreeParse(urls[i], useInternalNodes = TRUE)
		tables <- getNodeSet(doc, "//table")
		t <- readHTMLTable(tables[[4]])
		tt <- as.matrix(t)
		if(length(grep("No invasive species currently recorded",tt, value = TRUE)) > 0){
			Out[i,2] <- "Not in GISD"
		}
		else{
			if(simplified.df == FALSE){Out[i,2] <- tt[12,1]}
			else{Out[i,2] <- "Invasive"}
		}
		message(paste("Checking species", i+1))	
	}
	message("Done")
	Out
}
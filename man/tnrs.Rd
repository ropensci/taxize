\name{tnrs}
\alias{tnrs}
\title{Search the Phylotastic Taxonomic Name Resolution Service.}
\usage{
  tnrs(query = NA, getpost = "GET", sleep = 0,
    splitby = NULL)
}
\arguments{
  \item{query}{Quoted taxonomic names to search in a vector
  (character).}

  \item{getpost}{Use GET or POST method to send the query.
  If you have more than say 50 species or so in your query,
  you should probably use POST.}

  \item{sleep}{Numer of seconds by which to pause between
  calls. Defaults to 0 seconds. Use when doing many calls
  in a for loop ar lapply type call.}

  \item{splitby}{Number by which to split species list for
  querying the TNRS.}
}
\value{
  data.frame of results from TNRS plus the name submitted.
}
\description{
  Match taxonomic names using the Taxonomic Name Resolution
  Service (TNRS).  Returns score of the matched name, and
  whether it was accepted or not.
}
\details{
  If there is no match in the Taxosaurus database, nothing
  is returned, so you will not get anything back for non
  matches.
}
\examples{
\dontrun{
# Default, uses GET curl method
mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica", "Humbert humbert")
tnrs(query = mynames, splitby=2, sleep=1)

# Using POST method, especially useful when you have a lot of species
mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica", "Humbert humbert",
		"Helianthus annuus", "Pinus contorta", "Poa annua", "Abies magnifica",
		"Rosa california", "Festuca arundinace", "Mimulus bicolor", "Sorbus occidentalis",
		"Madia sativa", "Thymopsis thymodes", "Bartlettia scaposa")
tnrs(mynames, getpost="POST")
}
}


\name{tnrs}
\alias{tnrs}
\title{Search the Phylotastic Taxonomic Name Resolution Service.}
\usage{
  tnrs(query = NA, shortenurls = FALSE, getpost = "GET",
    sleep = 1, url = "http://taxosaurus.org/submit")
}
\arguments{
  \item{query}{Quoted taxonomic names to search in a vector
  (character).}

  \item{shortenurls}{Use bit.ly API to shorten URLs for
  matched species (default = FALSE).}

  \item{getpost}{Use GET or POST method to send the query.
  If you have more than say 50 species or so in your query,
  you should probably use POST.}

  \item{sleep}{Numer of seconds by which to pause by before
  retrieving the result. Defaults to 1 second. Set sleep
  for longer periods when queries contain more species.}

  \item{url}{The iPlant API url for the function (should be
  left to default).}
}
\value{
  data.frame of results from TNRS plus the name submitted.
}
\description{
  Match taxonomic names using the Taxonomic Name Resolution
  Service (TNRS).  Returns score of the matched name, and
  whether it was accepted or not.
}
\examples{
\dontrun{
# Default, uses GET curl method
mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica", "Humbert humbert")
tnrs(query = mynames)

# Using POST method, especially useful when you have a lot of species
mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica", "Humbert humbert",
		"Helianthus annuus", "Pinus contorta", "Poa annua", "Abies magnifica",
		"Rosa california", "Festuca arundinace", "Mimulus bicolor", "Sorbus occidentalis",
		"Madia sativa", "Thymopsis thymodes", "Bartlettia scaposa")
tnrs(mynames, getpost="POST")
}
}


\name{tnrastic}
\alias{tnrastic}
\title{Search the Phylotastic Taxonomic Name Resolution Service}
\usage{
  tnrastic(query = NA, output = NA, sleep = 1,
    url = "http://api.phylotastic.org/tnrs/submit", ...,
    curl = getCurlHandle())
}
\arguments{
  \item{query}{Quoted taxonomic names to search in a vector
  (character).}

  \item{output}{'all' for raw list output or 'names' for
  matched names and their match scores, plus plant family
  names (character).}

  \item{sleep}{Numer of seconds by which to pause by before
  retrieving the result. Defaults to 1 second. Set sleep
  for longer periods when queries contain more species.}

  \item{url}{The iPlant API url for the function (should be
  left to default).}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
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
mynames <- c("Crepis atrabarba", "Zygadenus venenosus")
tnrastic(query = mynames, output = 'names')
}
}


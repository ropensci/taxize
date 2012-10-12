\name{itis_getacceptname}
\alias{itis_getacceptname}
\title{Retrieve accepted TSN (with accepted name).}
\usage{
  itis_getacceptname(searchtsn = NA, supmess = TRUE,
    url = "http://www.itis.gov/ITISWebService/services/ITISService/getAcceptedNamesFromTSN?tsn=",
    curl = getCurlHandle())
}
\arguments{
  \item{searchtsn}{Quoted TSN for a taxonomic group
  (character).}

  \item{supmess}{Suppress informative messages
  (default=TRUE).}

  \item{url}{The ITIS API url for the function (should be
  left to default).}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\value{
  Names or TSNs of all downstream taxa.
}
\description{
  Retrieve accepted TSN (with accepted name).
}
\details{
  You can print informative messages by setting
  supmess=FALSE.
}
\examples{
\dontrun{
itis_getacceptname('208527')  # TSN accepted - good name
itis_getacceptname('504239')  # TSN not accepted - input TSN is old name
}
}


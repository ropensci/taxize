\name{gettsnfromlsid}
\alias{gettsnfromlsid}
\title{Gets the TSN corresponding to the LSID, or an empty result if there is no match.}
\usage{
  gettsnfromlsid(lsid = NA, ..., curl = getCurlHandle())
}
\arguments{
  \item{lsid}{lsid for a taxonomic group (numeric)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\description{
  Gets the TSN corresponding to the LSID, or an empty
  result if there is no match.
}
\examples{
\dontrun{
gettsnfromlsid(lsid = "urn:lsid:itis.gov:itis_tsn:28726")
gettsnfromlsid(lsid = "urn:lsid:itis.gov:itis_tsn:0")
}
}


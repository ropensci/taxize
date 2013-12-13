\name{getrecordfromlsid}
\alias{getrecordfromlsid}
\title{Gets the partial ITIS record for the TSN in the LSID, found by comparing the
 TSN in the search key to the TSN field. Returns an empty result set if
 there is no match or the TSN is invalid.}
\usage{
getrecordfromlsid(lsid = NA, ..., curl = getCurlHandle())
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
Gets the partial ITIS record for the TSN in the LSID, found
by comparing the TSN in the search key to the TSN field.
Returns an empty result set if there is no match or the TSN
is invalid.
}
\examples{
\dontrun{
getrecordfromlsid(lsid = "urn:lsid:itis.gov:itis_tsn:180543")
}
}
\keyword{internal}


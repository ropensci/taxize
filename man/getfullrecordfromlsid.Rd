\name{getfullrecordfromlsid}
\alias{getfullrecordfromlsid}
\title{Returns the full ITIS record for the TSN in the LSID, found by comparing the
     TSN in the search key to the TSN field. Returns an empty result set if there
     is no match or the TSN is invalid.}
\usage{
  getfullrecordfromlsid(lsid = NA, ...,
    curl = getCurlHandle())
}
\arguments{
  \item{lsid}{lsid for a taxonomic group (character)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\description{
  Returns the full ITIS record for the TSN in the LSID,
  found by comparing the TSN in the search key to the TSN
  field. Returns an empty result set if there is no match
  or the TSN is invalid.
}
\examples{
\dontrun{
getfullrecordfromlsid(lsid = "urn:lsid:itis.gov:itis_tsn:180543")
}
}


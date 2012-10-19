\name{itis}
\alias{itis}
\title{Use any function in ritis, searching by TSN or taxonomic name.}
\usage{
  itis(query,
    searchtype = c("anymatch", "sciname", "comnamebeg", "comname", "comnameend", "terms", "itistermscomname", "itistermssciname", "tsnsvernacular", "tsnfullhir", "tsnhirdown"),
    by_ = c("name", "tsn"), parselist = TRUE,
    base_url = "http://www.itis.gov/ITISWebService/services/ITISService/",
    curl = getCurlHandle())
}
\arguments{
  \item{query}{Any common or scientific name.}

  \item{searchtype}{One of 'sciname', 'anymatch',
  'comnamebeg', 'comname', 'comnameend', 'terms',
  'itistermscomname', 'itistermssciname', or
  'tsnsvernacular', 'tsnfullhir', 'tsnhirdown' .}

  \item{by_}{One of 'name' (any common or scientific name)
  or 'tsn' (taxonomic serial number).}

  \item{parselist}{Parse xml or not (defaults to TRUE).}

  \item{base_url}{The base URL for the ITIS API service,
  leave as is.}

  \item{curl}{Use to speed up calls when looping}
}
\value{
  xml with taxonomic information.
}
\description{
  Use any function in ritis, searching by TSN or taxonomic
  name.
}
\examples{
\dontrun{
itis("Plethodon ")
itis('202420', 'tsnhirdown', 'tsn')
itis("36616", "tsnfullhir", "tsn")
}
}


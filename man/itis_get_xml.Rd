\name{itis_get_xml}
\alias{itis_get_xml}
\title{Search individual strings.}
\usage{
  itis_get_xml(searchterm,
    searchtype = c("anymatch", "sciname", "comnamebeg", "comname", "comnameend", "terms", "itistermscomname", "itistermssciname", "tsnsvernacular", "tsnfullhir", "tsnhirdown"),
    by_ = c("name", "tsn"), parselist = TRUE,
    base_url = "http://www.itis.gov/ITISWebService/services/ITISService/",
    curl = getCurlHandle())
}
\arguments{
  \item{searchterm}{Any common or scientific name.}

  \item{searchtype}{One of 'sciname', 'anymatch',
  'comnamebeg', 'comname', 'comnameend', 'terms',
  'itistermscomname', 'itistermssciname', or
  'tsnsvernacular', 'tsnfullhir', 'tsnhirdown' .}

  \item{by_}{One of 'name' (any common or scientific name)
  or 'tsn' (taxonomic serial number).}

  \item{parselist}{Parse xml or not (defaults to TRUE).}

  \item{base_url}{The base URL for the ITIS API service,
  leave as is.}

  \item{curl}{Use to speed up calls when looping, e.g.}
}
\value{
  xml with taxonomic information.
}
\description{
  Search individual strings.
}
\examples{
\dontrun{
xml <- itis_get_xml("Plethodon ")
parse_itis(xml)
itis_get_xml('202420', 'tsnhirdown', 'tsn')
itis_get_xml("36616", "tsnfullhir", "tsn")
}
}


\name{get_itis_xml}
\alias{get_itis_xml}
\title{Search individual strings.}
\usage{
  get_itis_xml(searchterm,
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
}
\value{
  xml with taxonomic information.
}
\description{
  Search individual strings.
}
\examples{
\dontrun{
xml <- get_itis_xml("Plethodon ")
parse_itis(xml)
get_itis_xml('202420', 'tsnhirdown', 'tsn')
get_itis_xml("36616", "tsnfullhir", "tsn")
}
}


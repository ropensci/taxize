\name{downstream}
\alias{downstream}
\title{Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.}
\usage{
  downstream(searchtsn = NA, attachrank = TRUE,
    url = "http://www.itis.gov/ITISWebService/services/ITISService/getHierarchyDownFromTSN?tsn=",
    curl = getCurlHandle())
}
\arguments{
  \item{searchtsn}{Quoted TSN for a taxonomic group
  (character).}

  \item{attachrank}{Should rank be attached to the output?
  (default=TRUE).}

  \item{url}{The ITIS url for the function (should be left
  to default).}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\value{
  Names or TSNs of all downstream taxa in a data.frame.
}
\description{
  Retrieve all taxa names or TSNs downstream in hierarchy
  from given TSN.
}
\examples{
\dontrun{
downstream(searchtsn = '208527')
}
}


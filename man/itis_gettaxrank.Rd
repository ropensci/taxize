\name{itis_gettaxrank}
\alias{itis_gettaxrank}
\title{Retrieve taxonomic rank name from given TSN.}
\usage{
  itis_gettaxrank(searchtsn = NA,
    url = "http://www.itis.gov/ITISWebService/services/ITISService/getTaxonomicRankNameFromTSN?tsn=",
    curl = getCurlHandle())
}
\arguments{
  \item{searchtsn}{Quoted TSN for a taxonomic group
  (character).}

  \item{url}{The ITIS API url for the function (should be
  left to default).}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\value{
  Taxonomic rank name.
}
\description{
  Retrieve taxonomic rank name from given TSN.
}
\details{
  You can print informative messages by setting
  supmess=FALSE.
}
\examples{
\dontrun{
itis_gettaxrank('202385')
}
}


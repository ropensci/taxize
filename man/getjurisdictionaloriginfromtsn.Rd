\name{getjurisdictionaloriginfromtsn}
\alias{getjurisdictionaloriginfromtsn}
\title{Get jurisdictional origin from tsn}
\usage{
  getjurisdictionaloriginfromtsn(tsn = NA, ...,
    curl = getCurlHandle(), locally = FALSE)
}
\arguments{
  \item{tsn}{TSN for a taxonomic group (numeric)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}

  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}
}
\description{
  Get jurisdictional origin from tsn
}
\examples{
\dontrun{
getjurisdictionaloriginfromtsn(tsn = 180543)
}
}


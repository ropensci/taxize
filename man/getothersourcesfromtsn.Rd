\name{getothersourcesfromtsn}
\alias{getothersourcesfromtsn}
\title{Returns a list of the other sources used for the TSN.}
\usage{
  getothersourcesfromtsn(tsn = NA, ...,
    curl = getCurlHandle(), locally = FALSE,
    sqlconn = NULL)
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

  \item{sqlconn}{The sqlite3 connection object.}
}
\description{
  Returns a list of the other sources used for the TSN.
}
\examples{
\dontrun{
getothersourcesfromtsn(tsn = 182662)
}
}


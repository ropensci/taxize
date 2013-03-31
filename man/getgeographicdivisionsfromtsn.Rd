\name{getgeographicdivisionsfromtsn}
\alias{getgeographicdivisionsfromtsn}
\title{Get geographic divisions from tsn}
\usage{
  getgeographicdivisionsfromtsn(tsn = NA, ...,
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
  Get geographic divisions from tsn
}
\examples{
\dontrun{
getgeographicdivisionsfromtsn(tsn = 180543)
}
}


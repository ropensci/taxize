\name{getitistermsfromcommonname}
\alias{getitistermsfromcommonname}
\title{Get itis terms from common names}
\usage{
  getitistermsfromcommonname(srchkey = NA, ...,
    curl = getCurlHandle(), locally = FALSE)
}
\arguments{
  \item{srchkey}{text or taxonomic serial number (TSN)
  (character or numeric)}

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
  Get itis terms from common names
}
\examples{
\dontrun{
getitistermsfromcommonname("buya")
}
}


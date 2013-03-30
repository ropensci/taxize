\name{getcoveragefromtsn}
\alias{getcoveragefromtsn}
\title{Get coverge from tsn}
\usage{
  getcoveragefromtsn(tsn = NA, ..., curl = getCurlHandle(),
    locally = FALSE)
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
  Get coverge from tsn
}
\examples{
\dontrun{
getcoveragefromtsn(tsn = 28727)  # coverage data
getcoveragefromtsn(tsn = 526852)  # no coverage data
}
}


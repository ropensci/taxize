\name{getparenttsnfromtsn}
\alias{getparenttsnfromtsn}
\title{Returns the parent TSN for the entered TSN.}
\usage{
  getparenttsnfromtsn(tsn = NA, ...,
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
  Returns the parent TSN for the entered TSN.
}
\examples{
\dontrun{
getparenttsnfromtsn(tsn = 202385)
getparenttsnfromtsn(tsn = 202385, locally=TRUE)
getparenttsnfromtsn(tsn=c(202385,531894,526852,183671), locally=TRUE)
}
}


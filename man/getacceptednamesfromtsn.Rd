\name{getacceptednamesfromtsn}
\alias{getacceptednamesfromtsn}
\title{Get accepted names from tsn}
\usage{
  getacceptednamesfromtsn(tsn = NA, ...,
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
  Get accepted names from tsn
}
\examples{
\dontrun{
taxize:::getacceptednamesfromtsn(tsn=208527)  # TSN accepted - good name
taxize:::getacceptednamesfromtsn(208527, locally=TRUE)  # run it locally
getacceptednamesfromtsn(tsn=504239)  # TSN not accepted - input TSN is old name
getacceptednamesfromtsn('504239', FALSE)  # TSN not accepted - input TSN is old name
}
}


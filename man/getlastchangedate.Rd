\name{getlastchangedate}
\alias{getlastchangedate}
\title{Provides the date the ITIS database was last updated.}
\usage{
  getlastchangedate(locally = FALSE)
}
\arguments{
  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}

  \item{sqlconn}{The sqlite3 connection object.}
}
\description{
  Provides the date the ITIS database was last updated.
}
\examples{
\dontrun{
getlastchangedate()
}
}


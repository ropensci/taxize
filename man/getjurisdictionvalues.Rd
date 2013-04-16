\name{getjurisdictionvalues}
\alias{getjurisdictionvalues}
\title{Get possible jurisdiction values}
\usage{
  getjurisdictionvalues(locally = FALSE)
}
\arguments{
  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}

  \item{sqlconn}{The sqlite3 connection object.}
}
\description{
  Get possible jurisdiction values
}
\examples{
\dontrun{
getjurisdictionvalues()
}
}


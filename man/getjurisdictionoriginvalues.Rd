\name{getjurisdictionoriginvalues}
\alias{getjurisdictionoriginvalues}
\title{Get jurisdiction origin values}
\usage{
  getjurisdictionoriginvalues(locally = FALSE,
    sqlconn = NULL)
}
\arguments{
  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}

  \item{sqlconn}{The sqlite3 connection object.}
}
\description{
  Get jurisdiction origin values
}
\examples{
\dontrun{
getjurisdictionoriginvalues()
}
}


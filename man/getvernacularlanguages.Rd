\name{getvernacularlanguages}
\alias{getvernacularlanguages}
\title{Provides a list of the unique languages used in the vernacular table.}
\usage{
  getvernacularlanguages(locally = FALSE, sqlconn = NULL)
}
\arguments{
  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. lofcally=TRUE should be faster in
  almost all cases.}

  \item{sqlconn}{The sqlite3 connection object.}
}
\description{
  Provides a list of the unique languages used in the
  vernacular table.
}
\examples{
\dontrun{
getvernacularlanguages()
}
}


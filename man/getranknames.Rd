\name{getranknames}
\alias{getranknames}
\title{Provides a list of all the unique rank names contained in the database and
 their kingdom and rank ID values.}
\usage{
  getranknames(locally = FALSE, sqlconn = NULL)
}
\arguments{
  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}

  \item{sqlconn}{The sqlite3 connection object.}
}
\description{
  Provides a list of all the unique rank names contained in
  the database and their kingdom and rank ID values.
}
\examples{
\dontrun{
getranknames()
}
}


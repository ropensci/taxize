\name{getkingdomnames}
\alias{getkingdomnames}
\title{Get all possible kingdom names}
\usage{
  getkingdomnames(locally = FALSE)
}
\arguments{
  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}

  \item{sqlconn}{The sqlite3 connection object.}
}
\description{
  Get all possible kingdom names
}
\examples{
\dontrun{
getkingdomnamefromtsn(tsn = 202385)
}
}


\name{taxize_options}
\alias{taxize_options}
\title{Function to specify sqlite3 connection object inside ITIS* functions.}
\usage{
  taxize_options(localpath = NULL)
}
\description{
  Function to specify sqlite3 connection object inside
  ITIS* functions.
}
\examples{
# Set the path to your sqlite database file
taxize_options(localpath = "~/github/ropensci/sql/itis2.sqlite")
}
\keyword{internal}


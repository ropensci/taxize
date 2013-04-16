\name{sqlite_init}
\alias{sqlite_init}
\title{Initiate connection to sqlite3 database}
\usage{
  sqlite_init(path = "~/github/ropensci/sql/itis2.sqlite")
}
\arguments{
  \item{path}{Path to the sqlite database}
}
\description{
  Initiate connection to sqlite3 database
}
\examples{
\dontrun{
taxize:::sqlite_init()
}
}


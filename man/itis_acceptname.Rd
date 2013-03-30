\name{itis_acceptname}
\alias{itis_acceptname}
\title{Retrieve accepted TSN (with accepted name).}
\usage{
  itis_acceptname(searchtsn = NA, locally = FALSE)
}
\arguments{
  \item{searchtsn}{Quoted TSN for a taxonomic group
  (character).}

  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}
}
\value{
  Names or TSNs of all downstream taxa.
}
\description{
  Retrieve accepted TSN (with accepted name).
}
\details{
  You can print informative messages by setting
  supmess=FALSE.
}
\examples{
\dontrun{
itis_acceptname(searchtsn='208527')  # TSN accepted - good name
itis_acceptname(searchtsn='504239')  # TSN not accepted - input TSN is old name

itis_acceptname(searchtsn='208527', locally=TRUE)  # searching locally
}
}


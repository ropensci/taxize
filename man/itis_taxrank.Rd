\name{itis_taxrank}
\alias{itis_taxrank}
\title{Retrieve taxonomic rank name from given TSN.}
\usage{
  itis_taxrank(query = NULL, locally = FALSE)
}
\arguments{
  \item{query}{Quoted TSN for a taxonomic group (numeric),
  or scientific name (character).}

  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}
}
\value{
  Taxonomic rank name.
}
\description{
  Retrieve taxonomic rank name from given TSN.
}
\details{
  You can print informative messages by setting
  supmess=FALSE.
}
\examples{
\dontrun{
itis_taxrank(query=202385)
itis_taxrank(query=202385, locally=TRUE)
}
}


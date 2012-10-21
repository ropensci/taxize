\name{itis_taxrank}
\alias{itis_taxrank}
\title{Retrieve taxonomic rank name from given TSN.}
\usage{
  itis_taxrank(query = NULL)
}
\arguments{
  \item{query}{Quoted TSN for a taxonomic group (numeric),
  or scientific name (character).}
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
itis_taxrank(202385)
}
}


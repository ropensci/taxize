\name{itis_gettaxrank}
\alias{itis_gettaxrank}
\title{Retrieve taxonomic rank name from given TSN.}
\usage{
  itis_gettaxrank(query = NULL)
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
itis_gettaxrank(202385)
}
}


\name{printhier}
\alias{printhier}
\title{Print taxnomic hierarchy.}
\usage{
  printhier(hierout = NA)
}
\arguments{
  \item{hierout}{Output from a hierarchy search using
  get_itis_xml function.}
}
\value{
  Taxonomic hierarchy as data.fram.
}
\description{
  Print taxnomic hierarchy.
}
\examples{
\dontrun{
out <- get_itis_xml("36616", "tsnfullhir", "tsn", FALSE)
printhier(out)
}
}


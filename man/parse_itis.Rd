\name{parse_itis}
\alias{parse_itis}
\title{Return the scientific name, a common name, and tsn of all matches to an itis query..}
\usage{
  parse_itis(doc)
}
\arguments{
  \item{doc}{A query result from ITIS in xml format.}
}
\value{
  Taxonomic rank name.
}
\description{
  Return the scientific name, a common name, and tsn of all
  matches to an itis query..
}
\examples{
\dontrun{
tt <- get_itis_xml("Plethodon ")
parse_itis(tt)
}
}


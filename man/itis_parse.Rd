\name{itis_parse}
\alias{itis_parse}
\title{Return the scientific name, a common name, and tsn of all matches to an itis query.}
\usage{
  itis_parse(doc)
}
\arguments{
  \item{doc}{A query result from ITIS in xml format.}
}
\value{
  Taxonomic rank name.
}
\description{
  Return the scientific name, a common name, and tsn of all
  matches to an itis query.
}
\examples{
\dontrun{
tt <- get_itis_xml("Plethodon ")
itis_parse(tt)
}
}


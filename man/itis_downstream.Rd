\name{itis_downstream}
\alias{itis_downstream}
\title{Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.}
\usage{
  itis_downstream(tsns, downto, locally = FALSE)
}
\arguments{
  \item{tsns}{A taxonomic serial number.}

  \item{downto}{The taxonomic level you want to go down to.
  See examples below.  The taxonomic level IS case
  sensitive, and you do have to spell it correctly. See
  \code{data(rank_ref)} for spelling.}

  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}
}
\value{
  Data.frame of taxonomic information downstream to family
  from e.g., Order, Class, etc.
}
\description{
  Retrieve all taxa names or TSNs downstream in hierarchy
  from given TSN.
}
\examples{
\dontrun{
itis_downstream(tsns=846509, downto="Genus")
itis_downstream(tsns = 650497, "Family") # getting families downstream from Acridoidea
itis_downstream(tsns = 180541, "Species") # getting species downstream from Ursus
}
}
\author{
  Scott Chamberlain \email{myrmecocystus@gmail.com}
}


\name{downstream2}
\alias{downstream2}
\title{Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.}
\usage{
  downstream2(tsns, downto)
}
\arguments{
  \item{tsns}{A taxonomic serial number.}

  \item{downto}{The taxonomic level you want to go down to.
  See examples below.  The taxonomic level IS case
  sensitive, and you do have to spell it correctly. See
  \code{data(rank_ref)} for spelling.}
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
downstream2(846509, "Genus")
downstream2(tsns = 650497, "Family") # getting families downstream from Acridoidea
downstream2(tsns = 180541, "Species") # getting species downstream from Ursus
}
}
\author{
  Scott Chamberlain {myrmecocystus@gmail.com}
}


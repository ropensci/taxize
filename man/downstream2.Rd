\name{downstream2}
\alias{downstream2}
\title{Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.}
\usage{
  downstream2(tsns)
}
\arguments{
  \item{tsns}{A taxonomic serial number.}
}
\value{
  Data.frame of taxonomic information downstream to family
  from e.g., Order, Class, etc.
}
\description{
  THE FUNCTION NEEDS TO BE GENERALIZED TO ANY RANK DESIRED
  IN THE OUTPUT JUST WORKS FOR GOING DOWN TO FAMILY RIGHT
  NOW.
}
\examples{
\dontrun{
downstream2(tsns = 650497) # getting families downstream from Acridoidea
downstream2(tsns = 114989) # getting families downstream from Strepsiptera
downstream2(tsns = 99208) # getting families downstream from Class Insecta
}
}
\author{
  Scott Chamberlain {myrmecocystus@gmail.com}
}


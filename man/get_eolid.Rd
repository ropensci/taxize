\name{get_eolid}
\alias{get_eolid}
\title{Get the EOL ID from Encyclopedia of Life from taxonomic names.}
\usage{
  get_eolid(sciname, verbose = TRUE)
}
\arguments{
  \item{sciname}{character; scientific name.}

  \item{verbose}{logical; If TRUE the actual taxon queried
  is printed on the console.}
}
\value{
  A vector of unique identifiers (EOL). If a taxon is not
  found NA. If more than one ID is found the function asks
  for user input.
}
\description{
  Get the EOL ID from Encyclopedia of Life from taxonomic
  names.
}
\examples{
\dontrun{
get_eolid(sciname='Poa annua')
get_eolid(sciname='Pinus contorta')

get_eolid(c("Poa annua", "Pinus contorta"))

# When not found
get_eolid("howdy")
get_eolid(c("Chironomus riparius", "howdy"))
}
}
\author{
  Scott Chamberlain, \email{myrmecocystus@gmail.com}
}
\seealso{
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{get_uid}},
  \code{\link[taxize]{get_tpsid}}
}


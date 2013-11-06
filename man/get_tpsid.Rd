\name{get_tpsid}
\alias{get_tpsid}
\title{Get the NameID codes from Tropicos for taxonomic names.}
\usage{
  get_tpsid(sciname, verbose = TRUE)
}
\arguments{
  \item{sciname}{character; scientific name.}

  \item{verbose}{logical; If TRUE the actual taxon queried
  is printed on the console.}
}
\value{
  A vector of unique identifiers (UID). If a taxon is not
  found NA. If more than one UID is found the function asks
  for user input.
}
\description{
  Get the NameID codes from Tropicos for taxonomic names.
}
\examples{
\dontrun{
get_tpsid(sciname='Poa annua')
get_tpsid(sciname='Pinus contorta')

get_tpsid(c("Poa annua", "Pinus contorta"))

# When not found
get_tpsid("howdy")
get_tpsid(c("Chironomus riparius", "howdy"))
}
}
\author{
  Scott Chamberlain, \email{myrmecocystus@gmail.com}
}
\seealso{
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{get_uid}}
}


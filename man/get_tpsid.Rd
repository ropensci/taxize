\name{get_tpsid}
\alias{get_tpsid}
\title{Get the NameID codes from Tropicos for taxonomic names.}
\usage{
  get_tpsid(sciname, ask = TRUE, verbose = TRUE,
    key = NULL, ...)
}
\arguments{
  \item{sciname}{character; scientific name.}

  \item{ask}{logical; should get_tpsid be run in
  interactive mode? If TRUE and more than one ID is found
  for the species, the user is asked for input. If FALSE NA
  is returned for multiple matches.}

  \item{verbose}{logical; If TRUE the actual taxon queried
  is printed on the console.}

  \item{key}{Your API key; loads from .Rprofile.}

  \item{...}{Other arguments passed to
  \code{\link[taxize]{tp_search}}.}
}
\value{
  A vector of unique identifiers. If a taxon is not found
  NA. If more than one ID is found the function asks for
  user input.
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

# pass to tp_classification to get a taxonomic hierarchy
tp_classification(get_tpsid(sciname='Poa annua'))
}
}
\author{
  Scott Chamberlain, \email{myrmecocystus@gmail.com}
}
\seealso{
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{get_uid}}
}


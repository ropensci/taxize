\name{get_tsn}
\alias{get_tsn}
\title{Get the TSN code for a search term.}
\usage{
  get_tsn(searchterm, searchtype = "sciname",
    verbose = TRUE)
}
\arguments{
  \item{searchterm}{character; A vector of common or
  scientific names.}

  \item{searchtype}{character; One of 'sciname',
  'anymatch', 'comnamebeg', 'comname', 'comnameend'.}

  \item{verbose}{logical; should progress be printed?}
}
\value{
  A vector of taxonomic serial numbers (TSN). If a species
  is not found NA. If more than one TSN is found the
  function asks for user input.  See functions in the
  \code{itis} function.
}
\description{
  Retrieve the taxonomic serial numbers (TSN) of a species
  from ITIS.
}
\examples{
\dontrun{
get_tsn(searchterm = "Quercus douglasii", searchtype = "sciname")
get_tsn(searchterm = "Chironomus riparius", searchtype = "sciname")
get_tsn(c("Chironomus riparius","Quercus douglasii"), "sciname")
splist <- c("annona cherimola", 'annona muricata', "quercus robur",
		"shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus")
get_tsn(splist, "sciname")
}
}
\seealso{
  \code{\link[taxize]{get_uid}},
  \code{\link[taxize]{classification}}
}


\name{get_colid}
\alias{get_colid}
\title{Get the Catalogue of Life ID from taxonomic names.}
\usage{
  get_colid(sciname, ask = TRUE, verbose = TRUE)
}
\arguments{
  \item{sciname}{character; scientific name.}

  \item{ask}{logical; should get_colid be run in
  interactive mode? If TRUE and more than one ID is found
  for the species, the user is asked for input. If FALSE NA
  is returned for multiple matches.}

  \item{verbose}{logical; If TRUE the actual taxon queried
  is printed on the console.}
}
\value{
  A vector of unique identifiers. If a taxon is not found
  NA. If more than one ID is found the function asks for
  user input.
}
\description{
  Get the Catalogue of Life ID from taxonomic names.
}
\examples{
\dontrun{
get_colid(sciname='Poa annua')
get_colid(sciname='Pinus contorta')
get_colid(sciname='Puma concolor')

get_colid(c("Poa annua", "Pinus contorta"))

# When not found
get_colid(sciname="uaudnadndj")
get_colid(c("Chironomus riparius", "uaudnadndj"))
}
}
\author{
  Scott Chamberlain, \email{myrmecocystus@gmail.com}
}
\seealso{
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{get_uid}},
  \code{\link[taxize]{get_tpsid}},
  \code{\link[taxize]{get_eolid}}
}


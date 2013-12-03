\name{get_eolid}
\alias{get_eolid}
\title{Get the EOL ID from Encyclopedia of Life from taxonomic names.}
\usage{
  get_eolid(sciname, ask = TRUE, verbose = TRUE,
    key = NULL, ...)
}
\arguments{
  \item{sciname}{character; scientific name.}

  \item{ask}{logical; should get_eolid be run in
  interactive mode? If TRUE and more than one ID is found
  for the species, the user is asked for input. If FALSE NA
  is returned for multiple matches.}

  \item{verbose}{logical; If TRUE the actual taxon queried
  is printed on the console.}
}
\value{
  A vector of unique identifiers (EOL). If a taxon is not
  found NA. If more than one ID is found the function asks
  for user input.
}
\description{
  Note that EOL doesn't expose an API endpointn for
  directly querying for EOL taxon ID's, so we first use the
  function \code{\link[taxize]{eol_search}} to find pages
  that deal with the species of interest, then use
  \code{\link[taxize]{eol_pages}} to find the actual taxon
  IDs.
}
\examples{
\dontrun{
get_eolid(sciname='Poa annua')
get_eolid(sciname='Pinus contorta')
get_eolid(sciname='Puma concolor')

get_eolid(c("Poa annua", "Pinus contorta"))

# When not found
get_eolid(sciname="uaudnadndj")
get_eolid(c("Chironomus riparius", "uaudnadndj"))
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


\name{get_uid}
\alias{get_uid}
\title{Get the UID codes from NCBI for taxonomic names.}
\usage{
  get_uid(sciname, ask = TRUE, verbose = TRUE)
}
\arguments{
  \item{sciname}{character; scientific name.}

  \item{ask}{logical; should get_tsn be run in interactive
  mode? If TRUE and more than one TSN is found for the
  species, the user is asked for input. If FALSE NA is
  returned for multiple matches.}

  \item{verbose}{logical; If TRUE the actual taxon queried
  is printed on the console.}
}
\value{
  A vector of unique identifiers (UID). If a taxon is not
  found NA. If more than one UID is found the function asks
  for user input (if ask = TRUE), otherwise returns NA.
  Comes with an attribute \emph{match} to investigate the
  reason for NA (either 'not found', 'found' or if ask =
  FALSE 'multi match')
}
\description{
  Retrieve the Unique Identifier (UID) of a taxon from NCBI
  taxonomy browser.
}
\examples{
\dontrun{
get_uid(c("Chironomus riparius", "Chaetopteryx"))
get_uid(c("Chironomus riparius", "aaa vva"))

# When not found
get_uid("howdy")
get_uid(c("Chironomus riparius", "howdy"))

# multiple matches
get_uid('Dugesia')  # user prompt needed
get_uid('Dugesia', ask = FALSE) # returns NA for multiple matches

}
}
\author{
  Eduard Szoecs, \email{szoe8822@uni-landau.de}
}
\seealso{
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{classification}}
}


\name{get_uid}
\alias{get_uid}
\title{Get the UID codes from NCBI for species names.}
\usage{
  get_uid(sciname, verbose = TRUE)
}
\arguments{
  \item{sciname}{character; scientific name.}

  \item{verbose}{logical; If TRUE the actual taxon queried
  is printed on the console.}
}
\value{
  A vector of unique identifiers (UID). If a species is not
  found NA. If more than one UID is found the function asks
  for user input.
}
\description{
  Retrieve the Unique Identifier (UID) of a species from
  NCBI taxonomy browser.
}
\examples{
\dontrun{
get_uid(c("Chironomus riparius", "Chaetopteryx"))
get_uid(c("Chironomus riparius", "aaa vva"))
}
}
\author{
  Eduard Szoecs, \email{szoe8822@uni-landau.de}
}
\seealso{
  \code{\link[taxize]{get_tsn}}
}


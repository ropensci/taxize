\name{get_uid}
\alias{get_uid}
\title{Get the UID codes from NCBI for taxonomic names.}
\usage{
  get_uid(sciname, verbose = TRUE)
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
}
}
\author{
  Eduard Szoecs, \email{szoe8822@uni-landau.de}
}
\seealso{
  \code{\link[taxize]{get_tsn}},
  \code{\link[taxize]{classification}}
}


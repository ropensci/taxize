\name{get_uid}
\alias{get_uid}
\title{Get the UID codes from NCBI for species names.}
\usage{
  get_uid(sciname)
}
\arguments{
  \item{sciname}{scientific name.}
}
\value{
  UID for the supplied species names. NA for non-matching
  names.
}
\description{
  A function to retrieve the UID-Code (Unique Identifier)
  of a species from NCBI taxonomy browser.
}
\examples{
\dontrun{
get_uid(c("Chironomus riparius", "Chaetopteryx"))
get_uid(c("Chironomus riparius", "aaa vva"))
}
}
\author{
  Eduard Sz??cs \email{szoe8822@uni-landau.de}
}


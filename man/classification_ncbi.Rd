\name{classification_ncbi}
\alias{classification_ncbi}
\title{Get taxonomic hierarchy for a given taxon from NCBI taxonomy browser.}
\usage{
  classification_ncbi(ids)
}
\arguments{
  \item{ids}{Unique identifiers (UID) for the species.
  \code{get_uid} can be used to retrieve UIDs.}
}
\value{
  A list of data.frames with the classification of the
  taxon.
}
\description{
  Get taxonomic hierarchy for a given taxon from NCBI
  taxonomy browser.
}
\examples{
\dontrun{
classification_ncbi(get_uid("Chironomus riparius"))
classification_ncbi(get_uid(c("Chironomus riparius", "Chaetopteryx")))
classification_ncbi(get_uid(c("Chironomus riparius", "aaa vva")))
}
}
\author{
  Eduard Szöcs \email{szoe8822@uni-landau.de}
}


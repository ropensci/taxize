\name{itis_kingdomnames}
\alias{itis_kingdomnames}
\title{Get kingdom names.}
\usage{
itis_kingdomnames(tsn = NULL, ...)
}
\arguments{
  \item{tsn}{One or more TSN's (taxonomic serial number)}

  \item{...}{Further arguments passed on to
  getkingdomnamefromtsn}
}
\description{
Get kingdom names.
}
\examples{
\dontrun{
itis_kingdomnames(202385)
itis_kingdomnames(tsn=c(202385,183833,180543))

# suppress message
itis_kingdomnames(c(202385,183833,180543), verbose=FALSE)
}
}

